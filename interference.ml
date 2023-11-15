(*Interference graph*)

type edge = 
| Pref of Mips.register * Mips.register
| Intf of Mips.register * Mips.register

type edges = {
  prefs :Mips.register list;
  intfs :  Mips.register list
}

type graph = (Mips.register,edges) Hashtbl.t

type color = Reg of Mips.register | Spill of int

type coloring = (Mips.register,color) Hashtbl.t

let generate_edges () = { prefs = []; intfs = [] }

let is_empty g = Hashtbl.length g = 0
let remove_element el lst = 
  List.filter (fun x -> x <> el) lst

let update_intfs g (v1:Mips.register) (v2:Mips.register) =
  let {prefs; intfs} =  if Hashtbl.mem g v1 then Hashtbl.find g v1 else generate_edges () in
  let prefs = List.filter (fun x -> x <> v2) prefs in 
  let intfs = v2 :: intfs in
  {prefs; intfs}



let update_prefs g v1 v2 =
  let {prefs; intfs} =  if Hashtbl.mem g v1 then Hashtbl.find g v1 else generate_edges () in
  if List.mem v2 intfs then 
    {prefs; intfs}
  else(
    let prefs = v2 :: prefs in
    {prefs; intfs}
  ) 


let add_edge g e = 
  match e with
  | Pref(v1, v2) -> Hashtbl.replace g v1 (update_prefs g v1 v2); Hashtbl.replace g v2 (update_prefs g v2 v1)
  | Intf(v1, v2) -> Hashtbl.replace g v1 (update_intfs g v1 v2); Hashtbl.replace g v2 (update_intfs g v2 v1)

let add_intfs defs outs g = 
  let aux v g2 = 
    let outs2 = List.filter (fun x -> x <> v) outs in 
    List.iter (fun x -> add_edge g2 (Intf(v,x))) outs2
  in
  List.iter (fun x -> aux x g) defs

let increment g {Kildall.instr;defs; live_out} =
  match instr with 
  | Ertl.EMove (v1,_,_) -> 
    List.iter (fun x -> add_edge g (Pref(x,v1))) live_out;
    add_intfs defs (List.filter (fun x -> x <> v1) live_out) g
  |_ -> add_intfs defs live_out g 


let make live_info_map = 
  Hashtbl.fold (fun _ l_info g -> increment g l_info; g ) (Hashtbl.create 16) live_info_map 
;;


let degree g v = 
  let {prefs; intfs} = Hashtbl.find g v in
  List.length prefs + List.length intfs

(*Equivalent to Map choose but for Hashtable
  Return one binding of the given hashtable, or raise Not_found if the hashtable is empty. 
    Which binding is chosen is unspecified, 
    but equal bindings will be chosen for equal hashtable.   

*)

let first_element htbl =
  match Hashtbl.fold (fun key value acc -> Some (key, value)) htbl None with
  | Some element -> element
  | None -> failwith "Hashtable is empty"



let minimal g = 
  let v,_ = first_element g in
  Hashtbl.fold (fun w _ v -> if degree g v < degree g w then v else w) g v 

let neighbors g (v:Mips.register) = 
  let {prefs; intfs} = Hashtbl.find g v in
  prefs @ intfs



let george_criteria (k:int) g (v1:Mips.register) (v2:Mips.register) = 
  let verify condition =
    let v1_neighbors = neighbors g v1  in
    let aux w acc = acc && (not (condition w) || List.mem w v1_neighbors) in
    List.fold_right aux (neighbors g v2) true in 
  if Mips.is_hard v1 then 
    (not (Mips.is_hard v2)) && (verify (fun w -> not (Mips.is_hard w) || degree g w >= k))
  else 
    verify (fun w -> Mips.is_hard w  || degree g w >= k)


(*Find a pref edge based on george criteria*)
let find_pref k g = 
  let aux v1 {prefs;_} = function 
  | Some _ as res -> res
  | None -> 
    let possibles = List.filter (fun v2 -> george_criteria k g v1 v2) prefs in
    match possibles with
    | [] -> None
    | _ :: _ -> Some (v1, List.hd possibles)
  in
  Hashtbl.fold aux g None

let remove_edge v neighbors g = 
  let del e = 
    let prefs_without = List.filter (fun x -> x <> v) e.prefs in
    let intfs_without = List.filter (fun x -> x <> v) e.intfs in
    {prefs = prefs_without; intfs = intfs_without}
  in
  List.iter (fun w -> Hashtbl.replace g w (del (Hashtbl.find g w))) neighbors

let remove_vertex v g=
  let neighbors = neighbors g v in
  remove_edge v neighbors g;
  Hashtbl.remove g v;
  remove_edge v neighbors g


let pref_without_pref_edge_minimal g k  = 
   let result = Hashtbl.create (Hashtbl.length g) in 
  Hashtbl.iter (fun v { prefs; _ } ->
      if not (Mips.is_hard v) &&  (prefs = []) && degree g v < k then
        Hashtbl.add result v (Hashtbl.find g v)) g;
  result



let fusion g v1 v2 =
  let {prefs = prefs1; intfs = intfs1 }, { prefs = prefs2; intfs = intfs2 } =
    Hashtbl.find g v1, Hashtbl.find g v2 in
  let prefs  = remove_element v2 (prefs1 @ prefs2) in
  let intfs = remove_element v2 (intfs1 @ intfs2) in
  List.fold_right (fun w g' -> add_edge  g' (Pref (v2, w)); g') prefs g
  |> List.fold_right (fun w g' -> add_edge g' (Intf(v2, w)); g') intfs
  |> remove_vertex v1


let get_pseudos_regs g =
  let g' = Hashtbl.create (Hashtbl.length g) in
  Hashtbl.iter (fun v { prefs; _ } ->
      if not (Mips.is_hard v) then
        Hashtbl.add g' v (Hashtbl.find g v)) g;
      g'
let exist_minimal k g = 
  let g' = get_pseudos_regs g in
  not (is_empty g') && degree g' (minimal g') < k




let available_regs v c g =
  let used = 
    let aux w acc = 
      match Hashtbl.find_opt c w with
      | Some (Reg r) -> r :: acc
      | Some Spill _ -> acc
      | None  -> acc
    in
    List.fold_right aux (neighbors g v) [] in
  List.filter (fun r -> not (List.mem r used)) Mips.allocatable
let  colorize ~n_spilled: n ~n_allocatable: k graph  : coloring = 
  
  let rec  simplify  g  : coloring =
    let g' = pref_without_pref_edge_minimal g k  in
    if not (is_empty g' ) then select g (minimal g') else coalesce g


  and coalesce g = 
    match find_pref k g with
    | Some (v1,v2) ->  
      let v1, v2 = if Mips.is_hard v1 then v2, v1 else v1, v2 in 
      fusion g v1 v2;
      let c = simplify g in
      Hashtbl.replace c v1 (Hashtbl.find c v2) ;
      c
    | None -> freeze  g
  and freeze g = 
    if exist_minimal k g then (
      remove_vertex (minimal g) g ;
      simplify g)
    else
      spill g

  and spill g = 
    let g' = get_pseudos_regs g in
    if is_empty g' then 
     Hashtbl.fold (fun v _ c -> Hashtbl.replace c v (Reg v); c) g (Hashtbl.create (Hashtbl.length g))

    else 
    select g (first_element g' |> fst)


  and select g v =
    remove_vertex v g;
      let c = simplify g in
      let regs = if Mips.is_hard v then [v] else available_regs v c g in 
 
      if regs = [] then begin
        incr n;
        Hashtbl.replace c v (Spill !n);
        Hashtbl.replace c v (Spill(-4 * !n)); 
        c
      end 
      else (
        Hashtbl.replace c v (Reg (List.hd regs));
        c
      )
    
    in 
 simplify  graph 


let color graph = 
  let n = ref 0 in 
  let c = colorize ~n_spilled:n ~n_allocatable: Mips.k graph in
  (c, !n)


  
