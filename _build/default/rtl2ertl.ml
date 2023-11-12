open Ertl;;


(*Translation from RTL  (Register Transfer Language) to ERTL (Explicit Register Transfer Language)*)

let (graph : Ertl.cfg ref) = ref (Hashtbl.create 16);;





let cpt_label = ref (-1) 
let new_label () =

  fun () ->
    incr cpt_label;
    (* Printf.printf "L%d\n" !cpt_label;  *) 
    !cpt_label


let generate_reg = Mips.fresh  ;;
let generate_label = new_label ();;



let rec split lst n = 
  if n < 0 then failwith "split : n < 0"
  else if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (l1, l2) = split t (n - 1) in (h :: l1, l2)
;;

let add_in_graph (i : Ertl.instruction) = 
  let l = generate_label () in
  Hashtbl.add !graph l i;
  l;;

(* pseudo register  to real register *)


(* 
let pseudo_to_real (pseudo_reg : Rtl.pseudo_register) : register =
  match pseudo_reg with
  | PReg r -> r
  | PRegInt i -> failwith "pseudo_to_real : PRegInt"

let pseudo_to_real_list (pseudo_reg_list : Rtl.pseudo_register list) : register list =
  List.map pseudo_to_real pseudo_reg_list *)



(*Move hard registers to pseudo registers*)
let args_to_regs hw_reg_list pseudo_reg_list  (l : int ) : int =
  let rec aux hws pseudos = 
      match hws, pseudos with
      | [],_ | _, [] -> l
      | h :: t, p :: q -> 
        let l1 = aux t q in
        add_in_graph (Ertl.EMove (h, p, l1))
  in
  aux hw_reg_list pseudo_reg_list
;;
let regs_to_args pseudo_reg_list hw_reg_list (l : int) : int =
  let rec aux pseudos hws = 
      match pseudos, hws with
      | [],_ | _, [] -> l
      | h :: t, p :: q -> 
        let l1 = aux t q in
        add_in_graph (Ertl.EMove (p, h, l1))
  in
  aux pseudo_reg_list hw_reg_list
;;

let push_args args l = 
  let rec aux args = 
    match args with
    | [] -> l
    | h :: t -> 
      add_in_graph (Ertl.EPush_param (h, aux t))
    in
    aux args
  ;;

let pop_args args l = 
  let offset = ref (4 *2) in 
  let rec aux args = 
    match args with
    | [] -> l
    | h :: t -> 
      let curr_offset = !offset in
      offset := !offset + 4;
      let l1 = aux t in
      add_in_graph (Ertl.EStack_offset (h, curr_offset, l1))
    in
    aux args
  ;;


let rec tr_instruction (rtl_instr:Rtl.instruction) : Ertl.instruction = 
  match rtl_instr with
  | RCst (n, r, l) -> ECst(n, r, l)
  | RBool (b, r, l) -> EBool(b,  r, l)
  | RMove (r1, r2, l) -> EMove( r1,  r2, l)
  | RUnop (u, r, l) -> EUnop(u,  r, l)
  | RBinop (b, r1, r2, l) -> EBinop(b,  r1,  r2, l)
  | RCall (r, s, rl, l) -> tr_call r s rl l
  | RPutchar (r, l) -> EPutchar( r, l)
  | RJmp l -> EJmp l
  | RJccb (b, r1, r2, l1, l2) -> EJccb(b, r1, r2, l1, l2)
  | RJccu (b, r, l1, l2) -> EJccu(b, r, l1, l2)
  | RReturn -> EReturn
  | RAlloc (r,l) -> EAlloc_frame l
  | _ -> failwith "tr_instruction : not implemented"

and tr_call ret_reg id args l   =
  let nb_params = List.length Mips.parameters in
  let  (args_for_regs , args_for_stack) = split args nb_params in
  let nb_stacked_args = List.length args_for_stack in
  let pop = 
    if nb_stacked_args > 0 then 
      add_in_graph (Ertl.EUnop(OAddi (nb_stacked_args * 4), Mips.sp, l))
    else l
  in 

  let res = add_in_graph (Ertl.EMove( Mips.result,ret_reg, pop)) in
  let call = add_in_graph (Ertl.ECall(id,nb_stacked_args, res)) in
  let start_push = push_args args_for_stack call in
  let start_move = args_to_regs Mips.parameters args_for_regs start_push in
  Ertl.ENop start_move
;;



(* let print_cfg fmt cfg =


let print_label fmt l =
  Format.fprintf fmt "L%d" l
in

let print_instruction fmt (l, i) =
  Format.fprintf fmt "%a: %a\n" print_label l print_instruction i
in
Format.fprintf fmt "{\n@[<v 0>%a@]}"
  (Utils.print_list print_instruction) (Hashtbl.to_seq cfg |> List.of_seq) *)





let translate_cfg (rtl_cfg:Rtl.cfg) =
  Hashtbl.iter (fun l i ->(*  print_int l; print_string " : "; *) Hashtbl.add !graph l (tr_instruction i))  rtl_cfg;
;;


let save_callee regs l = 
    let rec aux regs =
      match regs with
      | [] ->([], l)
      | h :: t -> 
        let r = generate_reg () in 
        let rem_env, next = aux t in
        let l1 = add_in_graph (Ertl.EMove (h, r, next)) in
        
        ((h, r) :: rem_env), l1
        
    in
    aux regs
  ;;
    
  let restore_callee save l = 
    let rec aux save = 
      match save with
      | [] -> l
      | (phys, save_in) :: t -> 
        let l1 = aux t in 
        add_in_graph (Ertl.EMove (save_in, phys, l1))
    in 
    aux save
  ;;

  (* Retrieve parameters from registers or stack *)
  let retrieve_params params l = 
    let (args_from_regs, args_from_stack) = split params (List.length Mips.parameters) in
    let tmp = pop_args  args_from_regs l in
    regs_to_args args_from_regs Mips.parameters tmp 
  ;;

let tr_function (fdef:Rtl.function_def) : Ertl.function_def =

  translate_cfg fdef.code;

  let params = fdef.params in 
  let entry = fdef.entry in

  let get_params = retrieve_params params entry in
  let save, reg_save = 
  if fdef.name = "main" then 
    [], (add_in_graph (Ertl.ENop get_params))
  else 
    save_callee Mips.callee_saved get_params in

  let alloc = add_in_graph (Ertl.EAlloc_frame (reg_save)) in
  let l_ret = add_in_graph (Ertl.EReturn) in
  let dealloc = add_in_graph (Ertl.EDelete_frame l_ret) in
  let restore = restore_callee save dealloc in
  let exit = Ertl.EMove (fdef.result, Mips.result, restore) in 
  Hashtbl.add !graph fdef.exit exit;
  
  {
    name = fdef.name;
    entry = alloc;
    nb_params = List.length fdef.params;
    locals = fdef.locals;
    code = !graph;
  }
;;



let tr_program (rtl_prog:Rtl.program) : Ertl.program =
  {
    globals = rtl_prog.globals;
    functions = List.map tr_function rtl_prog.functions;
  }
  