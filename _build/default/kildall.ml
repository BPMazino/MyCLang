(* Registers allocation based on Kildall's algorithm *)

type register = Mips.register;;

type live_info = {
  defs : register list ; (* definition *)
  use : register list; (* use *)
  mutable live_in: register list;  (* living variables at the beginning of the instruction *)
  mutable live_out: register list; (* living variables at the end of the instruction *)
  succ : Ertl.label list; (* successors *)
  mutable pred : Ertl.label list; (* predecessors *)
  instr : Ertl.instruction; (* instruction *)
  
}

let succ (instr : Ertl.instruction) = 
  match instr with 
  | ECst (_,_,l) -> [l] 
  | EBool  (_,_,l) -> [l]
  | EMove (_,_,l) -> [l]
  | EUnop (_,_,l) -> [l]
  | EBinop (_,_,_,l) -> [l]
  | ECall  (_,_,l) -> [l]
  | ECmp (_,_,l) -> [l]
  | ECmpi (_,_,l) -> [l]
  | EPutchar (_,l) -> [l]
  | EJmp (l) -> [l]
  | EJccb (_,_,_,_,l) -> [l]
  | EJccu (_,_,_,l) -> [l]
  | ESetccu (_,_,l) -> [l]
  | ESetccb (_,_,_,l) -> [l]
  | EReturn  -> []
  | EAlloc_frame (l) -> [l]
  | EDelete_frame (l) -> [l]
  | EPush_param (_,l) -> [l]
  | EGet_param (_,l) -> [l]
  | EStack_offset (_,_,l) -> [l]
  | ENop (l) -> [l]



let rec prefix i = function
  | _ when i = 0 -> []
  | [] -> assert false
  | x :: r -> x :: prefix (i-1) r

(* Return the list of registers defined and used by an instruction *)
let def_use (instr : Ertl.instruction) : register list * register list =
  match instr with
  | ECst (_,r,_) -> [r], []
  | EBool (_,r,_) -> [r], []
  | EMove (r1,r2,_) -> [r2], [r1]
  | EUnop (_,r,_) -> [r], [r]
  | EBinop (_,r1,r2,_) -> [r2], [r1;r2]
  | ECall (_,n,_) -> Mips.caller_saved, prefix n Mips.parameters
  | ECmp (r1,r2,_) -> [], [r1;r2]
  | ECmpi (_,r,_) -> [], [r]
  | EPutchar (r,_) -> [] , [Mips.result; Mips.a0]
  | EJmp _ -> [], []
  | EJccb (_,_,_,_,_) -> [], []
  | EJccu (_,_,_,_) -> [], []
  | ESetccu (_,r,_) -> [r], []
  | ESetccb (_,r1,r2,_) -> [r1;r2], []
  | EReturn -> [], Mips.result :: Mips.callee_saved
  | EAlloc_frame _ -> [], []
  | EDelete_frame _ -> [], []
  | EPush_param (r,_) -> [], [r]
  | EGet_param (r,_) -> [r], []
  | EStack_offset (r,_,_) -> [r], []
  | ENop _ -> [], []




let get_info_from_ertl instr =
  let succ = succ instr in
  let pred = [] in
  let def_list, use_list = def_use instr in
  let defs, uses =  def_list, use_list in
  let ins, outs = [], [] in
  {
    defs = defs;
    use = uses;
    live_in = ins;
    live_out = outs;
    succ = succ;
    pred = pred;
    instr = instr;
  }




let kidall table = 
  let worklist = Queue.create () in
  let rec aux () = 
    if Queue.is_empty worklist then ()
    else 
      let label = Queue.pop worklist in
      let info = Hashtbl.find table label in
      let succs = info.succ in
      let ins = info.live_in in
      let outs = info.live_out in
      let new_ins = info.defs @ (List.filter (fun x -> not (List.mem x info.defs)) info.live_out) in
      let new_outs = List.fold_left (fun acc x -> acc @ (Hashtbl.find table x).live_in) [] succs in
      if ins <> new_ins || outs <> new_outs then 
        begin
          info.live_in <- new_ins;
          info.live_out <- new_outs;
          List.iter (fun x -> Queue.push x worklist) info.pred;
        end;
      aux ()
  in
  Hashtbl.iter (fun x y -> Queue.push x worklist) table;
  aux ()

type live_info_table = (Ertl.label, live_info) Hashtbl.t;;


(*Take a control flow and return a table of live_info*)

(*Cfg is a (label,instr) Hashtbl*)
let build_table (cfg : Ertl.cfg) =
  let table = Hashtbl.create 16 in
  Hashtbl.iter (fun label instr -> Hashtbl.add table label (get_info_from_ertl instr)) cfg;
  Hashtbl.iter (fun label info -> info.pred <- (List.filter (fun x -> x <> label) (List.fold_left (fun acc x -> acc @ [label]) [] info.succ))) table;
  kidall table;
  table
  
