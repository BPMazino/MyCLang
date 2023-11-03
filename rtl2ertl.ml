open Ertl;;

(*Translation from RTL  (Register Transfer Language) to ERTL (Explicit Register Transfer Language)*)

let (graph : Ertl.cfg ref) = ref (Hashtbl.create 16);;

let env = ref (Hashtbl.create 16);;



let cpt_label = ref (-1) 
let new_label () =

  fun () ->
    incr cpt_label;
    Printf.printf "L%d\n" !cpt_label;  
    !cpt_label


let new_label () =
  let i = ref (-1) in
  fun () ->
    incr i;
    Printf.printf "L%d\n" !i;  
    !i 

let generate_reg = Mips.fresh  ;;
let generate_label = new_label ();;



let split m l =
  let rec aux (acc, i) elt = if i >= m then (acc, i) else ((elt :: acc), (i + 1)) in
  let res, _ = List.fold_left aux ([], 0) l in 
  List.rev res 
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


let move_pseudo_to_hw (pseudo_reg : Rtl.register) (hw_reg : Mips.register) (l : int ) : int =
  add_in_graph (Ertl.ERMove (hw_reg, pseudo_reg, l) )

let move_pseudo_to_hw_list (pseudo_reg_list : Rtl.register list) (hw_reg_list : Mips.register list) (l : int ) : int =
  let aux hw_reg pseudo_reg l = move_pseudo_to_hw pseudo_reg hw_reg l in
  List.fold_right2 aux hw_reg_list pseudo_reg_list l


let move_hw_to_pseudo (hw_reg : Mips.register) (pseudo_reg : Rtl.register) (l : int ) : int =
  add_in_graph (Ertl.ERMove (pseudo_reg, hw_reg, l) )

let move_hw_to_pseudo_list (hw_reg_list : Mips.register list) (pseudo_reg_list : Rtl.register list) (l : int ) : int = 
  let aux hw_reg pseudo_reg l = move_hw_to_pseudo hw_reg pseudo_reg l in
  List.fold_right2 aux hw_reg_list pseudo_reg_list l


let rec tr_instruction (rtl_instr:Rtl.instruction) : Ertl.instruction = 
  match rtl_instr with
  | RCst (n, r, l) -> ERCst(n, r, l)
  | RBool (b, r, l) -> ERBool(b,  r, l)
  | RMove (r1, r2, l) -> ERMove( r1,  r2, l)
  | RBinop (b, r1, r2, l) -> ERBinop(b,  r1,  r2, l)
  | RCall (r, s, rl, l) -> tr_call r s rl l
  | RSet (r1, r2, l) -> ERSet( r1,  r2, l)
  | RIf (r, l1, l2) -> ERIf( r, l1, l2)
  | RPutchar (r, l) -> ERPutchar( r, l)
  | RWhile (r, l1, l2) -> ERWhile( r, l1, l2)
  | RGoto l -> ERGoto l
  | Jle (r1, r2, l1, l2) -> EJle( r1,  r2, l1, l2)
  | Jz (r, l1, l2) -> EJz( r, l1, l2)

and tr_call r id r_list l  : Ertl.instruction =
  let n = List.length r_list in
  let num_registers = List.length Mips.parameters in
  let unstack_l =
    if n <= num_registers then l else
      let offset = 8 * (n - num_registers) in
      (* Generate a constant representing the offset *)
      let offset_l = add_in_graph (ERCst (offset, Mips.tmp1, l)) in
      (* Adjust the stack pointer *)
      add_in_graph (ERBinop (Imp.Add,  Mips.sp,  Mips.tmp1, offset_l)) in
  let copyres_l = add_in_graph (ERMove ( Mips.result,  r, unstack_l)) in
  let funcall_l = add_in_graph (ERCall (id, min 6 n, copyres_l)) in
  let argpush_l =
    let r_list' = split (n - 6) (List.rev r_list) in
    List.fold_left (fun l r -> add_in_graph (EPush_param (r, l))) funcall_l ( r_list') in
  argpass r_list argpush_l

and argpass r_list l =
  let num_registers = List.length Mips.parameters in
  let m = min num_registers (List.length r_list) in
  let aux reg par l' = add_in_graph (ERMove(reg, par, l')) in
  ERGoto (List.fold_right2 aux (split m ( r_list)) (split m Mips.parameters) l)



let translate_cfg (rtl_cfg:Rtl.cfg) =
  Hashtbl.iter (fun k i -> Hashtbl.add !graph k (tr_instruction i)) rtl_cfg
;;



let tr_entry (fdef:Rtl.function_def) locals =
  
  let params = fdef.params in 
  let n = List.length params in 
  let entry = fdef.entry in
  (* Handling arguments passed on the stack *)
  let argpop_label =
    let r_list' = split (n - 4) (List.rev params) in
    let aux (l', ofs) r = (add_in_graph (EGet_param (ofs, r, l')), ofs + 8) in
    let l, _ = List.fold_left aux (entry, 16) ( r_list') in
    l in

  (* Handling arguments passed in registers *)
  let argrec_label =
    let m = min 4 n in
    let aux reg par l' = add_in_graph (ERMove (par, reg, l')) in
    List.fold_right2 aux (split m ( params)) (split m Mips.parameters) argpop_label in

  (* Saving callee-saved registers *)
  let callee_regs, calleesav_label =
    let aux (r_list', l') r =
      let r' = generate_reg () in
      Hashtbl.add !env r r';
      (r' :: r_list', add_in_graph (ERMove (r,   r', l'))) in
    List.fold_left aux ([], argrec_label) Mips.callee_saved in

  add_in_graph (EAlloc_frame calleesav_label), List.rev callee_regs

let tr_exit (fdef:Rtl.function_def) r_list  =
  let ret_l = add_in_graph (Ertl.EReturn) in
  let del_l = add_in_graph (Ertl.EDelete_frame ret_l) in
  let aux l r c  = add_in_graph (Ertl.ERMove (r, c, l)) in
  let call_e = List.fold_left2 aux del_l r_list Mips.callee_saved in
  Ertl.ERMove (fdef.result, Mips.result, call_e)

let tr_function (fdef:Rtl.function_def) : Ertl.function_def =
  let name = fdef.name in
  let params = List.length fdef.params in
  let locals = fdef.locals in
  let entry, callee_regs = tr_entry fdef locals in
  translate_cfg fdef.body;
  let exit = tr_exit fdef callee_regs in
  Hashtbl.add !graph fdef.exit exit ;
  let code  = !graph in
  { name = name; nb_params = params; locals = locals; entry = entry; body = code}


let tr_program (rtl_prog:Rtl.program) : Ertl.program =
  {
    globals = rtl_prog.globals;
    functions = List.map tr_function rtl_prog.functions;
  }
  