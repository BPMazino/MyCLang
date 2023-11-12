open Rtl;;
open Llast;;

(* Llast to RTL *)

let env = Hashtbl.create 16;;


let (graph : Rtl.cfg ref) = ref (Hashtbl.create 16);;


let cpt_label = ref (-1) 
let new_label () =

  fun () ->
    incr cpt_label;
    (* Printf.printf "L%d\n" !cpt_label;   *)
    !cpt_label

let generate_reg =  Mips.fresh ;;
let generate_label = new_label ();;


let add_in_graph (i : Rtl.instruction) = 
  let l = generate_label () in
  Hashtbl.replace !graph l i;
  l;;
let add_in_graph_with_label i l =
  Hashtbl.replace !graph l i
;;
  
let get_var x = 
  try Hashtbl.find env x with
  | Not_found -> failwith ("Variable " ^ x ^ " not found");;
     

(* Translation of an expression*)

(*e : expression to translate  r : pseudo register to store the result dest : destination of the result*)
let rec tr_expression e rd ld =
  match e with 
  | OCst n -> add_in_graph (RCst (n,rd,ld))
  | OBool b -> 
    if b then add_in_graph (RCst (1,rd,ld))
    else add_in_graph (RCst (0,rd,ld))
  | OVar x ->  
    let r = get_var x in
    add_in_graph (RMove (r,rd,ld))
  | OBinop(op,e1,e2)  ->  tr_binop op e1 e2 rd ld
  | OCall (f,args) -> tr_call f args rd ld
  | OUnop (op,e) -> tr_unop op e rd ld 
  | ODCall (e,el) ->


    let fun_addr_reg = generate_reg () in
    let _ = tr_expression e fun_addr_reg ld in
    
    let arg_regs = List.map (fun expr -> 
        let arg_reg = generate_reg () in
        let _ = tr_expression expr arg_reg ld in
        arg_reg) el in
    
    let return_label = generate_label () in

    let f_name = "f_name" in


    let call_instr = RCall (fun_addr_reg, f_name, arg_regs, return_label) in
    
    add_in_graph call_instr
    

  | ODeref (e) -> ld 
  | OAlloc   (e) -> let r = generate_reg () in
    let l = add_in_graph (RAlloc (r,ld)) in
      tr_expression e r l

  | OAddr (s) -> ld
 

    
  and tr_binop op e1 e2 rd ld =
    let r = generate_reg () in
    let l3 = add_in_graph (RBinop (op, r, rd, ld)) in
    let l2 = tr_expression e2 r l3 in
    let l1 = tr_expression e1 rd l2 in
    l1
  and tr_unop op e rd ld =
    let r = generate_reg () in
    let l2 = add_in_graph (RUnop (op, r, ld)) in
    let l1 = tr_expression e rd l2 in
    l1

  and tr_call f args rd ld =
    let r_args = List.rev_map (fun _ -> generate_reg ()) args in
    let l = add_in_graph (RCall (rd,f,r_args,ld)) in
    List.fold_right2 tr_expression args r_args l 

  
let general_case e lt lf =
  let r = generate_reg () in
  let l2 = add_in_graph (RJccu (Lt,r,lf,lt)) in
  let l = tr_expression e r l2 in
  l

let tr_condition e lt lf  =
  match e with 
  | OBinop(OLt,e1,e2) -> 
    let r1 = generate_reg () in
    let r2 = generate_reg () in
    let l3 = add_in_graph (RJccb(Ne,r1,r2,lt,lf)) in
    let l2 = tr_expression e2 r2 l3 in
    let l1 = tr_expression e1 r1 l2 in
    l1
  
  | _ -> general_case e lt lf
;;

let rec tr_instruction inst ld rret lret =
  match inst with 
  | OPutchar e ->
    let reg = generate_reg () in
    let l = add_in_graph (RPutchar (reg, ld)) in
    tr_expression e reg l
  | OWhile (e,s) ->
    let l_goto = generate_label () in
    let te = tr_sequence s rret l_goto in
    let le = tr_condition e te ld in 
    add_in_graph_with_label (RJmp le) l_goto;
    le
  | OIf (e,s1,s2) ->
    let c1 = tr_sequence s1 rret ld in
    let c2 = tr_sequence s2  rret ld in
    tr_condition e c1 c2
  | OReturn e ->
    let reg_e = generate_reg () in
    let jmp = add_in_graph (RJmp lret) in
    let l = add_in_graph (RMove (reg_e, rret, jmp)) in
    tr_expression e reg_e  l
  | OSet (s,e) -> 
    let r = get_var s in
    let rd = generate_reg () in
    let l = add_in_graph (RMove(rd,r,ld)) in
    tr_expression e r l
    
  | OExpr e -> tr_expression e (generate_reg ()) ld
  | OWrite (e1,e2) -> 
    let r1 = generate_reg () in
    let r2 = generate_reg () in
    let l3 = add_in_graph (RMove (r1,r2,ld)) in
    let l2 = tr_expression e2 r2 l3 in
    let l1 = tr_expression e1 r1 l2 in
    l1
  | OSeq s -> tr_sequence s rret ld
  
  and tr_sequence s rret ld =
    List.fold_right (fun i l -> tr_instruction i l rret ld) s ld
    
  ;;

let add_vars lst =
  List.map (fun id -> let r = generate_reg() in Hashtbl.replace env id r; r) lst;;

let  print_env () =
  Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k v) env;;

let print_list lst =
  List.iter (fun id -> Printf.printf "%s\n" id) lst;;

let tr_function (fdef: Llast.function_def) =
  let params = add_vars fdef.params in
  let locals = add_vars fdef.locals in
  let lret = add_in_graph (RReturn) in
  let result = generate_reg () in
  let entry = tr_sequence fdef.code result lret in
  let code = !graph in

  print_env () ;
  
  {
    name  = fdef.name;
    params =params;
    result = result;
    locals = locals;
    entry = entry;
    exit = lret;
    code = code;
  }
;;

let tr_program (p : Llast.program) : Rtl.program =
  let globals = add_vars p.globals in
  let functions = List.map tr_function p.functions in

  {
    globals = globals;
    functions = functions;
  }
 
  
;;

  
