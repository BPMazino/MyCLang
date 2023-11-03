open Rtl;;
open Imp;;

(* imp to RTL *)

let env = Hashtbl.create 16;;

let (graph : Rtl.cfg ref) = ref (Hashtbl.create 16);;




let cpt_label = ref (-1) 
let new_label () =

  fun () ->
    incr cpt_label;
    Printf.printf "L%d\n" !cpt_label;  
    !cpt_label

let generate_reg =  Mips.fresh ;;
let generate_label = new_label ();;


let add_in_graph (i : Rtl.instruction) = 
  let l = generate_label () in
  Hashtbl.add !graph l i;
  l;;
let add_in_graph_unit i  l =
  Hashtbl.add !graph l i
;;
  

(* Translation of an expression*)

(*e : expression to translate  r : pseudo register to store the result dest : destination of the result*)
let rec tr_expression e rd ld =
  match e with 
  | Cst n -> add_in_graph (RCst (n,rd,ld))
  | Bool b -> add_in_graph (RBool (b,rd,ld))
  | Var x ->  
    begin match  Hashtbl.find_opt env x with
      | Some r -> add_in_graph (RMove (r,rd,ld))
      | None -> 
        let r = generate_reg () in
        Hashtbl.add env x r;
        add_in_graph (RMove (r,rd,ld))
    end
  | Binop(op,e1,e2)  ->  tr_binop op e1 e2 rd ld
  | Call (f,args) -> tr_call f args rd ld
    
  and tr_binop op e1 e2 rd ld =
    let r2 = generate_reg () in
    let l3 = add_in_graph (RBinop (op, r2, rd, ld)) in
    let l2 = tr_expression e2 r2 l3 in
    let l1 = tr_expression e1 rd l2 in
    l1

  and tr_call f args rd ld =
    let r_args = List.rev_map (fun _ -> generate_reg ()) args in
    let l = add_in_graph (RCall (rd,f,r_args,ld)) in
    List.fold_right2 tr_expression args r_args l 

  
let general_case e lt lf =
  let r = generate_reg () in
  let l2 = add_in_graph (Jz (r,lf,lt)) in
  let l = tr_expression e r l2 in
  l




let tr_condition e lt lf  =
  match e with 
  | Binop(Lt,e1,e2) -> 
    let r1 = generate_reg () in
    let r2 = generate_reg () in
    let l3 = add_in_graph (Jle (r2,r1,lt,lf)) in
    let l2 = tr_expression e2 r2 l3 in
    let l1 = tr_expression e1 r1 l2 in
    l1
  
  | _ -> general_case e lt lf;;

let rec tr_instruction inst ld lret =
  match inst with 
  | Putchar e ->
    let reg = generate_reg () in
    let l = add_in_graph (RPutchar (reg, ld)) in
    tr_expression e reg l
  | While (e,s) ->
    let l_goto = generate_label () in
    let te = tr_sequence s l_goto in
    let le = tr_condition e te  ld in 
    add_in_graph_unit (RGoto le) l_goto;
    le
  | If (e,s1,s2) ->
    let c1 = tr_sequence s1 ld in
    let c2 = tr_sequence s2 ld in
    tr_condition e c1 c2
  | Return e ->
    let rret = generate_reg () in
    tr_expression e rret lret
  | Set (s,e) -> 
    begin match Hashtbl.find_opt env s with
      | Some r -> 
        let l = tr_expression e r ld in
        l
      | None -> 
        let r = generate_reg () in
        Hashtbl.add env s r;
        let l = tr_expression e r ld in
        l
    end
  | Expr e -> tr_expression e (generate_reg ()) ld
and tr_sequence s ld =
  match s with 
  | [] -> ld
  | [i] -> tr_instruction i ld ld
  | i::s -> tr_instruction i (tr_sequence s ld) ld;;



let add_vars params = 
  List.fold_left (fun acc x -> 
      let r = generate_reg () in
      Hashtbl.add env x r;
      r::acc
    ) [] params;;
let tr_function (fdef: Imp.function_def) =
  let lret = generate_label () in
  let entry = tr_sequence fdef.code lret in
  let code = !graph in
  let locals = Hashtbl.fold (fun _ r acc -> r::acc) env [] in
  let result = generate_reg () in
  {
    name  = fdef.name;
    params = add_vars fdef.params;
    result = result;
    locals = locals;
    entry = entry;
    exit = lret;
    body = code;
  }
;;
Hashtbl.clear env;;
Hashtbl.clear !graph;;
let tr_program (p : Imp.program) : Rtl.program =
  let functions = List.map tr_function p.functions in
  {
    globals = p.globals;
    functions = functions;
  }
;;

  
