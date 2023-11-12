open Objlng;;
open Lexing;;
module Env = Map.Make(String)

(*Create an error TypeError (loc,msg)*)
exception TypeError of loc * string

 let print_env env = 
    Env.iter (fun k v -> Printf.printf "%s : %s\n" k (string_of_typ v)) env
  
  let print_hash table  = 
    Printf.printf "printing hash\n";
    Hashtbl.iter (fun k v -> Printf.printf "%s : %s\n" k v.name) table
  

let loc_to_string ({fc;lc} : loc) : string = 
  Printf.sprintf "error in file %s, line %d, column %d" fc.pos_fname lc.pos_lnum (fc.pos_cnum - fc.pos_bol)

let (cenv : (string,  loc Objlng.class_def) Hashtbl.t) = Hashtbl.create 16 ;;

(* let loc2str ({fc;lc } : loc) : string  = 
  Printf.sprintf "error in file %s, line %d, column %d" fc lc.pos_lnum lc.pos_cnum *)
;;

let error loc msg = 
  raise (TypeError (loc,msg))
(* let did_not_find_var v ({fc;lc}) = 
  error (Printf.sprintf "Did not find variable %s at %s" v (loc2str {fc;lc})) *)
let get_type_var v env loc = 
  match Env.find_opt v env with
  | Some t -> t
  | None -> error loc (Printf.sprintf "Did not find variable %s" v )

let get_type_fct f functions loc  =
  match List.find_opt (fun (e : 'a function_def) -> e.name = f)  functions with
  | Some f -> f
  | None -> error loc (Printf.sprintf "Did not find function %s" f)

let get_class_name c loc  = 
  match c with
  | TClass c -> c
  | _ ->  error loc (Printf.sprintf "Expected class but got %s" (string_of_typ c))

let get_class c loc = 
  match Hashtbl.find_opt cenv c with
  | Some c -> c
  | None -> error loc (Printf.sprintf "Did not find class %s" c)
let get_method cname m loc = 
  let c = Hashtbl.find cenv cname in
  match List.find_opt (fun (e : 'a function_def) -> e.name = m)  c.methods with
  | Some m -> m
  | None ->  error loc (Printf.sprintf "Did not find method %s" m)

let get_field (f:string) (fields:(string * typ) list) loc = 
  match List.find_opt (fun (e : 'a) -> fst e = f)  fields with
  | Some (_,t) -> t
  | None -> error loc  (Printf.sprintf "Did not find field %s" f)


let check_type x t loc =
  if x <> t then error loc  ("type mismatch: expected " ^ (string_of_typ x) ^ " but got " ^ (string_of_typ t))


let type_prog (prog : loc program) : typ program = 
  let rec type_expr expr env  :typ expression = 
    match expr.expr with
    | Cst n -> mk_expr TInt (Cst n)
    | Bool b -> mk_expr TBool (Bool b)
    | Var v -> let tv = get_type_var v env expr.annot in
      mk_expr tv (Var v) 
    | Binop ((Add|Mul) as op , e1, e2) -> 
      let te1 = type_expr e1 env in
      let te2 = type_expr e2 env in
      check_type te1.annot TInt e1.annot;
      check_type te2.annot TInt e2.annot;
      mk_expr TInt (Binop (op, te1, te2))
    | Binop ((Lt) as op , e1, e2) ->
      let te1 = type_expr e1 env in
      let te2 = type_expr e2 env in
      check_type te1.annot TInt e1.annot;
      check_type te2.annot TInt e2.annot;
      mk_expr TBool (Binop (op, te1, te2))
    | Call(f,args) -> type_call f args env expr.annot
    | MCall(e,f,args) -> type_mcall e f args env expr.annot
    | New(s,e) -> type_new s e env expr.annot
    | NewTab (t,e) ->  
      let te = type_expr e env in
      check_type te.annot TInt e.annot;
      mk_expr (TArray t) (NewTab (t,te))
    | Read m -> type_mem m env expr.annot
    | This -> mk_expr  (get_type_var "this" env expr.annot) (Var "this")
  
    and type_args args (f:loc function_def) env loc  = 
      List.fold_left2 (fun acc arg (_,t) -> 
          let t' = type_expr arg env in
          check_type t t'.annot arg.annot;
          t' :: acc
        ) [] args f.params
    

    and type_call f args env loc = 
      let tf = get_type_fct f prog.functions loc in
      let targs = type_args args tf env loc in
      mk_expr tf.return (Call(tf.name,targs))
    
    and  type_mcall e f args env loc = 
      let te = type_expr e env in
      let cname = get_class_name te.annot loc  in
      let m = get_method cname f loc  in
      let targs = type_args args m env loc  in
      mk_expr m.return (MCall(te,f,targs))

    and type_new s args env loc  = 
      let m = get_method s "constructor" loc  in
      let targs = type_args args m env  loc in
      mk_expr (TClass s) (New(s,targs))
    
    and  type_mem m env loc = 
      match m with 
      | Arr (arr,n) -> 
        let tarr = type_expr arr env in
        let tn = type_expr n env in
        (*what s the type of an argument*)
        let type_arg = (match tarr.annot with
          | TArray t -> t
          | _ -> error loc "Expected array")
        in
        check_type tn.annot TInt n.annot;
        check_type tarr.annot (TArray type_arg ) arr.annot;
        mk_expr (type_arg) (Read(Arr (tarr,tn)))
      | Atr (c,s) -> 
        let tc = type_expr c env in
        let cname = get_class_name tc.annot loc in
        let c = get_class cname  loc in
        let t = get_field s c.fields loc in
        mk_expr t (Read(Atr (tc,s)))
      in

    let rec type_instr instr return env : typ instruction= 
      match instr with
      | Putchar e ->
        let te = type_expr e env in
        check_type te.annot TInt e.annot;
        Putchar te
      | Set (v,e) ->
        let te = type_expr e env in
        let tv = get_type_var v env e.annot in
        check_type tv te.annot e.annot;
        Set (v,te)
      | If (e,s1,s2) ->
        let te = type_expr e env in
        check_type TBool te.annot  e.annot;
        let ts1 = type_seq s1 return env in
        let ts2 = type_seq s2 return env in
        If (te,ts1,ts2)
      | While (e,s) ->
        let te = type_expr e env in
        check_type te.annot TBool e.annot;
        let ts = type_seq s return env in
        While (te,ts)
      | Return e ->
        let te = type_expr e env in
        check_type te.annot return e.annot;
        Return te
      | Expr e ->
        let te = type_expr e env in
        Expr te
      | Write (m,e) -> type_write m e env

      and type_write m e env = 
        let te = type_expr e env in
        match m with 
        | Arr (arr,n) -> 
          let tn = type_expr n env in
          check_type tn.annot TInt n.annot; 
          let tarr = type_expr arr env in
          check_type tarr.annot (TArray te.annot) arr.annot;
          Write(Arr (tarr,tn),te)
        
        | Atr (c,s) ->
          let tc = type_expr c env in
          let cname = get_class_name tc.annot c.annot in
          let c = get_class cname  e.annot in
          let t = get_field s c.fields e.annot in
          check_type t te.annot e.annot;
          Write(Atr (tc,s),te)
          
      and type_seq seq return env : typ sequence = 
        List.map (fun i -> type_instr i return env) seq
      in
    
  let add_vars globals env  = 
    List.fold_left (fun env (v,t) -> Env.add v t env) env globals
  in  
  let env = add_vars prog.globals  Env.empty  in

  let type_function (f:loc function_def) env : typ function_def = 
    let env = add_vars  f.params env in
    let env = add_vars  f.locals env in
    let ts = type_seq f.code f.return env in
    {f with code = ts}
  in
 
  let type_class (c:loc class_def) : typ class_def = 
    
    let env = Env.add "this" (TClass c.name) env in
    let env = add_vars  c.fields env in
    Hashtbl.add cenv c.name c;
    let methods = List.map (fun m -> type_function m env) c.methods in
    {c with methods = methods}
  in
  let classes = List.map (fun c -> type_class c) prog.classes in
  let functions = List.map (fun f -> type_function f env) prog.functions in
  {globals = prog.globals; classes = classes; functions = functions}