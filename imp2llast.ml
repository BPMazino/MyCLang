open Imp;;
open Llast;;
let rec pure e =
  match e with
  | OCst _ | OBool _ -> true
  | OVar _ -> false
  | OBinop (_, e1, e2) -> pure e1 && pure e2
  | OCall (_, el) -> false
  | OUnop (_, e) -> pure e
  | _ -> false


(*
   mkAdd(n1, n2) = n1 + n2
mkAdd(0, e) = e
mkAdd(e, 0) = e
mkAdd((add n1) e, n2) = mkAdd(n1 + n2, e)
mkAdd(n, e) = (add n) e
mkAdd(e, n) = (add n) e
mkAdd(e1, e2) = add e1 e2 sinon
*)

let rec mkAdd e1 e2 = 
  match e1, e2 with
  | OCst n1, OCst n2 -> OCst (n1 + n2)
  | OCst 0, e | e, OCst 0 -> e
  | OUnop ((OAddi n1), e),OCst n2  -> mkAdd (OCst (n1 + n2)) e
  | OCst n, e | e, OCst n -> OUnop ((OAddi n), e)
  | _ -> OBinop (OAdd, e1, e2)
 

let rec mkMul e1 e2 = 
  match e1, e2 with
  | OCst n1, OCst n2 -> OCst (n1 * n2)
  | OCst 0, e when pure e -> OCst 0
  | e, OCst 0 when pure e -> OCst 0
  | OCst 1, e -> e
  | e, OCst 1  -> e
  | OUnop ((OAddi n1), e), OCst n2 ->mkAdd (OCst (n1 *  n2)) (mkMul e (OCst n2))
  | OCst n, e | e, OCst n -> OUnop ((OAddi n), e)                                                 
  | _ -> OBinop (OMul, e1, e2)


let rec mkLt e1 e2 = 
  match e1, e2 with
  | OCst n1 , OCst n2 -> OBool (n1 < n2) (* mkLt(n1, n2) = n1 < n2 *)
  | OCst n, e | e, OCst n -> OBinop (OLt, OCst n, e) (* mkLt(n, e) = (lt n) e *)
  | _ -> OBinop (OLt, e1, e2)


let rec mkBinop op e1 e2 =
  match op with
  | Add -> mkAdd e1 e2
  | Mul -> mkMul e1 e2
  | Lt -> mkLt e1 e2




let rec tr_expression (e : Imp.expression) : Llast.expression = 
  match e with
  | Cst n -> OCst n
  | Bool b -> OBool b
  | Var x -> OVar x
  | Binop (op, e1, e2) -> mkBinop op (tr_expression e1) (tr_expression e2)
  | Call (f, el) -> OCall (f, List.map tr_expression el)
  | DCall (e, el) -> ODCall (tr_expression e, List.map tr_expression el)
  | Deref(e) -> ODeref(tr_expression e)
  | Alloc (e) -> OAlloc(tr_expression e)
  | Addr (s) -> OAddr(s)


let rec tr_instruction (i : Imp.instruction) : Llast.instruction = 
  match i with
  | Expr e -> OExpr (tr_expression e)
  | If (e, s1, s2) -> OIf (tr_expression e, tr_sequence s1, tr_sequence s2)
  | While (e, s) -> OWhile (tr_expression e, tr_sequence s)
  | Return e -> OReturn (tr_expression e)
  | Set (x, e) -> OSet (x, tr_expression e)
  | Putchar(e) -> OPutchar(tr_expression e)
  | Write (e1,e2) -> OWrite(tr_expression e1, tr_expression e2)
  | Seq s -> OSeq (tr_sequence s)


and tr_sequence (s : Imp.sequence) : Llast.sequence =
  List.map tr_instruction s


let tr_function_def (f : Imp.function_def) : Llast.function_def =
  { name = f.name;
    params = f.params;
    locals = f.locals;
    code = tr_sequence f.code;
  }

let tr_program (p : Imp.program) : Llast.program =
  { globals = p.globals;
    functions = List.map tr_function_def p.functions;
  }