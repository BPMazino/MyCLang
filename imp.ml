

(**
   Abstract syntax for the IMP language.
   Definition of types and data structures to represent an IMP program
   as caml data.
 *)

(**
   Binary operators: +, *, <
 *)
type binop = Add | Mul | Lt

(**
   Data structure for expressions
 *)
type expression =
  (* Integer constant: 0, -1, 42, ... *)
  | Cst   of int
  (* Boolean constant: true, false *)
  | Bool  of bool
  (* Variable, identified by a name *)
  | Var   of string
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list

(**
   Data structure for instructions
*)
type instruction =
  (* Primitive operation for printing a char, given as ASCII code *)
  | Putchar of expression
  (* Assignment of a new value to a variable *)
  | Set     of string * expression
  (* Conditional *)
  | If      of expression * sequence * sequence
  (* Loop *)
  | While   of expression * sequence
  (* Function termination *)
  | Return  of expression
  (* Expression used as an instruction (typically function call) *)
  | Expr    of expression
(* Instruction sequence *)
and sequence = instruction list




(* Vérification que e est sans effet de bord *)
let rec pure e =
  match e with
  | Cst _ | Bool _ -> true
  | Var _ -> false
  | Binop (_, e1, e2) -> pure e1 && pure e2
  | Call (_, el) -> false


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
  | Cst 0, e | e, Cst 0 -> e
  | Cst n1, Cst n2 -> Cst (n1 + n2)
  | Binop (Add, Cst n1, e), Cst n2 -> mkAdd (Cst (n1 + n2)) e
  | Binop (Add, e, Cst n1), Cst n2 -> mkAdd e (Cst (n1 + n2))
  | Binop (Add, e1', e2'), e -> mkAdd e1' (mkAdd e2' e)
  | e, Binop (Add, e1', e2') -> mkAdd (mkAdd e e1') e2'
  | Cst n, e | e, Cst n -> Binop (Add, Cst n, e)
  | _ -> Binop (Add, e1, e2)

let rec mkMul e1 e2 = 
  match e1, e2 with
  | Cst 0, _ | _, Cst 0 -> Cst 0
  | Cst 1, e | e, Cst 1 -> e
  | Cst n1, Cst n2 -> Cst (n1 * n2)
  | Binop (Mul, Cst n1, e), Cst n2 -> mkMul (Cst (n1 * n2)) e
  | Binop (Mul, e, Cst n1), Cst n2 -> mkMul e (Cst (n1 * n2))
  | Cst n, e | e, Cst n -> Binop (Mul, Cst n, e)
  | Binop (Mul, e1', e2'), e -> mkMul e1' (mkMul e2' e)
  | e, Binop (Mul, e1', e2') -> mkMul (mkMul e e1') e2'
  | _ -> Binop (Mul, e1, e2)


let rec mkLt e1 e2 = 
  match e1, e2 with
  | Cst n1 , Cst n2 -> Bool (n1 < n2) (* mkLt(n1, n2) = n1 < n2 *)
  | Cst n, e | e, Cst n -> Binop (Lt, Cst n, e) (* mkLt(n, e) = (lt n) e *)
  | _ -> Binop (Lt, e1, e2)


let rec mkBinop op e1 e2 =
  match op with
  | Add -> mkAdd e1 e2
  | Mul -> mkMul e1 e2
  | Lt -> mkLt e1 e2




(** 
   Data structure for a function definition
 *)
type function_def = {
    (* Function name *)
    name: string;
    (* List of named parameters *)
    params: string list;
    (* List of named local variables *)
    locals: string list;
    (* The actual code *)
    code: sequence;
  }

(**
   Data structure for a program
 *)
type program = {
    (* List of named global variables *)
    globals: string list;
    (* The functions defined by the program *)
    functions: function_def list;
  }





