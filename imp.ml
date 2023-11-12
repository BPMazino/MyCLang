

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
  | DCall of expression  * expression list 
  | Deref of expression (* déréférencement d'une variable *)
  | Alloc   of expression (* allocation d'une variable *)
  | Addr of string (* adresse d'une variable *)

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
  | Write  of expression * expression
  | Seq of sequence (* This constructor is convenient when a single Objlng instruction translates to (a sequence of) more than one Imp instructions.*)
(* Instruction sequence *)
and sequence = instruction list

type branchement = 
  | Bqez
  | Bnez


(* Vérification que e est sans effet de bord *)
(* let rec pure e =
  match e with
  | Cst _ | Bool _ -> true
  | Var _ -> false
  | Binop (_, e1, e2) -> pure e1 && pure e2
  | Call (_, el) -> false
  | DCall (e, el) -> false
  | Deref e -> false
  | Alloc e -> false
  | Addr _ -> false
 *)

(*
   mkAdd(n1, n2) = n1 + n2
mkAdd(0, e) = e
mkAdd(e, 0) = e
mkAdd((add n1) e, n2) = mkAdd(n1 + n2, e)
mkAdd(n, e) = (add n) e
mkAdd(e, n) = (add n) e
mkAdd(e1, e2) = add e1 e2 sinon
*)

(* let rec mkAdd e1 e2 = 
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
 *)



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



let print_list print_elt sep fmt l =
  let rec print_list_aux fmt l =
    match l with
    | [] -> ()
    | [x] -> print_elt fmt x
    | x :: l -> Format.fprintf fmt "%a%s%a" print_elt x sep print_list_aux l
  in
  Format.fprintf fmt "%a" print_list_aux l

(** Print a binary operator *)
let print_binop fmt op =
  match op with
  | Add -> Format.fprintf fmt "+"
  | Mul -> Format.fprintf fmt "*"
  | Lt  -> Format.fprintf fmt "<"



(** Print an expression *)
let rec print_expression fmt e =
  match e with
  | Cst n -> Format.fprintf fmt "%d" n
  | Bool b -> Format.fprintf fmt "%b" b
  | Var x -> Format.fprintf fmt "%s" x
  | Binop (op, e1, e2) ->
     Format.fprintf fmt "(%a %a %a)"
                    print_expression e1
                    print_binop op
                    print_expression e2
  | Call (f, el) ->
     Format.fprintf fmt "%s(%a)"
                    f
                    (print_list print_expression ", ") el
  | DCall (e, el) -> 
     Format.fprintf fmt "%a(%a)"
                    print_expression e
                    (print_list print_expression ", ") el
  | Deref e ->
      Format.fprintf fmt "*%a" print_expression e
  | Alloc e ->
      Format.fprintf fmt "alloc(%a)" print_expression e
  | Addr x ->
      Format.fprintf fmt "&%s" x

(** Print an instruction *)
let rec print_instruction fmt i =
  match i with
  | Putchar e ->
     Format.fprintf fmt "putchar(%a)" print_expression e
  | Set (x, e) ->
     Format.fprintf fmt "%s = %a" x print_expression e
  | If (e, s1, s2) ->
     Format.fprintf fmt "if (%a) {@\n%a@\n} else {@\n%a@\n}"
                    print_expression e
                    print_sequence s1
                    print_sequence s2
  | While (e, s) ->
     Format.fprintf fmt "while (%a) {@\n%a@\n}"
                    print_expression e
                    print_sequence s
  | Return e ->
     Format.fprintf fmt "return %a" print_expression e
  | Expr e ->
     Format.fprintf fmt "%a" print_expression e
  | Write (e1, e2) ->
     Format.fprintf fmt "%a = %a" print_expression e1 print_expression e2
  | Seq s ->
      Format.fprintf fmt "%a" print_sequence s
and print_sequence fmt s =
  Format.fprintf fmt "%a"
                 (print_list print_instruction "@\n") s

(** Print a function definition *)
let print_function_def fmt f =
  Format.fprintf fmt "function %s(%a) {@\n%a@\n}"
                 f.name
                 (print_list Format.pp_print_string ", ") f.params
                 print_sequence f.code

(** Print a program *)
let print_program fmt p =
  Format.fprintf fmt "var %a@\n@\n%a"
                 (print_list Format.pp_print_string ", ") p.globals
                 (print_list print_function_def "@\n@\n") p.functions




let string_of_list print_elt sep l =
  let rec string_of_list_aux l =
    match l with
    | [] -> ()
    | [x] -> print_elt x
    | x :: l -> print_elt x; Printf.printf "%s" sep;  string_of_list_aux l
  in
  string_of_list_aux l


(** Print a binary operator *)
let  string_of_binop op =
  match op with
  | Add -> Printf.printf "+"
  | Mul -> Printf.printf "*"
  | Lt  -> Printf.printf "<"

let rec string_of_expr e =
  match e with
  | Cst n -> Printf.printf "%d" n
  | Bool b -> Printf.printf "%b" b
  | Var x -> Printf.printf "%s" x
  | Binop (op, e1, e2) ->
     Printf.printf "(";
     string_of_expr e1;
     string_of_binop op;
     string_of_expr e2;
     Printf.printf ")"
  | Call (f, el) ->
     Printf.printf "%s(" f;
     string_of_list string_of_expr ", " el;
     Printf.printf ")"
  | DCall (e, el) -> 
     string_of_expr e;
     Printf.printf "(";
     string_of_list string_of_expr ", " el;
     Printf.printf ")"
  | Deref e ->
      Printf.printf "*";
      string_of_expr e
  | Alloc e ->
      Printf.printf "alloc(";
      string_of_expr e;
      Printf.printf ")"
  | Addr x ->
      Printf.printf "&%s" x
