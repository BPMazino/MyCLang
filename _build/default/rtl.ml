(*Register Transfer Language*)

type label = int


(*Pseudo Register*)
type pseudo_reg = 
  | PReg of Mips.register
  | PRegInt of int
  


(*Control Flow Graph*)
(* Le graphe de flot de contrôle peut être représenté par un dictionnaire associant une
instruction RTL à chaque étiquette. *)

type cfg = (label, instruction) Hashtbl.t


type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | Cst   of int * Mips.register * label
  (* Boolean constant: true, false *)
  | Bool  of bool * Mips.register * label
  (* Variable, identified by a name *)
  | Var   of string 
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list
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



type function_def = {
  name : string;
  params: register list;
  locals: register list;
  entry : label;
  exit : label;
  body : cfg;
}

type program = {
  globals: (string * int) list;
  functions: function_def list;
}

