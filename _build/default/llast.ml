(*Syntaxe abstraite pour la sélection d'instructionuction mini-C*)

type binop = 
| OAdd (* integer addition *)
| OMul (* integer multiplication *)
| OLt (* less than *)

type unop = 
| OAddi of int (* in mips addi n *)
| OSubi of int (* in mips subi n *)


type expression = 
| OCst of int 
| OBool of bool
| OVar of string 
| OBinop of binop * expression * expression
| OUnop of unop * expression
| OCall of string * expression list
| ODCall of expression  * expression list 
| ODeref of expression (* déréférencement d'une variable *)
| OAlloc   of expression (* allocation d'une variable *)
| OAddr of string (* adresse d'une variable *)



;;





type instruction = 
|OExpr of expression
|OIf of expression * sequence * sequence
|OWhile of expression * sequence
|OReturn of expression
|OSet of string * expression
|OPutchar of expression
| OWrite of expression * expression
| OSeq of sequence 

and sequence = instruction list

;;

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


type program = {
  globals: string list;
  functions: function_def list;
}


(*Print *)

open Format 

let print_unop fmt = function
  | OAddi n -> fprintf fmt "addi %d" n
  | OSubi n -> fprintf fmt "subi %d" n

let print_binop fmt = function
  | OAdd -> fprintf fmt "add"
  | OMul -> fprintf fmt "mul"
  | OLt -> fprintf fmt "slt"

let print_string fmt s = fprintf fmt "%s" s


let rec print_expression fmt = function
  | OCst n -> fprintf fmt "%d" n
  | OBool b -> fprintf fmt "%b" b
  | OVar x -> fprintf fmt "%s" x
  | OBinop (op, e1, e2) -> fprintf fmt "%a %a %a" print_expression e1 print_binop op print_expression e2
  | OUnop (op, e) -> fprintf fmt "%a %a" print_unop op print_expression e
  | OCall (f, el) -> fprintf fmt "%s(%a)" f (Utils.print_list print_expression) el
  | ODCall (e, el) -> fprintf fmt "%a(%a)" print_expression e (Utils.print_list print_expression) el
  | ODeref e -> fprintf fmt "*%a" print_expression e
  | OAlloc e -> fprintf fmt "alloc(%a)" print_expression e
  | OAddr x -> fprintf fmt "&%s" x

and print_instruction fmt = function
  | OExpr e -> fprintf fmt "%a;" print_expression e
  | OIf (e, s1, s2) -> fprintf fmt "if (%a) {@\n%a@\n} else {@\n%a@\n}" print_expression e print_sequence s1 print_sequence s2
  | OWhile (e, s) -> fprintf fmt "while (%a) {@\n%a@\n}" print_expression e print_sequence s
  | OReturn e -> fprintf fmt "return %a;" print_expression e
  | OSet (x, e) -> fprintf fmt "%s = %a;" x print_expression e
  | OPutchar e -> fprintf fmt "putchar(%a);" print_expression e
  | OWrite (e1, e2) -> fprintf fmt "write(%a, %a);" print_expression e1 print_expression e2
  | OSeq s -> fprintf fmt "%a" print_sequence s

and print_sequence fmt s =
  Utils.print_list print_instruction fmt s

let print_function_def fmt f =
  fprintf fmt "function %s(%a) {@\n%a@\n}" f.name (Utils.print_list print_string) f.params print_sequence f.code

let print_program fmt p =
  fprintf fmt "globals(%a)@\n%a" (Utils.print_list print_string) p.globals (Utils.print_list print_function_def) p.functions
  









