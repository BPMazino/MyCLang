open Imp;;


type register =  Mips.register
type label = int;;



type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | ERCst   of int * register * label
  (* Boolean constant: true, false *)
  | ERBool  of bool * register * label
  (* Variable, identified by a name *)
  | ERMove of register * register * label
  (* Binary operation, with an operator and two operands *)
  | ERBinop of Imp.binop * register * register * label 
  (* Function call, with a function name and a list of parameters *)
  | ERCall  of string * int * label
  (* Assignment of a new value to a variable *)
  | ERSet     of  register * register * label
  (* Conditional *)
  | ERIf      of  register * label * label
  | ERPutchar of register * label
  (* Loop *)
  | ERWhile   of register * label * label
  | ERGoto of label 
  | EJle  of register * register * label * label
  | EJz of register * label * label
  | EReturn 
  | EAlloc_frame of label
  | EDelete_frame of label
  | EPush_param of register * label
  | EGet_param of int * register * label

type cfg = (label, instruction) Hashtbl.t  ;;


type function_def = {
  name : string;
  nb_params: int;
  locals: register list;
  entry : label;
  body : cfg;
}

type program = {
  globals: string list;
  functions: function_def list;
}


let print_register fmt r =
  Mips.print_register fmt r

let print_register_list fmt l =
  Format.fprintf fmt "@[%a@]" (Utils.print_list print_register) l

let print_label fmt l =
  Format.fprintf fmt "L%d" l

let print_instruction fmt i =
  match i with 
  | ERCst (n,r,l) -> Format.fprintf fmt "mov $%d %a  --> %a" n print_register r  print_label l
  | ERBool (b,r,l) -> Format.fprintf fmt "mov $%b %a  --> %a" b print_register r  print_label l
  | ERMove (r1, r2, l) -> Format.fprintf fmt "mov %a %a  --> %a" print_register r1 print_register r2  print_label l 
  | ERBinop (b, r1, r2, l) -> Format.fprintf fmt "binop %a %a %a  --> %a" Imp.print_binop b print_register r1 print_register r2  print_label l
  | ERCall (f, n, l) -> Format.fprintf fmt "call %s %d  --> %a" f n print_label l
  | ERSet (r1, r2, l) -> Format.fprintf fmt "set %a %a  --> %a" print_register r1 print_register r2  print_label l
  | ERIf (r, l1, l2) -> Format.fprintf fmt "if %a  --> %a %a" print_register r print_label l1 print_label l2
  | ERPutchar (r, l) -> Format.fprintf fmt "putchar %a  --> %a" print_register r print_label l
  | ERWhile (r, l1, l2) -> Format.fprintf fmt "while %a  --> %a %a" print_register r print_label l1 print_label l2
  | ERGoto l -> Format.fprintf fmt "goto %a" print_label l
  | EJle (r1, r2, l1, l2) -> Format.fprintf fmt "jle %a %a  --> %a %a" print_register r1 print_register r2 print_label l1 print_label l2
  | EJz (r, l1, l2) -> Format.fprintf fmt "jz %a  --> %a %a" print_register r print_label l1 print_label l2
  | EReturn -> Format.fprintf fmt "return"
  | EAlloc_frame l -> Format.fprintf fmt "alloc_frame --> %a" print_label l
  | EDelete_frame l -> Format.fprintf fmt "delete_frame --> %a" print_label l
  | EPush_param (r, l) -> Format.fprintf fmt "push_param %a --> %a" print_register r print_label l
  | EGet_param (n, r, l) -> Format.fprintf fmt "get_param %d %a --> %a" n print_register r print_label l
  


let print_cfg fmt cfg =
  let print_instruction fmt (l, i) =
    Format.fprintf fmt "%d: %a\n" l print_instruction i
  in
  Format.fprintf fmt "{@[<v 0>%a@]}"
    (Utils.print_list print_instruction) (Hashtbl.to_seq cfg |> List.of_seq)


let print_function_def fmt f =
  Format.fprintf fmt "function %s(@[%a@])@\n%a@\n@\n"
    f.name
    print_register_list f.locals
    print_cfg f.body


let print_program fmt p =
  Format.fprintf fmt "globals: @[%a@]@\n@\n%a"
    (Utils.print_list Format.pp_print_string) p.globals
    (Utils.print_list print_function_def) p.functions
    
  



