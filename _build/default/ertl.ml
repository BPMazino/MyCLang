


type register =  Mips.register
type label = int;;



type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | ECst   of int * register * label
  (* Boolean constant: true, false *)
  | EBool  of bool * register * label
  (* Assignement d'une valeur*)
  | EMove of register * register * label
  | EUnop of Llast.unop * register * label
  (* Binary operation, with an operator and two operands *)
  | EBinop of Llast.binop * register * register * label 
  (* Function call, with a function name and a list of parameters *)
  | ECall  of string * int * label
  (* Conditional *)
  | ECmp   of  register * register * label
  | ECmpi  of  int * register * label
  (*Putchar*)
  | EPutchar of register * label
  | EJmp of label (*saut inconditionnel Jmp*)
  | EJccb of Rtl.condition * register  * register* label * label (*saut conditionnel Jcc*)
  | EJccu of Rtl.condition * register * label * label (*saut conditionnel Jcc*)
  (* | RSetcc of Llast.condition * register * label (*setcc*) *)
  | ESetccu of Rtl.condition * register * label (*setcc*)
  | ESetccb of Rtl.condition * register * register * label (*setcc*)
  | EReturn 

  | EAlloc_frame of label
  | EDelete_frame of label
  | EPush_param of register * label
  | EGet_param of register * label
  | EStack_offset of register * int  * label

  | ENop of label

type cfg = (label, instruction) Hashtbl.t  ;;


type function_def = {
  name : string;
  nb_params: int;
  locals: register list;
  entry : label;
  code : cfg;
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

let print_condition fmt (c:Rtl.condition) =
  match c with
  | Lt -> Format.fprintf fmt "lt"
  | Le -> Format.fprintf fmt "le"
  | Eq -> Format.fprintf fmt "eq"
  | Ne -> Format.fprintf fmt "ne"
  | Ge -> Format.fprintf fmt "ge"
  | Gt -> Format.fprintf fmt "gt"


let print_instruction fmt i =
  match i with 
  | ECst (n,r,l) -> Format.fprintf fmt "loadi #%d %a  --> %a" n print_register r  print_label l
  | EBool (b,r,l) -> Format.fprintf fmt "loadb #%b %a  --> %a" b print_register r  print_label l
  | EMove (r1, r2, l) -> Format.fprintf fmt "mov %a %a  --> %a" print_register r1 print_register r2  print_label l 
  | EBinop (b, r1, r2, l) -> Format.fprintf fmt "%a %a %a  --> %a" Llast.print_binop b print_register r1 print_register r2  print_label l
  | ECall (f, n, l) -> Format.fprintf fmt "call %s %d  --> %a" f n print_label l
  | EPutchar (r, l) -> Format.fprintf fmt "putchar %a  --> %a" print_register r  print_label l
  | EJmp l -> Format.fprintf fmt "jmp %a" print_label l
  | EJccb (b, r1, r2, l1, l2) -> Format.fprintf fmt "jcc %a %a %a  --> %a" print_condition b print_register r1 print_register r2  print_label l1
  | EJccu (b, r, l1, l2) -> Format.fprintf fmt "jcc %a %a  --> %a" print_condition b print_register r  print_label l1
  | EReturn -> Format.fprintf fmt "return"
  | EUnop (u, r, l) -> Format.fprintf fmt "%a %a  --> %a" Llast.print_unop u print_register r  print_label l

  | EAlloc_frame l -> Format.fprintf fmt "alloc_frame --> %a" print_label l
  | EDelete_frame l -> Format.fprintf fmt "delete_frame --> %a" print_label l
  | EPush_param (r, l) -> Format.fprintf fmt "push_param %a --> %a" print_register r print_label l
  | EGet_param (r, l) -> Format.fprintf fmt "get_param %a --> %a" print_register r print_label l
  | EStack_offset (r, n, l) -> Format.fprintf fmt "stack_offset %a %d --> %a" print_register r n print_label l
  (* | EAddi (n, r, l) -> Format.fprintf fmt "addi %d %a --> %a" n print_register r print_label l *)
  | ENop l -> Format.fprintf fmt "nop --> %a" print_label l
  | ECmp (r1, r2, l) -> Format.fprintf fmt "cmp %a %a --> %a" print_register r1 print_register r2 print_label l
  | ECmpi (n, r, l) -> Format.fprintf fmt "cmpi %d %a --> %a" n print_register r print_label l
  | ESetccu (c, r, l) -> Format.fprintf fmt "setcc %a %a --> %a" print_condition c print_register r print_label l
  | ESetccb (c, r1,r2, l) -> Format.fprintf fmt "setcc %a %a %a --> %a" print_condition c print_register r1 print_register r2 print_label l
  


  


let print_cfg fmt cfg =
  let print_instruction fmt (l, i) =
    Format.fprintf fmt "%a: %a\n" print_label l print_instruction i
  in
  Format.fprintf fmt "{@[<v 0>%a@]}"
    (Utils.print_list print_instruction) (Hashtbl.to_seq cfg |> List.of_seq)


let print_function_def fmt f =
  Format.fprintf fmt "%s(@[@])@\n" f.name;
  Format.fprintf fmt "  @[";
  Format.fprintf  fmt "entry : %a@\n"  print_label f.entry;
  Format.fprintf  fmt "locals: @[%a@]@\n" (Utils.print_list print_register) f.locals;
  print_cfg fmt f.code ;
  Format.fprintf  fmt "@]@."
let print_program fmt p =
  Format.fprintf fmt "globals: @[%a@]@\n@\n%a"
    (Utils.print_list Format.pp_print_string) p.globals
    (Utils.print_list print_function_def) p.functions


