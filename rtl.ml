(*Register Transfer Language*)

type label = int


(*Pseudo Register*)
type register = Mips.register
  


(*Control Flow Graph*)
(* Le graphe de flot de contrôle peut être représenté par un dictionnaire associant une
instruction RTL à chaque étiquette. *)

type condition = Lt | Le | Eq | Ne | Ge | Gt

type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | RCst   of int * register * label
  (* Boolean constant: true, false *)
  | RBool  of bool * register * label
  (* Assignement d'une valeur*)
  | RMove of register * register * label
  | RUnop of Llast.unop * register * label
  (* Binary operation, with an operator and two operands *)
  | RBinop of Llast.binop * register * register * label 
  (* Function call, with a function name and a list of parameters *)
  | RCall  of register * string * register list * label
  (* Conditional *)
  | RCmp   of  register * register * label
  (*Putchar*)
  | RPutchar of register * label
  | RJmp of label (*saut inconditionnel Jmp*)
  | RJccb of condition * register  * register* label * label (*saut conditionnel Jcc*)
  | RJccu of condition * register * label * label (*saut conditionnel Jcc*)
  | RSetccu of condition * register * label (*setcc*)
  | RSetccb of condition * register * label (*setcc*)
  (* | RSetcc of Llast.condition * register * label (*setcc*) *)
  | RReturn 
  | RAlloc of register * label
  
  

type cfg = (label, instruction) Hashtbl.t  ;;


type function_def = {
  name : string;
  params: register list;
  result: register;
  locals: register list;
  entry : label;
  exit : label;
  code : cfg;
}

type program = {
  globals: string list;
  functions: function_def list;
}



(** Print RTL instructions *)
let print_register fmt r =
  Mips.print_register fmt r

let print_register_list fmt l =
  Format.fprintf fmt "@[%a@]" (Utils.print_list print_register) l

let print_label fmt l =
  Format.fprintf fmt "L%d" l

let print_condition fmt c =
  match c with
  | Lt -> Format.fprintf fmt "lt"
  | Le -> Format.fprintf fmt "le"
  | Eq -> Format.fprintf fmt "eq"
  | Ne -> Format.fprintf fmt "ne"
  | Ge -> Format.fprintf fmt "ge"
  | Gt -> Format.fprintf fmt "gt"

let print_instruction fmt i =
  match i with 
  | RCst (n,r,l) -> Format.fprintf fmt "loadi #%d %a  --> %a" n print_register r  print_label l
  | RBool (b,r,l) -> Format.fprintf fmt "mov #%b %a  --> %a" b print_register r  print_label l
  | RMove (r1, r2, l) -> Format.fprintf fmt "mov %a %a  --> %a" print_register r1 print_register r2  print_label l 
  | RBinop (b, r1, r2, l) -> Format.fprintf fmt "%a %a %a  --> %a" Llast.print_binop b print_register r1 print_register r2  print_label l
  | RCall (r, f, args, l) -> Format.fprintf fmt "%a <- call %s(@[%a@])  --> %a" print_register r f (Utils.print_list print_register) args  print_label l
  (* | RSetcc (c, r, l) -> Format.fprintf fmt "set %a %a  --> %a" Llast.print_condition c print_register r  print_label l *)
  | RCmp (r1, r2, l) -> Format.fprintf fmt "cmp %a %a  --> %a" print_register r1 print_register r2  print_label l
  | RPutchar (r, l) -> Format.fprintf fmt "putchar %a  --> %a" print_register r  print_label l
  | RJmp l -> Format.fprintf fmt "jmp %a" print_label l
  | RJccb (b, r1, r2, l1, l2) -> Format.fprintf fmt "jcc %a %a %a  --> %a" print_condition b print_register r1 print_register r2  print_label l1
  | RJccu (b, r, l1, l2) -> Format.fprintf fmt "jcc %a %a  --> %a" print_condition b print_register r  print_label l1
  | RReturn -> Format.fprintf fmt "return"
  | RUnop (u, r, l) -> Format.fprintf fmt "%a %a  --> %a" Llast.print_unop u print_register r  print_label l
  | RSetccu (c, r, l) -> Format.fprintf fmt "set %a %a  --> %a" print_condition c print_register r  print_label l
  | RSetccb (c, r, l) -> Format.fprintf fmt "set %a %a  --> %a" print_condition c print_register r  print_label l
  | RAlloc (r, l) -> Format.fprintf fmt "alloc %a  --> %a" print_register r  print_label l



let print_cfg fmt cfg =
  let print_instruction fmt (l, i) =
    Format.fprintf fmt "%a: %a\n" print_label l print_instruction i
  in
  Format.fprintf fmt "{\n@[<v 0>%a@]}"
    (Utils.print_list print_instruction) (Hashtbl.to_seq cfg |> List.of_seq)


let print_function_def fmt f =
  Format.fprintf fmt "%a %s(@[%a@])@\n" print_register f.result f.name
    (Utils.print_list print_register) f.params;
  Format.fprintf fmt "  @[";
  Format.fprintf  fmt "entry : %a@\n"  print_label f.entry;
  Format.fprintf  fmt "exit  : %a@\n" print_label f.exit;
  Format.fprintf  fmt "locals: @[%a@]@\n" (Utils.print_list print_register) f.locals;
  print_cfg fmt f.code ;
  Format.fprintf  fmt "@]@."
let print_program fmt p =
  Format.fprintf fmt "globals: @[%a@]@\n@\n%a"
    (Utils.print_list Format.pp_print_string) p.globals
    (Utils.print_list print_function_def) p.functions
    
  


