(*Register Transfer Language*)

type label = int


(*Pseudo Register*)
type register = Mips.register
  


(*Control Flow Graph*)
(* Le graphe de flot de contrôle peut être représenté par un dictionnaire associant une
instruction RTL à chaque étiquette. *)


type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | RCst   of int * register * label
  (* Boolean constant: true, false *)
  | RBool  of bool * register * label
  (* Variable, identified by a name *)
  | RMove of register * register * label
  (* Binary operation, with an operator and two operands *)
  | RBinop of Imp.binop * register * register * label 
  (* Function call, with a function name and a list of parameters *)
  | RCall  of register * string * register list * label
  (* Assignment of a new value to a variable *)
  | RSet     of  register * register * label
  (* Conditional *)
  | RIf      of  register * label * label
  | RPutchar of register * label
  (* Loop *)
  | RWhile   of register * label * label
  | RGoto of label 
  | Jle  of register * register * label * label
  | Jz of register * label * label
  
  

type cfg = (label, instruction) Hashtbl.t  ;;


type function_def = {
  name : string;
  params: register list;
  result: register;
  locals: register list;
  entry : label;
  exit : label;
  body : cfg;
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

let print_instruction fmt i =
  match i with 
  | RCst (n,r,l) -> Format.fprintf fmt "mov $%d %a  --> %a" n print_register r  print_label l
  | RBool (b,r,l) -> Format.fprintf fmt "mov $%b %a  --> %a" b print_register r  print_label l
  | RMove (r1, r2, l) -> Format.fprintf fmt "mov %a %a  --> %a" print_register r1 print_register r2  print_label l 
  | RBinop (b, r1, r2, l) -> Format.fprintf fmt "binop %a %a %a  --> %a" Imp.print_binop b print_register r1 print_register r2  print_label l
  | RCall (r, f, args, l) -> Format.fprintf fmt "%a <- call %s(@[%a@])  --> %a" print_register r f (Utils.print_list print_register) args  print_label l
  | RSet (r1, r2, l) -> Format.fprintf fmt "mov %a %a  --> %a" print_register r1 print_register r2  print_label l
  | RIf (r, l1, l2) -> Format.fprintf fmt "RIf(%a, %a, %a)" print_register r print_label l1 print_label l2
  | RPutchar (r, l) -> Format.fprintf fmt "RPutchar(%a, %a)" print_register r print_label l
  | RWhile (r, l1, l2) -> Format.fprintf fmt "RWhile(%a, %a, %a)" print_register r print_label l1 print_label l2
  | RGoto l -> Format.fprintf fmt "goto %d" l
  | Jle (r1, r2, l1, l2) -> Format.fprintf fmt "jle %a %a %a %a" print_register r1 print_register r2 print_label l1 print_label l2
  | Jz (r, l1, l2) -> Format.fprintf fmt "jz %a %a %a" print_register r print_label l1 print_label l2


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
  print_cfg fmt f.body ;
  Format.fprintf  fmt "@]@."
let print_program fmt p =
  Format.fprintf fmt "globals: @[%a@]@\n@\n%a"
    (Utils.print_list Format.pp_print_string) p.globals
    (Utils.print_list print_function_def) p.functions
    
  


