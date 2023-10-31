(*Register Transfer Language*)

type label = int


(*Pseudo Register*)
type pseudo_register = 
  | PReg of Mips.register
  | PRegInt of int
  


(*Control Flow Graph*)
(* Le graphe de flot de contrôle peut être représenté par un dictionnaire associant une
instruction RTL à chaque étiquette. *)


type instruction =
(* Integer constant: 0, -1, 42, ... *)
  | RCst   of int * pseudo_register * label
  (* Boolean constant: true, false *)
  | RBool  of bool * pseudo_register * label
  (* Variable, identified by a name *)
  | RMove of pseudo_register * pseudo_register * label
  (* Binary operation, with an operator and two operands *)
  | RBinop of Imp.binop * pseudo_register * pseudo_register * label 
  (* Function call, with a function name and a list of parameters *)
  | RCall  of pseudo_register * string * pseudo_register list * label
  (* Assignment of a new value to a variable *)
  | RSet     of  pseudo_register * pseudo_register * label
  (* Conditional *)
  | RIf      of  pseudo_register * label * label
  | RPutchar of pseudo_register * label
  (* Loop *)
  | RWhile   of pseudo_register * label * label
  | RGoto of label 
  | Jle  of pseudo_register * pseudo_register * label * label
  | Jz of pseudo_register * label * label
  
  

type cfg = (label, instruction) Hashtbl.t  ;;


type function_def = {
  name : string;
  params: pseudo_register list;
  result: pseudo_register;
  locals: pseudo_register list;
  entry : label;
  exit : label;
  body : cfg;
}

type program = {
  globals: string list;
  functions: function_def list;
}



(** Print RTL instructions *)

let print_pseudo_register fmt r =
  match r with
  | PReg r -> Format.fprintf fmt "%a" Mips.print_register r
  | PRegInt i -> Format.fprintf fmt "%d" i

let print_instruction fmt i =
  match i with 
  | RCst (i, r, l) -> Format.fprintf fmt "RCst(%d, %a, %d)" i print_pseudo_register r l
  | RBool (b, r, l) -> Format.fprintf fmt "RBool(%b, %a, %d)" b print_pseudo_register r l
  | RMove (r1, r2, l) -> Format.fprintf fmt "RMove(%a, %a, %d)" print_pseudo_register r1 print_pseudo_register r2 l
  | RBinop (b, r1, r2, l) -> Format.fprintf fmt "RBinop(%a, %a, %a, %d)" Imp.print_binop b print_pseudo_register r1 print_pseudo_register r2 l
  | RCall (r, s, rl, l) -> Format.fprintf fmt "%a <- call %s(@[%a@])  --> %d" print_pseudo_register r s (Utils.print_list print_pseudo_register) rl l
  | RSet (r1, r2, l) -> Format.fprintf fmt "RSet(%a, %a, %d)" print_pseudo_register r1 print_pseudo_register r2 l
  | RIf (r, l1, l2) -> Format.fprintf fmt "RIf(%a, %d, %d)" print_pseudo_register r l1 l2
  | RPutchar (r, l) -> Format.fprintf fmt "RPutchar(%a, %d)" print_pseudo_register r l
  | RWhile (r, l1, l2) -> Format.fprintf fmt "RWhile(%a, %d, %d)" print_pseudo_register r l1 l2
  | RGoto l -> Format.fprintf fmt "RGoto(%d)" l
  | Jle (r1, r2, l1, l2) -> Format.fprintf fmt "Jle(%a, %a, %d, %d)" print_pseudo_register r1 print_pseudo_register r2 l1 l2
  | Jz (r, l1, l2) -> Format.fprintf fmt "Jz(%a, %d, %d)" print_pseudo_register r l1 l2


let print_cfg fmt cfg =
  let print_instruction fmt (l, i) =
    Format.fprintf fmt "%d: %a" l print_instruction i
  in
  Format.fprintf fmt "{@[<v 0>%a@]}"
    (Utils.print_list print_instruction) (Hashtbl.to_seq cfg |> List.of_seq)


let print_function_def fmt f =
  Format.fprintf fmt "function %s(@[%a@]) returns %a {@\n%a@\n}"
    f.name
    (Utils.print_list print_pseudo_register) f.params
    print_pseudo_register f.result
    print_cfg f.body

let print_program fmt p =
  Format.fprintf fmt "globals: @[%a@]@\n@\n%a"
    (Utils.print_list Format.pp_print_string) p.globals
    (Utils.print_list print_function_def) p.functions
    
  


