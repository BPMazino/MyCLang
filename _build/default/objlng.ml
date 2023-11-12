(**
   Annotated abstract syntax for the OBJEling language.
 *)

module Env = Map.Make(String)

 type loc = 
  { fc  : Lexing.position
  ; lc  : Lexing.position}


(* Types of SIMP values *)
type typ =
  | TInt 
  | TBool
  | TClass of string (* class type, identified by its name *)
  | TArray of typ    (* array containing elements of the specified type *)
  | TVoid (* not an actual type in the source language, but having it in
             the AST makes the code more uniform *)

type binop = Imp.binop

type 'a expression = {
  annot: 'a;
  expr: 'a expr;
}
and 'a expr =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * 'a expression * 'a expression
  | Call  of string * 'a expression list
  | MCall of 'a expression * string * 'a expression list
  | New   of string * 'a expression list (* create an instance and call the constructor *)
  | NewTab of typ * 'a expression (* create an array of the given type and size *)
  | Read  of 'a mem               (* read in memory *)
  | This (* current object *)
and 'a mem =
  | Arr of 'a expression * 'a expression (* array access     e1[e2]  *)
  | Atr of 'a expression * string        (* attribute access  o.x    *)

let mk_expr a e = { annot=a; expr=e }

type 'a instruction =
  | Putchar of 'a expression
  | Set     of string * 'a expression
  | If      of 'a expression * 'a sequence * 'a sequence
  | While   of 'a expression * 'a sequence
  | Return  of 'a expression
  | Expr    of 'a expression
  | Write   of 'a mem * 'a expression (*   m = e;   *)
and 'a sequence = 'a instruction list

(* Function definition *)
type 'a function_def = {
  name:   string;
  params: (string * typ) list;
  locals: (string * typ) list;
  code:   'a sequence;
  return: typ;
}

(* Class definition *)
type 'a class_def = {
  name:   string;
  fields: (string * typ) list;
  methods: 'a function_def list;
  parent: string option;
  loc : loc;
}

(* Program as in IMP + types + user-defined  *)
type 'a program = {
  globals:   (string * typ) list;
  functions: 'a function_def list;
  classes:   'a class_def list;
}



let type_size = function
  |TVoid -> 0
  | _ -> 4



let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TClass s -> s
  | TArray t -> (string_of_typ t) ^ "[]"
  | TVoid -> "void"

type method_info = {
    class_name : string;
    offset : int;
 }
type class_info = {
   size : int;
   methods : method_info Env.t ;
   class_def_info : typ class_def
 }

let print_binop = function
  | Imp.Add -> "+"
  | Imp.Lt  -> "<"
  | Imp.Mul -> "*"

 let rec print_expression e =
  match e.expr with
  | Cst n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var x -> x
  | Binop (op, e1, e2) ->
    "(" ^ print_expression e1 ^ " " ^ print_binop op ^ " " ^ print_expression e2 ^ ")"
  | Call (f, args) ->
    f ^ "(" ^ String.concat ", " (List.map print_expression args) ^ ")"
  | MCall (e, f, args) ->
    "(" ^ print_expression e ^ ")." ^ f ^ "(" ^ String.concat ", " (List.map print_expression args) ^ ")"
  | New (c, args) ->
    "new " ^ c ^ "(" ^ String.concat ", " (List.map print_expression args) ^ ")"
  | NewTab (t, e) ->
    "new " ^ string_of_typ t ^ "[" ^ print_expression e ^ "]"
  | Read m -> print_mem m
  | This -> "this"
and print_mem = function
  | Arr (e1, e2) -> print_expression e1 ^ "[" ^ print_expression e2 ^ "]"
  | Atr (e, x) -> print_expression e ^ "." ^ x


