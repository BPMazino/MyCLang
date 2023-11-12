
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYP_VOID
  | TYP_INT
  | TYP_BOOL
  | THIS
  | STAR
  | SET
  | SEMI
  | RPAR
  | RETURN
  | RBRACKET
  | PUTCHAR
  | PLUS
  | NEW
  | METHOD
  | LT
  | LPAR
  | LBRACKET
  | IF
  | IDENT of (string)
  | FUNCTION
  | EXTENDS
  | EOF
  | END
  | ELSE
  | DOT
  | CST of (int)
  | COMMA
  | CLASS
  | BOOL of (bool)
  | BEGIN
  | ATTRIBUTE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Objlng.loc Objlng.program)
