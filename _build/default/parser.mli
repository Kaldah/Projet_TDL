
(* The type of tokens. *)

type token = 
  | WHILE
  | VIRG
  | TRUE
  | SLASH
  | RETURN
  | RAT
  | PV
  | PRINT
  | PO
  | PLUS
  | PF
  | NUM
  | MULT
  | INT
  | INF
  | IF
  | ID of (string)
  | FALSE
  | EQUAL
  | EOF
  | ENTIER of (int)
  | ELSE
  | DENOM
  | CONST
  | CO
  | CF
  | BOOL
  | AO
  | AF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.AstSyntax.programme)
