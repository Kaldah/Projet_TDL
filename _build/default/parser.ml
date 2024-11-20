
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
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
    | ID of (
# 11 "parser.mly"
       (string)
# 31 "parser.ml"
  )
    | FALSE
    | EQUAL
    | EOF
    | ENTIER of (
# 10 "parser.mly"
       (int)
# 39 "parser.ml"
  )
    | ELSE
    | DENOM
    | CONST
    | CO
    | CF
    | BOOL
    | AO
    | AF
  
end

include MenhirBasics

# 3 "parser.mly"
  

open Type
open Ast.AstSyntax

# 60 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState06 : (('s, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID, _menhir_box_main) _menhir_state
    (** State 06.
        Stack shape : typ ID.
        Start symbol: main. *)

  | MenhirState11 : (('s, _menhir_box_main) _menhir_cell1_param, _menhir_box_main) _menhir_state
    (** State 11.
        Stack shape : param.
        Start symbol: main. *)

  | MenhirState14 : ((('s, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID, _menhir_box_main) _menhir_cell1_loption_separated_nonempty_list_VIRG_param__, _menhir_box_main) _menhir_state
    (** State 14.
        Stack shape : typ ID loption(separated_nonempty_list(VIRG,param)).
        Start symbol: main. *)

  | MenhirState15 : (('s, _menhir_box_main) _menhir_cell1_AO, _menhir_box_main) _menhir_state
    (** State 15.
        Stack shape : AO.
        Start symbol: main. *)

  | MenhirState16 : (('s, _menhir_box_main) _menhir_cell1_WHILE, _menhir_box_main) _menhir_state
    (** State 16.
        Stack shape : WHILE.
        Start symbol: main. *)

  | MenhirState18 : (('s, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_state
    (** State 18.
        Stack shape : PO.
        Start symbol: main. *)

  | MenhirState19 : (('s, _menhir_box_main) _menhir_cell1_NUM, _menhir_box_main) _menhir_state
    (** State 19.
        Stack shape : NUM.
        Start symbol: main. *)

  | MenhirState21 : (('s, _menhir_box_main) _menhir_cell1_ID, _menhir_box_main) _menhir_state
    (** State 21.
        Stack shape : ID.
        Start symbol: main. *)

  | MenhirState24 : (('s, _menhir_box_main) _menhir_cell1_DENOM, _menhir_box_main) _menhir_state
    (** State 24.
        Stack shape : DENOM.
        Start symbol: main. *)

  | MenhirState25 : (('s, _menhir_box_main) _menhir_cell1_CO, _menhir_box_main) _menhir_state
    (** State 25.
        Stack shape : CO.
        Start symbol: main. *)

  | MenhirState27 : ((('s, _menhir_box_main) _menhir_cell1_CO, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 27.
        Stack shape : CO e.
        Start symbol: main. *)

  | MenhirState35 : (('s, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 35.
        Stack shape : e.
        Start symbol: main. *)

  | MenhirState39 : ((('s, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 39.
        Stack shape : PO e.
        Start symbol: main. *)

  | MenhirState43 : ((('s, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 43.
        Stack shape : PO e.
        Start symbol: main. *)

  | MenhirState46 : ((('s, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 46.
        Stack shape : PO e.
        Start symbol: main. *)

  | MenhirState49 : ((('s, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 49.
        Stack shape : PO e.
        Start symbol: main. *)

  | MenhirState52 : ((('s, _menhir_box_main) _menhir_cell1_WHILE, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 52.
        Stack shape : WHILE e.
        Start symbol: main. *)

  | MenhirState54 : (('s, _menhir_box_main) _menhir_cell1_RETURN, _menhir_box_main) _menhir_state
    (** State 54.
        Stack shape : RETURN.
        Start symbol: main. *)

  | MenhirState57 : (('s, _menhir_box_main) _menhir_cell1_PRINT, _menhir_box_main) _menhir_state
    (** State 57.
        Stack shape : PRINT.
        Start symbol: main. *)

  | MenhirState60 : (('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_state
    (** State 60.
        Stack shape : IF.
        Start symbol: main. *)

  | MenhirState61 : ((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_state
    (** State 61.
        Stack shape : IF e.
        Start symbol: main. *)

  | MenhirState63 : (((('s, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_cell1_bloc, _menhir_box_main) _menhir_state
    (** State 63.
        Stack shape : IF e bloc.
        Start symbol: main. *)

  | MenhirState66 : (('s, _menhir_box_main) _menhir_cell1_ID, _menhir_box_main) _menhir_state
    (** State 66.
        Stack shape : ID.
        Start symbol: main. *)

  | MenhirState76 : (('s, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID, _menhir_box_main) _menhir_state
    (** State 76.
        Stack shape : typ ID.
        Start symbol: main. *)

  | MenhirState81 : (('s, _menhir_box_main) _menhir_cell1_i, _menhir_box_main) _menhir_state
    (** State 81.
        Stack shape : i.
        Start symbol: main. *)

  | MenhirState88 : (('s, _menhir_box_main) _menhir_cell1_list_fonc_ _menhir_cell0_ID, _menhir_box_main) _menhir_state
    (** State 88.
        Stack shape : list(fonc) ID.
        Start symbol: main. *)

  | MenhirState90 : (('s, _menhir_box_main) _menhir_cell1_fonc, _menhir_box_main) _menhir_state
    (** State 90.
        Stack shape : fonc.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_bloc = 
  | MenhirCell1_bloc of 's * ('s, 'r) _menhir_state * (Ast.AstSyntax.bloc)

and ('s, 'r) _menhir_cell1_e = 
  | MenhirCell1_e of 's * ('s, 'r) _menhir_state * (Ast.AstSyntax.expression)

and ('s, 'r) _menhir_cell1_fonc = 
  | MenhirCell1_fonc of 's * ('s, 'r) _menhir_state * (Ast.AstSyntax.fonction)

and ('s, 'r) _menhir_cell1_i = 
  | MenhirCell1_i of 's * ('s, 'r) _menhir_state * (Ast.AstSyntax.instruction)

and ('s, 'r) _menhir_cell1_list_fonc_ = 
  | MenhirCell1_list_fonc_ of 's * ('s, 'r) _menhir_state * (Ast.AstSyntax.fonction list)

and ('s, 'r) _menhir_cell1_loption_separated_nonempty_list_VIRG_param__ = 
  | MenhirCell1_loption_separated_nonempty_list_VIRG_param__ of 's * ('s, 'r) _menhir_state * ((Type.typ * string) list)

and ('s, 'r) _menhir_cell1_param = 
  | MenhirCell1_param of 's * ('s, 'r) _menhir_state * (Type.typ * string)

and ('s, 'r) _menhir_cell1_typ = 
  | MenhirCell1_typ of 's * ('s, 'r) _menhir_state * (Type.typ)

and ('s, 'r) _menhir_cell1_AO = 
  | MenhirCell1_AO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CO = 
  | MenhirCell1_CO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DENOM = 
  | MenhirCell1_DENOM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ID = 
  | MenhirCell1_ID of 's * ('s, 'r) _menhir_state * (
# 11 "parser.mly"
       (string)
# 241 "parser.ml"
)

and 's _menhir_cell0_ID = 
  | MenhirCell0_ID of 's * (
# 11 "parser.mly"
       (string)
# 248 "parser.ml"
)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NUM = 
  | MenhirCell1_NUM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PO = 
  | MenhirCell1_PO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRINT = 
  | MenhirCell1_PRINT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Ast.AstSyntax.programme) [@@unboxed]

let _menhir_action_01 =
  fun li ->
    (
# 62 "parser.mly"
                        (li)
# 277 "parser.ml"
     : (Ast.AstSyntax.bloc))

let _menhir_action_02 =
  fun n xs ->
    let lp = 
# 241 "<standard.mly>"
    ( xs )
# 285 "parser.ml"
     in
    (
# 79 "parser.mly"
                                         (AppelFonction (n,lp))
# 290 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_03 =
  fun e1 e2 ->
    (
# 80 "parser.mly"
                          (Binaire(Fraction,e1,e2))
# 298 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_04 =
  fun n ->
    (
# 81 "parser.mly"
                          (Ident n)
# 306 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_05 =
  fun () ->
    (
# 82 "parser.mly"
                          (Booleen true)
# 314 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_06 =
  fun () ->
    (
# 83 "parser.mly"
                          (Booleen false)
# 322 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_07 =
  fun e ->
    (
# 84 "parser.mly"
                          (Entier e)
# 330 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_08 =
  fun e1 ->
    (
# 85 "parser.mly"
                          (Unaire(Numerateur,e1))
# 338 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_09 =
  fun e1 ->
    (
# 86 "parser.mly"
                          (Unaire(Denominateur,e1))
# 346 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_10 =
  fun e1 e2 ->
    (
# 87 "parser.mly"
                          (Binaire (Plus,e1,e2))
# 354 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_11 =
  fun e1 e2 ->
    (
# 88 "parser.mly"
                          (Binaire (Mult,e1,e2))
# 362 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_12 =
  fun e1 e2 ->
    (
# 89 "parser.mly"
                          (Binaire (Equ,e1,e2))
# 370 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_13 =
  fun e1 e2 ->
    (
# 90 "parser.mly"
                          (Binaire (Inf,e1,e2))
# 378 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_14 =
  fun exp ->
    (
# 91 "parser.mly"
                          (exp)
# 386 "parser.ml"
     : (Ast.AstSyntax.expression))

let _menhir_action_15 =
  fun li n t xs ->
    let lp = 
# 241 "<standard.mly>"
    ( xs )
# 394 "parser.ml"
     in
    (
# 58 "parser.mly"
                                                              (Fonction(t,n,lp,li))
# 399 "parser.ml"
     : (Ast.AstSyntax.fonction))

let _menhir_action_16 =
  fun e1 n t ->
    (
# 65 "parser.mly"
                                    (Declaration (t,n,e1))
# 407 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_17 =
  fun e1 n ->
    (
# 66 "parser.mly"
                                    (Affectation (n,e1))
# 415 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_18 =
  fun e n ->
    (
# 67 "parser.mly"
                                    (Constante (n,e))
# 423 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_19 =
  fun e1 ->
    (
# 68 "parser.mly"
                                    (Affichage (e1))
# 431 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_20 =
  fun exp li1 li2 ->
    (
# 69 "parser.mly"
                                    (Conditionnelle (exp,li1,li2))
# 439 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_21 =
  fun exp li ->
    (
# 70 "parser.mly"
                                    (TantQue (exp,li))
# 447 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_22 =
  fun exp ->
    (
# 71 "parser.mly"
                                    (Retour (exp))
# 455 "parser.ml"
     : (Ast.AstSyntax.instruction))

let _menhir_action_23 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 463 "parser.ml"
     : (Ast.AstSyntax.fonction list))

let _menhir_action_24 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 471 "parser.ml"
     : (Ast.AstSyntax.fonction list))

let _menhir_action_25 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 479 "parser.ml"
     : (Ast.AstSyntax.bloc))

let _menhir_action_26 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 487 "parser.ml"
     : (Ast.AstSyntax.bloc))

let _menhir_action_27 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 495 "parser.ml"
     : (Ast.AstSyntax.expression list))

let _menhir_action_28 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 503 "parser.ml"
     : (Ast.AstSyntax.expression list))

let _menhir_action_29 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 511 "parser.ml"
     : ((Type.typ * string) list))

let _menhir_action_30 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 519 "parser.ml"
     : ((Type.typ * string) list))

let _menhir_action_31 =
  fun lfi ->
    (
# 54 "parser.mly"
                        (lfi)
# 527 "parser.ml"
     : (Ast.AstSyntax.programme))

let _menhir_action_32 =
  fun n t ->
    (
# 60 "parser.mly"
                    ((t,n))
# 535 "parser.ml"
     : (Type.typ * string))

let _menhir_action_33 =
  fun lf li ->
    (
# 56 "parser.mly"
                            (Programme (lf,li))
# 543 "parser.ml"
     : (Ast.AstSyntax.programme))

let _menhir_action_34 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 551 "parser.ml"
     : (Ast.AstSyntax.expression list))

let _menhir_action_35 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 559 "parser.ml"
     : (Ast.AstSyntax.expression list))

let _menhir_action_36 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 567 "parser.ml"
     : ((Type.typ * string) list))

let _menhir_action_37 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 575 "parser.ml"
     : ((Type.typ * string) list))

let _menhir_action_38 =
  fun () ->
    (
# 74 "parser.mly"
          (Bool)
# 583 "parser.ml"
     : (Type.typ))

let _menhir_action_39 =
  fun () ->
    (
# 75 "parser.mly"
          (Int)
# 591 "parser.ml"
     : (Type.typ))

let _menhir_action_40 =
  fun () ->
    (
# 76 "parser.mly"
          (Rat)
# 599 "parser.ml"
     : (Type.typ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AF ->
        "AF"
    | AO ->
        "AO"
    | BOOL ->
        "BOOL"
    | CF ->
        "CF"
    | CO ->
        "CO"
    | CONST ->
        "CONST"
    | DENOM ->
        "DENOM"
    | ELSE ->
        "ELSE"
    | ENTIER _ ->
        "ENTIER"
    | EOF ->
        "EOF"
    | EQUAL ->
        "EQUAL"
    | FALSE ->
        "FALSE"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | INF ->
        "INF"
    | INT ->
        "INT"
    | MULT ->
        "MULT"
    | NUM ->
        "NUM"
    | PF ->
        "PF"
    | PLUS ->
        "PLUS"
    | PO ->
        "PO"
    | PRINT ->
        "PRINT"
    | PV ->
        "PV"
    | RAT ->
        "RAT"
    | RETURN ->
        "RETURN"
    | SLASH ->
        "SLASH"
    | TRUE ->
        "TRUE"
    | VIRG ->
        "VIRG"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_89 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_list_fonc_ _menhir_cell0_ID -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      let MenhirCell0_ID (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_list_fonc_ (_menhir_stack, _, lf) = _menhir_stack in
      let li = _v in
      let _v = _menhir_action_33 lf li in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let lfi = _v in
          let _v = _menhir_action_31 lfi in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_40 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState81 ->
          _menhir_run_74 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_74 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState90 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_74 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL ->
              let _menhir_s = MenhirState76 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PO ->
                  _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NUM ->
                  _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ID _v ->
                  _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FALSE ->
                  _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ENTIER _v ->
                  _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | DENOM ->
                  _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CO ->
                  _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_e : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState76 ->
          _menhir_run_77 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState66 ->
          _menhir_run_67 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState60 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState57 ->
          _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState54 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState49 ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState46 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState43 ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState39 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState18 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState35 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState24 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState27 ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_77 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_ID (_menhir_stack, n) = _menhir_stack in
          let MenhirCell1_typ (_menhir_stack, _menhir_s, t) = _menhir_stack in
          let e1 = _v in
          let _v = _menhir_action_16 e1 n t in
          _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_i : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_i (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | RETURN ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | RAT ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | PRINT ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | INT ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | IF ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | ID _v_0 ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState81
      | CONST ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | BOOL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState81
      | AF ->
          let _v_1 = _menhir_action_25 () in
          _menhir_run_82 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState16 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState18 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NUM (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState19 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PO ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState21 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PF ->
              let _v = _menhir_action_27 () in
              _menhir_goto_loption_separated_nonempty_list_VIRG_e__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR ())
      | AO | CF | EQUAL | INF | MULT | PF | PLUS | PV | SLASH | VIRG ->
          let n = _v in
          let _v = _menhir_action_04 n in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_06 () in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_23 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let e = _v in
      let _v = _menhir_action_07 e in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_24 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DENOM (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState24 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_25 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState25 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_VIRG_e__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ID (_menhir_stack, _menhir_s, n) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_02 n xs in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_54 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState54 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_57 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRINT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState57 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_39 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_60 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState60 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PO ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NUM ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ENTIER _v ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DENOM ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CO ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_65 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EQUAL ->
          let _menhir_s = MenhirState66 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_69 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ENTIER _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | PV ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (n, e) = (_v, _v_0) in
                      let _v = _menhir_action_18 e n in
                      _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_38 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_82 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_i -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_i (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_26 x xs in
      _menhir_goto_list_i_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_i_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState81 ->
          _menhir_run_82 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState15 ->
          _menhir_run_79 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_79 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_AO -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_AO (_menhir_stack, _menhir_s) = _menhir_stack in
      let li = _v in
      let _v = _menhir_action_01 li in
      _menhir_goto_bloc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bloc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState88 ->
          _menhir_run_89 _menhir_stack _v _tok
      | MenhirState14 ->
          _menhir_run_83 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState63 ->
          _menhir_run_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState61 ->
          _menhir_run_62 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState52 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_83 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID, _menhir_box_main) _menhir_cell1_loption_separated_nonempty_list_VIRG_param__ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_loption_separated_nonempty_list_VIRG_param__ (_menhir_stack, _, xs) = _menhir_stack in
      let MenhirCell0_ID (_menhir_stack, n) = _menhir_stack in
      let MenhirCell1_typ (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let li = _v in
      let _v = _menhir_action_15 li n t xs in
      let _menhir_stack = MenhirCell1_fonc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RAT ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState90
      | INT ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState90
      | BOOL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState90
      | ID _ ->
          let _v_0 = _menhir_action_23 () in
          _menhir_run_91 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_91 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_fonc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_fonc (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_24 x xs in
      _menhir_goto_list_fonc_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_fonc_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState90 ->
          _menhir_run_91 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_87 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_87 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_fonc_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v_0) in
          let _menhir_s = MenhirState88 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | AO ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AO (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | RETURN ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | RAT ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | PRINT ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | INT ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | IF ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | ID _v ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15
      | CONST ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | BOOL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | AF ->
          let _v = _menhir_action_25 () in
          _menhir_run_79 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_64 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_e, _menhir_box_main) _menhir_cell1_bloc -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_bloc (_menhir_stack, _, li1) = _menhir_stack in
      let MenhirCell1_e (_menhir_stack, _, exp) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let li2 = _v in
      let _v = _menhir_action_20 exp li1 li2 in
      _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_62 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_IF, _menhir_box_main) _menhir_cell1_e as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bloc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState63 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | AO ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_53 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_WHILE, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_e (_menhir_stack, _, exp) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let li = _v in
      let _v = _menhir_action_21 exp li in
      _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_67 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ID (_menhir_stack, _menhir_s, n) = _menhir_stack in
          let e1 = _v in
          let _v = _menhir_action_17 e1 n in
          _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_61 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | AO ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState61
      | _ ->
          _eRR ()
  
  and _menhir_run_58 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PRINT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_PRINT (_menhir_stack, _menhir_s) = _menhir_stack in
          let e1 = _v in
          let _v = _menhir_action_19 e1 in
          _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_55 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
          let exp = _v in
          let _v = _menhir_action_22 exp in
          _menhir_goto_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_52 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | AO ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState52
      | _ ->
          _eRR ()
  
  and _menhir_run_50 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_e (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_PO (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_12 e1 e2 in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_47 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_e (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_PO (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_44 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_e (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_PO (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_11 e1 e2 in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_40 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PO, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | PF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_e (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_PO (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_10 e1 e2 in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_PO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState39 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_PO (_menhir_stack, _menhir_s) = _menhir_stack in
          let exp = _v in
          let _v = _menhir_action_14 exp in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MULT ->
          let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState43 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | INF ->
          let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState46 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState49 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_NUM -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NUM (_menhir_stack, _menhir_s) = _menhir_stack in
      let e1 = _v in
      let _v = _menhir_action_08 e1 in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_34 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VIRG ->
          let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState35 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PF ->
          let x = _v in
          let _v = _menhir_action_34 x in
          _menhir_goto_separated_nonempty_list_VIRG_e_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_VIRG_e_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState35 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState21 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_36 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_e (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_35 x xs in
      _menhir_goto_separated_nonempty_list_VIRG_e_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_31 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_28 x in
      _menhir_goto_loption_separated_nonempty_list_VIRG_e__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_30 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_DENOM -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_DENOM (_menhir_stack, _menhir_s) = _menhir_stack in
      let e1 = _v in
      let _v = _menhir_action_09 e1 in
      _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_28 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_CO, _menhir_box_main) _menhir_cell1_e -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | CF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_e (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_CO (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_03 e1 e2 in
          _menhir_goto_e _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_CO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_e (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SLASH ->
          let _menhir_s = MenhirState27 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PO ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NUM ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ENTIER _v ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DENOM ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CO ->
              _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (n, t) = (_v_0, _v) in
          let _v = _menhir_action_32 n t in
          (match (_tok : MenhirBasics.token) with
          | VIRG ->
              let _menhir_stack = MenhirCell1_param (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState11 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RAT ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | PF ->
              let x = _v in
              let _v = _menhir_action_36 x in
              _menhir_goto_separated_nonempty_list_VIRG_param_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_VIRG_param_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState06 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_param -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_param (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_37 x xs in
      _menhir_goto_separated_nonempty_list_VIRG_param_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let x = _v in
      let _v = _menhir_action_30 x in
      _menhir_goto_loption_separated_nonempty_list_VIRG_param__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_loption_separated_nonempty_list_VIRG_param__ : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_typ _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_loption_separated_nonempty_list_VIRG_param__ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState14 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | AO ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PO ->
              let _menhir_s = MenhirState06 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RAT ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PF ->
                  let _v = _menhir_action_29 () in
                  _menhir_goto_loption_separated_nonempty_list_VIRG_param__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RAT ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | INT ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | BOOL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ID _ ->
          let _v = _menhir_action_23 () in
          _menhir_run_87 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
