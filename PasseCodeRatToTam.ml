(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)

open Tam
open Tds
open Ast

open Type

type t1 = Ast.AstPlacement.programme
type t2 = string

let rec analyse_code_expression e =
  match e with
  | AstType.AppelFonction (info, le ) -> ""
  | AstType.Ident info -> ""
  | AstType.Booleen b -> 
    (
    match b with
    | true -> loadl_int 1
    | false -> loadl_int 0
    )
  | AstType.Entier i -> loadl_int i
  | AstType.Unaire (op, e) -> let code_e = analyse_code_expression e in
    (
      match op with
        | Numerateur -> code_e ^ (pop (0) 1)
        | Denominateur -> code_e ^ (pop (1) 1)
    ) 
  | AstType.Binaire (op, e1, e2) -> 
    let code_e1 = analyse_code_expression e1 in
    let code_e2 = analyse_code_expression e2 in
    let code = code_e1 ^ code_e2 in
  (
    match op with
      | Fraction ->""
      | PlusInt -> ""
      | PlusRat -> ""
      | MultInt -> ""
      | MultRat -> ""
      | EquBool -> ""
      | EquInt -> ""
      | Inf -> code ^ (subr "sub")

  ) 
  | _ -> 
  
  (*
  | AstPlacement.AppelFonction (info, le ) -> ""
  | AstPlacement.Ident info -> ""
  | AstPlacement.Booleen b -> ""
  | AstPlacement.Entier i -> ""
  | AstPlacement.Unaire (op, e1) -> ""
  | AstPlacement.Binaire (b, e1, e2) -> ""
  *)

  let rec analyse_code_instruction i =
    match i with
    | AstPlacement.Declaration ( info , e) -> ""
    | AstPlacement.Affectation ( info , e) -> ""
    | AstPlacement.AffichageInt e -> ""
    | AstPlacement.AffichageRat e -> ""
    | AstPlacement.AffichageBool e -> ""
    | AstPlacement.Conditionnelle (c, t , e) -> ""
    | AstPlacement.TantQue (c, b) -> ""
    | AstPlacement.Retour (e, tailleRet , tailleParam) -> ""
    | AstPlacement.Empty -> ""
    and analyse_code_bloc (li , taille ) = "" in "" ;;

let analyse_code_fonction (AstPlacement.Fonction (info ,_ , ( li , _ ))) = ""

let analyser (AstPlacement.Programme (fonctions, prog)) = ""