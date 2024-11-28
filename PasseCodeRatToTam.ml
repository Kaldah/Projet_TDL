(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)

open Tds
open Ast

open Type

type t1 = Ast.AstPlacement.programme
type t2 = string

let rec analyse_code_expression e =
  match e with
  | AstType.AppelFonction (info, le ) -> ""
  | AstType.Ident info -> ""
  | AstType.Booleen b -> ""
  | AstType.Entier i -> ""
  | AstType.Unaire (op, e1) -> ""
  | AstType.Binaire (b, e1, e2) -> ""
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
    and analyse_code_bloc (li , taille ) = ""

let analyse_code_fonction (AstPlacement.Fonction (info ,_ , ( li , _ ))) = ""

let analyser (AstPlacement.Programme (fonctions, prog)) = ""