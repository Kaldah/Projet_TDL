open Rat
open Tds
open Ast
open PasseTdsRat

(* Définitions de variables pour les tests *)
let tds = creerTDSMere ()

let a = AstSyntax.Ident("a")


(* Exemple de test pour `info_fun` *)

let%test "analyse_gestion_id_affectable1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  ajouter tds "a" ia;
  let res = analyse_gestion_id_affectable tds a true in
  match res with
  | Affectable (AstTds.Ident(n)) -> (n = ia)
  | _ -> false
