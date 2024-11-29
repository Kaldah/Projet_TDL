open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../Projets_Tests/gestion_id/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "test"= 
  let _ = compiler (pathFichiersRat^"test.rat") in ()

(*************)
(* POINTEURS *)
(*************)
(* 
Vérifier la déclaration des pointeurs (int *px).
Tester l’accès à l’adresse d’une variable (px = &x).
Tester le déréférencement (int y = *px).
Vérifier l’utilisation du pointeur null (px = null).
*)

(*************)
(*  GLOBALES *)
(*************)

(* 
Valider l’unicité des noms de variables globales.
Vérifier que les variables globales sont accessibles depuis n’importe quelle fonction.
*)

(*************)
(*  LOCALES  *)
(*************)

(*
Vérifier que les noms de variables statiques locales n’entrent pas en conflit avec d’autres variables locales/globales.
S’assurer que leur initialisation est correcte et unique.
*)

(*************)
(* DÉFAUTS   *)
(*************)

(*
Vérifier la présence d’une valeur par défaut pour les paramètres dans les définitions.
S’assurer que les paramètres sans valeur par défaut sont placés avant ceux avec une valeur par défaut.
*)

(* Fichiers de tests de la génération de code -> doivent passer la TDS *)
open Unix
open Filename

let rec test d p_tam = 
  try 
    let file = readdir d in
    if (check_suffix file ".rat") 
    then
    (
     try
       let _ = compiler  (p_tam^file) in (); 
     with e -> print_string (p_tam^file); print_newline(); raise e;
    )
    else ();
    test d p_tam
  with End_of_file -> ()

let%test_unit "all_tam" =
  let p_tam = "../../../../Projets_Tests/tam/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
