open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../Projets_Tests/type/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(*************)
(* POINTEURS *)
(*************)

(*
Vérifier que le type de la variable pointée correspond au type défini.
Tester les opérations invalides (e.g., déréférencer une valeur non pointeur).
*)

(*************)
(*  GLOBALES *)
(*************)

(*
S’assurer que le type des variables globales est correct et compatible avec leurs usages.
*)

(*************)
(*  LOCALES  *)
(*************)

(*
Valider le typage statique local, avec conservation des valeurs entre appels.
Tester l’incompatibilité entre le type défini et les affectations ultérieures.
*)

(*************)
(* DÉFAUTS   *)
(*************)

(*
Vérifier que les valeurs par défaut respectent le type déclaré.
Tester les cas où des paramètres sont omis lors de l’appel.
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