open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../projet_Tests/gestion_id/avec_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(*************)
(* POINTEURS *)
(*************)
let%test_unit "testBdeclaF"= 
  let _ = compiler (pathFichiersRat^"testBdeclaF.rat") in ()

let%test_unit "testDDPointeur1"= 
  let _ = compiler (pathFichiersRat^"testDDPointeur1.rat") in ()

let%test_unit "testDDVar2" = 
  let _ = compiler (pathFichiersRat^"testDDVar2.rat") in ()

(*a confirmer et a verifier*)
(* a reutiliser dans la passe de typage car ca plante au typage mais pas ici*)
let%test_unit "testDeclaF1" = 
  let _ = compiler (pathFichiersRat^"testDeclaF1.rat") in ()

let%test_unit "testDeclaF2" = 
  let _ = compiler (pathFichiersRat^"testDeclaF2.rat") in ()

let%test_unit "testDDParam" = 
try 
  let _ = compiler (pathFichiersRat^"testDDParam.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("a") -> ()

let%test_unit "testDDParam1" = 
try 
  let _ = compiler (pathFichiersRat^"testDDParam1.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("a") -> ()

let%test_unit "testDDPointeurF" = 
try 
  let _ = compiler (pathFichiersRat^"testDDPointeurF.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("a") -> ()

let%test_unit "testDDVar" = 
try 
  let _ = compiler (pathFichiersRat^"testDDVar.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("px") -> ()

let%test_unit "testDDVar1" = 
try 
  let _ = compiler (pathFichiersRat^"testDDvar1.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("a") -> ()

let%test_unit "testDDVar3" = 
try 
  let _ = compiler (pathFichiersRat^"testDDVar3.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("px") -> ()

let%test_unit "testDDVar4" = 
try 
  let _ = compiler (pathFichiersRat^"testDDVar4.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("px") -> ()

let%test_unit "testDDVar5" = 
try 
  let _ = compiler (pathFichiersRat^"testDDVar5.rat")
  in raise ErreurNonDetectee
with
| DoubleDeclaration("x") -> ()

let%test_unit "testRetourP" = 
try 
  let _ = compiler (pathFichiersRat^"testRetourP.rat")
  in raise ErreurNonDetectee
with
| RetourDansMain -> ()

(*************)
(*  GLOBALES *)
(*************)


(*************)
(*  LOCALES  *)
(*************)


(*************)
(* DÉFAUTS   *)
(*************)



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
  let p_tam = "../../../../../projet_Tests/tam/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
