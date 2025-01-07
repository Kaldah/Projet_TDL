open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../projet_Tests/type/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(*************)
(* POINTEURS *)
(*************)

let%test_unit "testPointeur"= 
  let _ = compiler (pathFichiersRat^"testPointeur0.rat") in ()

let%test_unit "testBaffectation1"= 
  let _ = compiler (pathFichiersRat^"testBaffectation1.rat") in ()

let%test_unit "testBaffectation2"= 
  let _ = compiler (pathFichiersRat^"testBaffectation2.rat") in ()

let%test_unit "testBconditionelle1"= 
  let _ = compiler (pathFichiersRat^"testBconditionnelle1.rat") in ()

let%test_unit "testBconditionelle2"= 
  let _ = compiler (pathFichiersRat^"testBconditionnelle2.rat") in ()

let%test_unit "testBconditionelle3"= 
  let _ = compiler (pathFichiersRat^"testBconditionnelle3.rat") in ()

let%test_unit "testBdeclaration1"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration1.rat") in ()

let%test_unit "testBdeclaration2"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration2.rat") in ()

let%test_unit "testBdeclaration3"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration3.rat") in ()

let%test_unit "testBdeclaration4"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration4.rat") in ()

let%test_unit "testBdeclaration5"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration5.rat") in ()

let%test_unit "testBdeclaration6"= 
  let _ = compiler (pathFichiersRat^"testBdeclaration6.rat") in ()

let%test_unit "testBdenominateur"= 
  let _ = compiler (pathFichiersRat^"testBdenominateur.rat") in ()

  let%test_unit "testBident"= 
  let _ = compiler (pathFichiersRat^"testBident.rat") in ()

  let%test_unit "testBident1"= 
  let _ = compiler (pathFichiersRat^"testBident1.rat") in ()

  let%test_unit "testBident2"= 
  let _ = compiler (pathFichiersRat^"testBident2.rat") in ()

  let%test_unit "testBnumerateur"= 
  let _ = compiler (pathFichiersRat^"testBnumerateur.rat") in ()

  let%test_unit "testBnumerateur2"= 
  let _ = compiler (pathFichiersRat^"testBnumerateur2.rat") in ()

  let%test_unit "testBoperation"= 
  let _ = compiler (pathFichiersRat^"testBoperation.rat") in ()

  let%test_unit "testBoperation1"= 
  let _ = compiler (pathFichiersRat^"testBoperation1.rat") in ()

  let%test_unit "testBprint"= 
  let _ = compiler (pathFichiersRat^"testBprint.rat") in ()

  let%test_unit "testBprint1"= 
  let _ = compiler (pathFichiersRat^"testBprint1.rat") in ()

  let%test_unit "testBprint2"= 
  let _ = compiler (pathFichiersRat^"testBprint2.rat") in ()

let%test_unit "testDeclaF1" = 
try 
  let _ = compiler (pathFichiersRat^"testDeclaF1.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Int, Pointeur Rat) -> ()

let%test_unit "testDenominateur" = 
try 
  let _ = compiler (pathFichiersRat^"testDenominateur.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Int, Rat) -> ()

let%test_unit "testMaffectation" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur( (Pointeur Rat)), (Pointeur Rat)) -> ()

let%test_unit "testMaffectation1" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation1.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Rat, Int) -> ()

let%test_unit "testMaffectation3" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation3.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Rat, Bool) -> ()

let%test_unit "testMaffectation4" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation4.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur Bool) -> ()

let%test_unit "testMaffectation5" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation5.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Rat, Pointeur Rat) -> ()

let%test_unit "testMaffectation6" = 
try 
  let _ = compiler (pathFichiersRat^"testMaffectation6.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Bool, Pointeur (Pointeur Int)) -> ()

let%test_unit "testMappelfonction1" = 
try 
  let _ = compiler (pathFichiersRat^"testMappelfonction1.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Bool, Pointeur (Pointeur Bool)) -> ()

let%test_unit "testMconditionnelle" = 
try 
  let _ = compiler (pathFichiersRat^"testMconditionnelle.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Equ,Pointeur _, Int) -> ()

let%test_unit "testMconditionnelle2" = 
try 
  let _ = compiler (pathFichiersRat^"testMconditionnelle2.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Equ,(Pointeur Int),Bool) -> ()

let%test_unit "testMdeclaPointeurInt3" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaPointeurInt3.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur Int) -> ()

let%test_unit "testMdeclaration1" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration1.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Int, Pointeur Rat) -> ()

let%test_unit "testMdeclaration2" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration2.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur Int) -> ()

(*a verifier et a confirmer*)

let%test_unit "testMdeclaration3" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration3.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur (Pointeur Int), Pointeur(Pointeur Rat)) -> ()

let%test_unit "testMdeclaration4" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration4.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur (Pointeur Rat), Pointeur(Pointeur Int)) -> ()

let%test_unit "testMdeclaration5" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration5.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Null, Rat) -> ()

let%test_unit "testMdeclaration6" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaration6.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Null, Int) -> ()

let%test_unit "testMdenominateur" = 
try 
  let _ = compiler (pathFichiersRat^"testMdenominateur.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Bool, Rat) -> ()

let%test_unit "testMident" = 
try 
  let _ = compiler (pathFichiersRat^"testMident.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Int, Pointeur Rat) -> ()

let%test_unit "testMident1" = 
try 
  let _ = compiler (pathFichiersRat^"testMident1.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Bool, Pointeur Int) -> ()

let%test_unit "testMident3" = 
try 
  let _ = compiler (pathFichiersRat^"testMident3.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur Bool) -> ()

let%test_unit "testMident4" = 
try 
  let _ = compiler (pathFichiersRat^"testMident4.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Pointeur(Pointeur Rat)) -> ()

let%test_unit "testMnumerateur" = 
try 
  let _ = compiler (pathFichiersRat^"testMnumerateur.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Bool, Rat) -> ()

let%test_unit "testMnumerateur2" = 
try 
  let _ = compiler (pathFichiersRat^"testMnumerateur2.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Pointeur Rat, Rat) -> ()

let%test_unit "testMoperation" = 
try 
  let _ = compiler (pathFichiersRat^"testMoperation.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Plus, Pointeur Int, Pointeur Int) -> ()

let%test_unit "testMoperation2" = 
try 
  let _ = compiler (pathFichiersRat^"testMoperation2.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Inf, Pointeur Rat, Pointeur Rat) -> ()

let%test_unit "testMoperation3" = 
try 
  let _ = compiler (pathFichiersRat^"testMoperation3.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Inf, Int, Rat) -> ()

let%test_unit "testMoperation4" = 
try 
  let _ = compiler (pathFichiersRat^"testMoperation4.rat")
  in raise ErreurNonDetectee
with
| TypeBinaireInattendu(Inf, Pointeur(Pointeur Int), Pointeur Int) -> ()





(*
Vérifier que le type de la variable pointée correspond au type défini.
Tester les opérations invalides (e.g., déréférencer une valeur non pointeur).
*)

(*************)
(*  GLOBALES *)
(*************)

let%test_unit "testVarglobal1"= 
let _= compiler (pathFichiersRat^"testVarglobal1.rat") in ()

let%test_unit "testVarglobal2" = 
try 
  let _ = compiler (pathFichiersRat^"testVarglobal2.rat")
  in raise ErreurNonDetectee
with
| TypeInattendu(Int,  (Pointeur Rat)) -> ()

let%test_unit "testVarglobal3"= 
let _= compiler (pathFichiersRat^"testVarglobal3.rat") in ()

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


let%test_unit "testParamDef1" = 
  let _ = compiler (pathFichiersRat^"testParamDef1.rat") in ()

let%test_unit "testParamDef2" = 
  let _ = compiler (pathFichiersRat^"testParamDef2.rat") in ()



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
  let p_tam = "../../../../projet_Tests/tam/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam