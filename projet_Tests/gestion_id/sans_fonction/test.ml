open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../projet_Tests/gestion_id/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(*************)
(* POINTEURS *)
(*************)
(* 
Vérifier la déclaration des pointeurs (int *px).
Tester l’accès à l’adresse d’une variable (px = &x).
Tester le déréférencement (int y = *px).
Vérifier l’utilisation du pointeur null (px = null).
*)
 (* Pour passe typage
let%test_unit "testUtilisation1"= 
  let _ = compiler (pathFichiersRat^"testUtilisation1.rat") in ()
*)
let%test_unit "testUtilisation5"= 
  let _ = compiler (pathFichiersRat^"testUtilisation5.rat") in ()
 (* Pour passe typage
let%test_unit "testUtilisation6"= 
  let _ = compiler (pathFichiersRat^"testUtilisation6.rat") in ()
*)
   (* Pour passe typage
let%test_unit "testBaffectationInt"= 
  let _ = compiler (pathFichiersRat^"testBaffectationInt.rat") in ()
*)
let%test_unit "testBaffectationRat1"= 
  let _ = compiler (pathFichiersRat^"testBaffectationRat1.rat") in ()

  (* Pour passe typage
let%test_unit "testBddecla1"= 
  let _ = compiler (pathFichiersRat^"testBddecla1.rat") in ()
  *)
let%test_unit "testBdeclaPointeurInt"= 
  let _ = compiler (pathFichiersRat^"testBdeclaPointeurInt.rat") in ()

let%test_unit "testBdeclaPointeurInt1"= 
  let _ = compiler (pathFichiersRat^"testBdeclaPointeurInt1.rat") in ()

let%test_unit "testBdeclaPointeurRat"= 
  let _ = compiler (pathFichiersRat^"testBdeclaPointeurRat.rat") in ()

let%test_unit "testUtilisation4"= 
try 
  let _ = compiler (pathFichiersRat^"testUtilisation4.rat") 
  in raise ErreurNonDetectee
with
| MauvaiseUtilisationIdentifiant("a") -> ()

let%test_unit "testEDoubleDeclaration"= 
try 
  let _ = compiler (pathFichiersRat^"testEDoubleDeclaration.rat") 
  in raise ErreurNonDetectee
with
| DoubleDeclaration("px") -> ()

let%test_unit "testEDoubleDeclaration2"= 
try 
  let _ = compiler (pathFichiersRat^"testEDoubleDeclaration2.rat") 
  in raise ErreurNonDetectee
with
| DoubleDeclaration("px") -> ()

let%test_unit "testMaffectationRat1"= 
try 
  let _ = compiler (pathFichiersRat^"testMaffectationRat1.rat") 
  in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("px") -> ()

let%test_unit "testMaffectationRat2"= 
try 
  let _ = compiler (pathFichiersRat^"testMaffectationRat2.rat") 
  in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("z") -> ()

(*reutilisation pour test passe de typage car ca plante

let%test_unit "testMaffectationRat3"=  
  let _ = compiler (pathFichiersRat^"testMaffectationRat3.rat") in ()
*)
let%test_unit "testMaffectationRat4"= 
try 
  let _ = compiler (pathFichiersRat^"testMaffectationRat4.rat") 
  in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("px") -> ()

(*
let%test_unit "testMdeclaraPointeurInt1" = 
  try 
    let _ = compiler (pathFichiersRat^"testMdeclaPointeurInt1.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("px") -> ()

a revoir et a confirmer / a utiliser pour tester la grammaire*)

(*
  let%test_unit "testMdeclaraPointeurInt2" = 
  try 
    let _ = compiler (pathFichiersRat^"testMdeclaPointeurInt2.rat")
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("int") -> ()
*)


  (*a confirmer et a revoir*)
  (*ca passe pour gestion id
  a reutiliser pour la passe de typage car ca plante
  

let%test_unit "testMdeclaPointeurInt3"= 
  let _ = compiler (pathFichiersRat^"testMdeclaPointeurInt3.rat") in ()
*)

let%test_unit "testMdeclaraPointeurInt4" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaPointeurInt4.rat")
  in raise ErreurNonDetectee
with
| MauvaiseUtilisationIdentifiant("x") -> ()

 (*a confirmer et a revoir*)
 (*ca passe pour gestion id
  a reutiliser pour la passe de typage car ca plante
  
let%test_unit "testMdeclaPointeurRat1"= 
 let _ = compiler (pathFichiersRat^"testMdeclaPointeurRat1.rat") in ()
*)
let%test_unit "testMdeclaraPointeurRat2" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaPointeurRat2.rat")
  in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("px") -> ()

(*a revoir et a confirmer / a utiliser pour tester la grammaire*)

(*
  let%test_unit "testMdeclaraPointeurRat2" = 
try 
  let _ = compiler (pathFichiersRat^"testMdeclaPointeurRat2.rat")
  in raise ErreurNonDetectee
with
| IdentifiantNonDeclare("px") -> ()
*)



(*************)
(*  GLOBALES *)
(*************)

(* 
Valider l’unicité des noms de variables globales.
Vérifier que les variables globales sont accessibles depuis n’importe quelle fonction.
*)

let%test_unit "testVarGlobales1"= 
  let _ = compiler (pathFichiersRat^"testVarGlobales1.rat") in ()

let%test_unit "testVarGlobales2"= 
  let _ = compiler (pathFichiersRat^"testVarGlobales2.rat") in ()

let%test_unit "testVarGlobales3"= 
  let _ = compiler (pathFichiersRat^"testVarGlobales3.rat") in ()

let%test_unit "testVarGlobalesNum"= 
  let _ = compiler (pathFichiersRat^"testVarGlobalesNum.rat") in ()


let%test_unit "testVarGlobalesDoubleDeclaration"= 
try 
  let _ = compiler (pathFichiersRat^"testVarGlobalesDoubleDeclaration.rat") 
  in raise ErreurNonDetectee
with
| DoubleDeclaration("a") -> ()


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

(* Voir dans avec_fonction *)


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
