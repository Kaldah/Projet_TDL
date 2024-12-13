open Rat
open Compilateur
open Passe

(* Return la liste des adresses des variables d'un programme RAT *)
let getListeDep ratfile =
  let input = open_in ratfile in
  let filebuf = Lexing.from_channel input in
  try
  let ast = Parser.main Lexer.token filebuf in
  let past = CompilateurRat.calculer_placement ast in
  let listeAdresses = VerifPlacement.analyser past in
  listeAdresses
  with
  | Lexer.Error _ as e ->
      report_error ratfile filebuf "lexical error (unexpected character).";
      raise e
  | Parser.Error as e->
      report_error ratfile filebuf "syntax error.";
      raise e

(* teste si dans le fichier fichier, dans la fonction fonction (main pour programme principal)
la occ occurence de la variable var a l'adresse dep[registre]
*)
let test fichier fonction (var,occ) (dep,registre) = 
  let l = getListeDep fichier in
  let lmain = List.assoc fonction l in
  let rec aux i lmain = 
    if i=1 
    then
      let (d,r) = List.assoc var lmain in
      (d=dep && r=registre)
    else 
      aux (i-1) (List.remove_assoc var lmain)
  in aux occ lmain

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../projet_Tests/gestion_id/avec_fonction/fichiersRat/"

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
  let _ = compiler (pathFichiersRat^"testDDVar1.rat")
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