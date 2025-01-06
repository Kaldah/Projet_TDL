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

let pathFichiersRat = "../../../../projet_Tests/placement/fichiersRat/"

(**********)
(*  TESTS *)
(**********)


(*************)
(* POINTEURS *)
(*************)

(*
Vérifier que chaque pointeur dispose d’une adresse unique en mémoire.
S’assurer de la gestion correcte des adresses et des valeurs pointées.
*)

let%test "test_px" = 
  test (pathFichiersRat^"test.rat")  "main" ("px", 1)  (0,"SB")

let%test "test_x" = 
  test (pathFichiersRat^"test.rat")  "main" ("x", 1)  (1,"SB")

let%test "test_y" = 
  test (pathFichiersRat^"test.rat")  "main" ("y", 1)  (2,"SB")

(* Bug, non reconnaissance du main*)

let%test "pointeur1_a" = 
  test (pathFichiersRat^"pointeur1.rat") "main" ("a",2) (0,"SB")

let%test "pointeur1_x" = 
  test (pathFichiersRat^"pointeur1.rat") "main" ("x",2) (3,"SB")

  let%test "pointeur1_c" = 
  test (pathFichiersRat^"pointeur1.rat") "main" ("a",2) (2,"SB")

let%test "pointeur1_a_f" = 
  test (pathFichiersRat^"pointeur1.rat") "add" ("a",1) (-3,"LB")

  let%test "pointeur1_b_f" = 
  test (pathFichiersRat^"pointeur1.rat") "add" ("b",1) (-2,"LB")

  let%test "pointeur1_c_f" = 
  test (pathFichiersRat^"pointeur1.rat") "add" ("c",1) (-1,"LB")


(*************)
(*  GLOBALES *)
(*************)

(*
Tester le placement en mémoire, avec des adresses distinctes pour chaque variable.
Valider que les variables globales sont initialisées au démarrage du programme.
*)

(*************)
(*  LOCALES  *)
(*************)

(*
Vérifier que la mémoire allouée est persistante entre plusieurs appels.
S’assurer que l’espace mémoire pour ces variables est bien isolé des variables locales classiques.
*)

(*************)
(* DÉFAUTS   *)
(*************)


(*
Vérifier la gestion mémoire pour les paramètres par défaut (valeurs expressions comme (1+2)).
*)

let%test_unit "testParamDef1" = 
  let _ = compiler (pathFichiersRat^"testParamDef1.rat") in ()

  let%test_unit "testParamDef2" = 
  let _ = compiler (pathFichiersRat^"testParamDef2.rat") in ()