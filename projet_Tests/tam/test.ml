open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)

let runtamcmde = "java -jar ../../../../projet_Tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  (*Sys.remove tamfile;    à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../projet_Tests/tam/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

(*************)
(* POINTEURS *)
(*************)

(* Tester les opérations générées pour le déréférencement et l’assignation.
Vérifier l’absence d’erreurs lors de l’utilisation d’un pointeur null.
*)
(*
let%expect_test "testPointeur1" =
  runtam (pathFichiersRat^"testPointeur1.rat");
  [%expect{| 423 |}]

  let%expect_test "testPointeurRat" =
  runtam (pathFichiersRat^"testPointeurRat.rat");
  [%expect{| [2/5] |}]


let%expect_test "testPointeurFun" =
  runtam (pathFichiersRat^"testPointeurFun.rat");
  [%expect{| 34[12/3] |}]


let%expect_test "testPointeurFunParam" =
  runtam (pathFichiersRat^"testPointeurFunParam.rat");
  [%expect{| 312 |}]

(*************)
(*  GLOBALES *)
(*************)

let%expect_test "testVarGlobalesSimpleInt" =
  runtam (pathFichiersRat^"testVarGlobalesSimpleInt.rat");
  [%expect{| 3 |}]

let%expect_test "testVarGlobalesSimpleRat" =
  runtam (pathFichiersRat^"testVarGlobalesSimpleRat.rat");
  [%expect{|[2/5]  |}]

let%expect_test "testVarGlobalesSimplePointeur" =
  runtam (pathFichiersRat^"testVarGlobalesSimplePointeur.rat");
  [%expect{| [2/5] |}]


let%expect_test "testVarGlobalesInt" =
  runtam (pathFichiersRat^"testVarGlobalesInt.rat");
  [%expect{| 313574 |}]


let%expect_test "testVarGlobalesRat" =
  runtam (pathFichiersRat^"testVarGlobalesRat.rat");
  [%expect{| [1/5][2/5][4/5][7/5][11/5][11/5] |}]
*)

let%expect_test "testVarGlobalesEtPointeur" =
  runtam (pathFichiersRat^"testVarGlobalesEtPointeur.rat");
  [%expect{| 13574 |}]

(*
S’assurer que les instructions générées permettent une modification correcte depuis différentes fonctions.
Tester les dépendances et initialisations successives.
*)


(*************)
(*  LOCALES  *)
(*************)

(*
Vérifier la persistance des valeurs entre appels via le code généré.
Tester les réinitialisations illégales.
*)

(*************)
(* DÉFAUTS   *)
(*************)

(*
Valider que le code gère correctement les appels avec des paramètres omis.
Tester les cas où les valeurs par défaut interagissent avec d’autres arguments.
*)
(*
let%expect_test "testParamDef1" =
  runtam (pathFichiersRat^"testParamDef1.rat");
  [%expect{| 54 |}]

let%expect_test "testParamDef2" =
  runtam (pathFichiersRat^"testParamDef2.rat");
  [%expect{| 56 |}]
*)