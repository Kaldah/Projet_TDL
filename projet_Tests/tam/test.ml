open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../Projets_Tests/runtam.jar"
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
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../Projets_Tests/tam/fichiersRat/"

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

let%expect_test "testprintint" =
  runtam (pathFichiersRat^"testprintint.rat");
  [%expect{| 42 |}]

(*************)
(*  GLOBALES *)
(*************)

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