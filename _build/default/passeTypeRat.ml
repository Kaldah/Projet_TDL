(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* analyse_type_expression : tds -> AstTds.expression -> AstType.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_expression tds e = (AstType.Booleen true) (* failwith "todo"*)

(* analyse_type_instruction : tds -> info_ast option -> AstTds.instruction -> AstType.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction tds oia i = 
  failwith "TO DO"


(* analyse_type_bloc : tds -> info_ast option -> AstTds.bloc -> AstType.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc tds oia li = 
  failwith "TO DO"

(* analyse_type_fonction : tds -> AstTds.fonction -> AstType.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_type_fonction maintds (AstTds.Fonction(t,n,lp,li))  =
  failwith "TO DO"

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions, prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_type_fonction tds) fonctions in
  let nb = analyse_type_bloc tds None prog in
  AstType.Programme (nf,nb)


