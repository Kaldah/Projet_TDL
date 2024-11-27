(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* analyse_type_expression : AstTds.expression -> AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_expression e = (AstType.Booleen true , type.Rat)

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i = 
  match i with
  | AstTds.Declaration (t, info_ast , e) -> 
    (* On vérifie si les types sont compatibles *)
      let (ne, te) = analyse_type_expression e in if (est_compatible t te) then
        let InfoVar(n, _, _, _) = info_ast_to_info info_ast in
      (* On ajoute le type à l'info *)
      AstType.Declaration(info_to_info_ast InfoVar(n, te, 0, ""), ne)
      else 
        raise Exceptions.TypeInattendu

  | Ast.Tds.Affectation(info, e) -> 
    (* On vérifie si les types sont compatibles *)
    let (ne, te) = analyse_type_expression e in
      let InfoVar(n, tid_, _, _) = info_ast_to_info info_ast in
      if (est_compatible tid te) then
        AstType.Affectation(info, ne)
      else 
        raise Exceptions.TypeInattendu
  | AstTds.Affichage (e) -> let (ne, te) = analyse_type_expression e in
        match te with
        | Int -> AstType.AffichageInt ne
        | Bool -> AstType.AffichageBool ne
        | Rat -> AstType.AffichageRat ne
        | _ -> failwith "Erreur interne" 
  | AstTds.Conditionnelle (e, t, c) -> let (ne, te) = analyse_type_expression e in
        if (te est_compatible Bool) then
          nbt = analyse_type_bloc t;
          nbc = analyse_type_bloc c;
          AstType.Conditionnelle(ne, nbc, nbt);
        else raise Exceptions.TypeInattendu
  | AstTds.TantQue -> 
  | AstTds.Retour (e, ia) -> 
  | AstTds Empty
(* analyse_type_bloc : AstTds.instruction List -> AstType.instruction List *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li = 
  List.map analyse_type_instruction li

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_type_fonction (AstTds.Fonction(t,info,lp,li))  =
  failwith "TO DO"


  let analyse_type_fonctions lf =
  failwith "TO DO"

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions, prog)) =
  let nfs = List.map (analyse_type_fonction fonctions) fonctions in
  let np = analyse_type_bloc prog None prog in
  AstType.Programme (nfs,np)


