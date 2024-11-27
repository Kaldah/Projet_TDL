(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme


(* analyse_type_expression : AstTds.expression -> AstType.expression * typ *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e = match e with
| AstTds.AppelFonction(info, le) -> (AstType.Booleen(true) , Bool)
| AstTds.Ident info ->
  begin
  match (info_ast_to_info info) with
    | InfoVar(n, tid, _, _) ->
        (AstType.Ident(info_to_info_ast (InfoVar(n, tid, 0, ""))), tid)
    | _ -> failwith "Erreur interne"
  end

| AstTds.Unaire (op, e1) -> let (ne, te) = analyse_type_expression e1 in
if (est_compatible te Rat) then
begin
  match op with
  | Numerateur -> (AstType.Unaire(AstType.Numerateur, ne), Rat)
  | Denominateur -> (AstType.Unaire(AstType.Denominateur, ne), Rat)
end
else
  raise (Exceptions.TypeInattendu(te, Rat))
| AstTds.Binaire(op, e1, e2) -> let (ne1, te1) = analyse_type_expression e1 in
let (ne2, te2) = analyse_type_expression e2 in
begin
  match op with
  | Fraction -> if ((est_compatible te1 Int) && (est_compatible te2 Int)) then 
    (AstType.Binaire(AstType.Fraction, ne1, ne2), Rat)
  else
    begin
    print_string "Erreur Fraction \n";
    raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
    end
  | Plus -> (
      match (te1, te2) with
      | Int, Int -> (AstType.Binaire(AstType.PlusInt, ne1, ne2), Int)
      | Rat, Rat -> (AstType.Binaire(AstType.PlusRat, ne1, ne2), Rat)
      | _ -> print_string "Erreur PLus \n"; raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
    )
  | Mult -> (
      match (te1, te2) with
      | Int, Int -> (AstType.Binaire(AstType.MultInt, ne1, ne2), Int)
      | Rat, Rat -> (AstType.Binaire(AstType.MultRat, ne1, ne2), Rat)
      | _ -> raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
    )
  | Equ -> (
      match (te1, te2) with
      | Int, Int -> (AstType.Binaire(AstType.EquInt, ne1, ne2), Bool)
      | Bool, Bool -> (AstType.Binaire(AstType.EquBool, ne1, ne2), Bool)
      | _ -> raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
    )
  | Inf -> (
      match (te1, te2) with
      | Int, Int -> (AstType.Binaire(AstType.Inf, ne1, ne2), Bool)
      | _ -> raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
    )
end

| AstTds.Booleen (b) -> (AstType.Booleen b, Bool)
| AstTds.Entier i -> (AstType.Entier i, Int)

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i = 
  match i with
  | AstTds.Declaration (t, info , e) ->
    (* On vérifie si les types sont compatibles *)
      let (ne, te) = analyse_type_expression e in 
        if (est_compatible t te) then
          begin
          match (info_ast_to_info info) with 
          | InfoVar(n, _, _, _) -> 
            (* On ajoute le type à l'info *)
            AstType.Declaration(info_to_info_ast(InfoVar(n, te, 0, "")), ne)
          | _ -> failwith "Erreur interne"
          end
        else
          raise (Exceptions.TypeInattendu(te, t))
        
  | AstTds.Affectation(info_ast, e) -> 
    (* On vérifie si les types sont compatibles *)
    let (ne, te) = analyse_type_expression e in
    begin
    match (info_ast_to_info info_ast) with
      | InfoVar(_, tid, _, _) ->
        if (est_compatible tid te) then
          AstType.Affectation(info_ast, ne)
        else 
          raise (Exceptions.TypeInattendu(te, tid))
      | _ -> failwith "Erreur interne"
    end

  | AstTds.Affichage (e) -> let (ne, te) = analyse_type_expression e in
       begin
        match te with
        | Int -> AstType.AffichageInt ne
        | Bool -> AstType.AffichageBool ne
        | Rat -> AstType.AffichageRat ne
        | _ -> failwith "Erreur interne"
       end
      
  | AstTds.Conditionnelle (e, t, c) -> let (ne, te) = analyse_type_expression e in
        if (est_compatible te Bool) then
          let nbt = analyse_type_bloc t in
          let nbc = analyse_type_bloc c in
          AstType.Conditionnelle(ne, nbc, nbt)
        else
          raise (Exceptions.TypeInattendu (te, Bool))
  
  | AstTds.TantQue (e, b) -> let (ne, te) = analyse_type_expression e in
          if (est_compatible te Bool) then
            let nb = analyse_type_bloc b in
            AstType.TantQue(ne, nb)
          else
            raise (Exceptions.TypeInattendu (te, Bool))

  | AstTds.Retour (e, ia) ->
    let (ne, te) = analyse_type_expression e in
          if (est_compatible te Bool) then
            AstType.Retour(ne, ia)
          else
            raise (Exceptions.TypeInattendu (te, Bool))

  | AstTds.Empty -> AstType.Empty
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
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction(_,info,lp,li))  =
(* info contient déjà les types grâce à la passe précédente *)

  let nlip = List.map snd lp in
  let nli = analyse_type_bloc li in
  AstType.Fonction(info, nlip , nli )

  let analyse_type_fonctions lf =
    List.map analyse_type_fonction lf
(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions, prog)) =
  let nfs = List.map analyse_type_fonction fonctions in
  let np = analyse_type_bloc prog in
  AstType.Programme (nfs,np)

