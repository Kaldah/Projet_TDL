(* Module de la passe de gestion des types *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* obtenir_type_info : info_ast -> typ *)
(* Paramètre ia : l'information dont on veut obtenir le type *)
(* Renvoie le type associé à l'information *)
let obtenir_type_info ia = 
  match (info_ast_to_info ia) with
  | InfoFun (_,t,_) -> t              (* Si c'est une fonction, on renvoie son type *)
  | InfoVar (_,t,_,_) -> t            (* Si c'est une variable, on renvoie son type *)
  | InfoConst (_,_) -> Int            (* Si c'est une constante, le type est Int *)

(* analyse_type_affectable : AstTds.affectable -> AstType.affectable * typ *)
(* Paramètre a : l'affectable à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'affectable
en un affectable de type AstType.affectable *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_affectable a = match a with
  | AstTds.Ident info -> 
    (
      match info_ast_to_info info with
        | InfoVar(_,t,_,_)-> (AstTds.Ident info,t)
        | _ -> failwith "Erreur interne Ident"
    )
  | AstTds.Deref na -> 
    let (na2, ta) = analyse_type_affectable na in
    match ta with
      | Pointeur t -> (AstTds.Deref na2, t)
      | _ -> raise (Exceptions.TypeInattendu(ta, Pointeur Undefined))

(* analyse_type_expression : AstTds.expression -> AstType.expression * typ *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e = match e with
  | AstTds.AppelFonction(info, le) -> (
    match info_ast_to_info info with
    | InfoFun(_, tr, ltp) -> 
      (* On analyse et récupère les expression et les types *)
      let lnet = List.map analyse_type_expression le in

      (* On les récupère séparément *)
      let lne = List.map fst lnet in
      let lte = List.map snd lnet in

      (* Comparaison des types *)
      if (est_compatible_list lte ltp) then
        (AstType.AppelFonction(info, lne), tr)
      else
        raise (Exceptions.TypesParametresInattendus(lte, ltp))
    | _ -> failwith "Erreur interne AppelFonction"
  )

  | AstTds.Affectable a ->
    let (na, ta) = analyse_type_affectable a in
    (AstType.Affectable(na), ta)
  | AstTds.New t -> (AstType.New t, Pointeur t)
  | AstTds.Null -> (AstType.Null, Null)
  | AstTds.Adresse info -> 
    (
      match info_ast_to_info info with
        | InfoVar(_, t, _, _) -> (AstType.Adresse info, Pointeur t)
        | _ -> failwith "Erreur interne Adresse"
    )

  | AstTds.Unaire (op, e1) -> let (ne, te) = analyse_type_expression e1 in
    if (est_compatible te Rat) then
    (
      match op with
      | Numerateur -> (AstType.Unaire(AstType.Numerateur, ne), Int)
      | Denominateur -> (AstType.Unaire(AstType.Denominateur, ne), Int)
    )
    else
      raise (Exceptions.TypeInattendu(te, Rat))
  | AstTds.Binaire(op, e1, e2) ->
    
    (* On analyse les deux expressions *)
    let (ne1, te1) = analyse_type_expression e1 in
    let (ne2, te2) = analyse_type_expression e2 in

    (* On règle la surcharge et on vérifie les types *)
    begin
      match op with
      | Fraction ->
        (* On vérifie que l'opération a lieu entre des types compatibles avec des Int pour faire une Fraction *)
        if ((est_compatible te1 Int) && (est_compatible te2 Int)) then 
          (AstType.Binaire(AstType.Fraction, ne1, ne2), Rat)
        else
          raise (Exceptions.TypeBinaireInattendu (op, te1, te2))

      | Plus -> (
          match (te1, te2) with
          | Int, Int -> (AstType.Binaire(AstType.PlusInt, ne1, ne2), Int)
          | Rat, Rat -> (AstType.Binaire(AstType.PlusRat, ne1, ne2), Rat)
          | _ -> raise (Exceptions.TypeBinaireInattendu (op, te1, te2))
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
;;

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
            AstType.Declaration(info, ne)
        else
          raise (Exceptions.TypeInattendu(te, t))
        
  | AstTds.Affectation(a, e) -> 
    (* On vérifie si les types sont compatibles *)
    let (ne, te) = analyse_type_expression e in
    let (na, ta) = analyse_type_affectable a in
      if (est_compatible ta te) then
        AstType.Affectation(na, ne)
      else
        raise (Exceptions.TypeInattendu(te, ta))

  | AstTds.Affichage (e) -> 
    let (ne, te) = analyse_type_expression e in
    (
    match te with
      | Int -> AstType.AffichageInt ne
      | Bool -> AstType.AffichageBool ne
      | Rat -> AstType.AffichageRat ne
      | _ -> failwith "Erreur interne"
    )
      
  | AstTds.Conditionnelle (c, t, e) ->
    let (nc, te) = analyse_type_expression c in
      if (est_compatible te Bool) then
        let nbt = analyse_type_bloc t in
        let nbe = analyse_type_bloc e in
        AstType.Conditionnelle(nc, nbt, nbe)
      else
        raise (Exceptions.TypeInattendu (te, Bool))

  | AstTds.TantQue (e, b) -> 
    let (ne, te) = analyse_type_expression e in
      if (est_compatible te Bool) then
        let nb = analyse_type_bloc b in
        AstType.TantQue(ne, nb)
      else
        raise (Exceptions.TypeInattendu (te, Bool))

  | AstTds.Retour (e, ia) ->
    let (ne, te) = analyse_type_expression e in
    (
    match info_ast_to_info ia with
      | InfoFun(_,tr , _) ->
        if (est_compatible te tr) then 
          AstType.Retour(ne, ia)
        else
          raise (Exceptions.TypeInattendu(te, tr))
      | _ -> failwith "Erreur interne Retour"
    )

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
  let nfs = analyse_type_fonctions fonctions in
  let np = analyse_type_bloc prog in
  AstType.Programme (nfs,np)