(* Module de la passe de gestion des placements *)
(* doit être conforme à l'interface Passe *)

open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let rec analyse_placement_instruction i depl reg =
match i with
  | AstType.DeclarationStatic (info, e) -> print_string "Static ?!";(AstPlacement.DeclarationStatic(info, e), 0)
  | AstType.Declaration (info, e) -> 
    (
    match (info_ast_to_info info) with
    | InfoVar(_, t, _, _) ->
      (* On met à jour les infos de la variable *)
      modifier_adresse_variable depl reg info;
      (AstPlacement.Declaration(info, e), getTaille t)
    | _ -> failwith "Erreur interne"
  )
  | AstType.Conditionnelle (c, t, e) -> (AstPlacement.Conditionnelle(c, analyse_placement_bloc t depl reg, analyse_placement_bloc e depl reg), 0)
  | AstType.TantQue (e, b) -> (AstPlacement.TantQue(e, analyse_placement_bloc b depl reg), 0)
  | AstType.Retour (e, ia) -> 
    (
    match (info_ast_to_info ia) with
    | InfoFun(_, t, lt,_) -> 
      let tailleParams = List.fold_left (fun acc x -> acc + (getTaille x)) 0 lt in 
      let tailleRetour = getTaille t in
      (AstPlacement.Retour (e, tailleRetour, tailleParams), tailleRetour + tailleParams)
    | _ -> failwith "Erreur interne"
    )
  | AstType.Affectation (ia,e) ->
      (AstPlacement.Affectation (ia, e), 0)

  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  (* Les affichages des pointeurs et de null ne sont pas implémenter *)
  | AstType.AffichagePointeur (_, _) ->(AstPlacement.Empty, 0) (* (AstPlacement.AffichagePointeur (e, t), 0) *)
  | AstType.AffichageNull _ -> (AstPlacement.Empty, 0) (*(AstPlacement.AffichageNull e, 0)*)
  | AstType.Empty -> (AstPlacement.Empty, 0)
and
analyse_placement_bloc li depl reg =
let rec aux compteur lst = match lst with
      | h::q ->  let (i, taille) = analyse_placement_instruction h compteur reg in 
      (i, taille)::(aux (compteur + taille) q)
      | [] -> []
    in
    (* La liste des instructions avec leur taille *)
    let nliTaille = aux depl li in
    (* Liste des instructions *)
    let nli = List.map fst nliTaille in
    (* Liste des tailles des instructions *)
    let listeTaille  = List.map snd nliTaille in
    (* pour obtenir la taille il faut retirer la valeur du déplacement d'origine *)
    let taille = ((List.fold_left (fun acc n -> acc + n) 0 listeTaille)) in
    (*print_string "Taille :\n";
    print_int taille; print_newline ();*)
  (nli, taille)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) = 
  match (info_ast_to_info info) with
  | InfoFun(_, _, _,_) -> 
  (* Traiter lp la liste des infos des paramèters *)
  let rec aux_params compteur lst = 
    (
    match lst with 
    | [] -> () 
    | h::q ->
      begin
        match (info_ast_to_info h) with
        | InfoVar(_, t, _, _) -> let ncompteur = (compteur - (getTaille t)) in
        modifier_adresse_variable ncompteur "LB" h;
        aux_params ncompteur q;
        | _ -> failwith ("Erreur interne paramètres fonction")
      end
    )
    in
    aux_params 0 (List.rev lp);
    (* Lors de la création du registre, on décale de 3 places pour le registre *)
    AstPlacement.Fonction(info, lp, (analyse_placement_bloc li 3 "LB"))
  | _-> failwith "Erreur interne Placement Fonction"

let analyse_placement_variable_globale lg = 

  let rec aux_globales lg compteur = 
  match lg with
  | [] -> (compteur, [])
  | h::q -> 
    begin
    match h with
    | AstType.DeclarationGlobale(info, e) -> 
      (
      match (info_ast_to_info info) with
      | InfoVar(_, t, _, _) -> 
        modifier_adresse_variable compteur "SB" info;
        let (ncompteur, nlg) = aux_globales q (compteur + (getTaille t)) in
        (ncompteur, (AstType.DeclarationGlobale(info, e))::nlg)
      | _ -> failwith "Erreur interne"
      )
    end 
    in aux_globales (List.rev lg) 0
let analyser (AstType.Programme (lg, fonctions, prog)) = 
  let (nlg, depl) = analyse_placement_bloc lg 0 "SB" in
  let nfs = List.map analyse_placement_fonction fonctions in
  let (np,npTaille) = analyse_placement_bloc prog depl "SB" in
  AstPlacement.Programme ((nlg, depl), nfs,(np,npTaille))