(* Module de la passe de gestion des placements *)
(* doit être conforme à l'interface Passe *)

open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let rec analyse_placement_instruction i depl reg =
match i with
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
    | InfoFun(_, t, lt) -> 
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
  | AstType.Empty -> (AstPlacement.Empty, 0)
and
analyse_placement_bloc li depl reg =
let rec aux compteur lst = match lst with
      | h::q ->  let (i, taille) = analyse_placement_instruction h compteur reg in 
      (i, taille)::(aux (compteur + taille) q)
      | [] -> []
    in
    let nliTaille = aux depl li in
    let nli = List.map fst nliTaille in
    let listeTaille  = List.map snd nliTaille in
    let taille = List.fold_left (fun acc n -> acc + n) 0 listeTaille in
  (nli, taille)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) = 
  match (info_ast_to_info info) with
  | InfoFun(_, _, _) -> 
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

let analyser (AstType.Programme (fonctions, prog)) = 
  let nfs = List.map analyse_placement_fonction fonctions in
  let np = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nfs,np)