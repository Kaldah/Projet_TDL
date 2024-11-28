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
    | InfoVar(n, t, _, _) ->
      let nInfo = InfoVar(n, t, depl, reg) in
      (AstPlacement.Declaration(info_to_info_ast nInfo, e), getTaille t)
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
    (
    match (info_ast_to_info ia) with
    | InfoVar(n, t, _, _) -> 
      let nInfo = InfoVar(n, t, depl, reg) in
      (AstPlacement.Affectation (info_to_info_ast nInfo, e), getTaille t)
    | _ -> failwith "Erreur interne"
    )
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
    (* let nliTaille = List.map (fun x -> analyse_placement_instruction x depl reg) li in *)
    let nli = List.map fst nliTaille in
    let listeTaille  = List.map snd nliTaille in
    let taille = List.fold_left (fun acc n -> acc + n) 0 listeTaille in
  (nli, taille)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) = 
  match (info_ast_to_info info) with
  | InfoFun(_, t, ltp) -> let taille = getTaille Rat + getTaille t in
  (* De la place est déjà prise dans le registre pour le type de retour ou taille vaut juste 3
  à comprendre, peut être réserver pour Rat, la fonction et le type de retour *)

  (* Traiter lp la liste des infos des paramèters *)
  let rec aux_params compteur lst = 
    (
    match lst with 
    | [] -> [] 
    | h::q ->
      begin
        match (info_ast_to_info info) with
        | InfoVar(n, t, _, _) -> let nInfo = info_to_info_ast (InfoVar(n, t, compteur, "LB")) in
          nInfo::(aux_params (-(getTaille t) + compteur) q)
        | _ -> failwith "Erreur interne paramètres fonction" 
      end
    )
    in
    (*let nlp = aux_params (-1) lp in *)
    AstPlacement.Fonction(info, lp, (analyse_placement_bloc li taille "LB"))
  | _-> failwith "Erreur interne Placement Fonction"

let analyser (AstType.Programme (fonctions, prog)) = 
  let nfs = List.map analyse_placement_fonction fonctions in
  let np = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nfs,np)