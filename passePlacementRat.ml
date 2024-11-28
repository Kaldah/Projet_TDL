(* Module de la passe de gestion des placements *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme


let rec analyse_placement_instruction i depl reg =
match i with 
  | AstType.Declaration (info,e) ->
    (AstPlacement.Declaration(info, e), 0)
  | AstType.Conditionnelle (c, t, e) -> (AstPlacement.Conditionnelle(c, analyse_placement_bloc t depl reg, analyse_placement_bloc e depl reg), 0)
  | AstType.TantQue (e, b) -> (AstPlacement.TantQue(e, analyse_placement_bloc b depl reg), 0)
  | AstType.Retour (e, ia) -> (AstPlacement.Retour (e, 0, 0), 0)
  | AstType.Affectation (ia,e) -> (AstPlacement.Affectation (ia,e), 0)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, getTaille Int)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, getTaille Rat)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, getTaille Bool)
  | AstType.Empty -> (AstPlacement.Empty, 0)
  | _ -> (AstPlacement.Empty, 0)
and
analyse_placement_bloc li depl reg = 
    let nliTaille = List.map (fun x -> analyse_placement_instruction x depl reg) li in
    let nli = List.map fst nliTaille in
    let listeTaille  = List.map (fun x -> 0) nli in
    let taille = List.fold_left (fun acc n -> acc + n) 0 listeTaille in
  (nli, taille)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) = 
  AstPlacement.Fonction(info, lp, (analyse_placement_bloc li 0 "Bloc Fonction"))

let analyser (AstType.Programme (fonctions, prog)) = 
  let nfs = List.map analyse_placement_fonction fonctions in
  let np = analyse_placement_bloc prog 0 "Bloc Programme" in
  AstPlacement.Programme (nfs,np)