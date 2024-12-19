(* Module de la passe de gestion des placements *)
(* doit être conforme à l'interface Passe *)

open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

let rec analyse_placement_instruction i depl reg =
match i with
  | AstType.DeclarationStatic (info, e) -> (AstPlacement.DeclarationStatic(info, e), 0)
  | AstType.Declaration (info, e) -> 
    (
    match (info_ast_to_info info) with
    | InfoVar(_, t, _, _) ->
      print_string "Déclaration Var Main, nDepl : ";
      print_int depl; print_newline ();
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


(* On défini des fonctions pour traiter spécifiquement certaines instructions des fonctions. *)
(* Notamment les variables statiques locales pour renvoyer le déplacement dans ST et le prendre en compte*)

let analyse_placement_instruction_fonction i depl reg deplSB = match i with
    | AstType.DeclarationStatic (info, e) -> let (_, t, _,_,_) = info_var info in
     let taille = getTaille t in 
     ((AstPlacement.DeclarationStatic(info, e), 0), deplSB +  taille)
    | AstType.Declaration (info, e) -> 
      begin
        match (info_ast_to_info info) with
        | InfoStaticVar(_, t, _, _, _) -> 
          let taille = getTaille t in
            print_string "Déclaration Var Statique, nDepl : ";
            print_int deplSB; print_newline ();
            modifier_adresse_variable (deplSB) "SB" info;
            let nDeplSB = deplSB + taille in
              ((AstPlacement.Declaration(info, e), getTaille t), nDeplSB)
        | _ -> print_string "Déclaration Var Fonction, nDepl : ";
              print_int deplSB; print_newline (); 
              (analyse_placement_instruction i depl reg, deplSB)
      end
    | _ -> (analyse_placement_instruction i depl reg, deplSB)

let analyse_placement_bloc_fonction li depl reg deplSB = 
  let rec aux compteur compteurSB lst = match lst with
      | h::q -> 
        let ((i, taille), nDeplSB) = analyse_placement_instruction_fonction h compteur reg compteurSB in 
        let (nli, ntaille) = (aux (compteur + taille) (nDeplSB) q) in
        ((i, taille)::nli, ntaille)
      | [] -> ([], compteurSB)
    in
    let (nliTaille, nDeplSB) = aux depl deplSB li
  in
  let nli = List.map fst nliTaille in
    let listeTaille  = List.map snd nliTaille in
    let taille = ((List.fold_left (fun acc n -> acc + n) 0 listeTaille)) in
    (* nDeplSB correspond au décalage causé par les variables satiques de la fonction dans la SB *)
    ((nli, taille), nDeplSB)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) deplSB = 
  match (info_ast_to_info info) with
  | InfoFun(_, _, _,_) -> 
  (* Traiter lp la liste des infos des paramètres *)
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
    let (bloc, ndeplSB) = analyse_placement_bloc_fonction li 3 "LB" deplSB in
    
    (AstPlacement.Fonction(info, lp, bloc), ndeplSB)
  | _-> failwith "Erreur interne Placement Fonction"


let analyse_placement_fonctions lf deplSB = 
  let rec aux lst depl =
  match lst with
  | h::q -> 
    let (nf, ndeplSB) = analyse_placement_fonction h depl in
      let (nlf, nDeplSB) = aux q ndeplSB in
        (nf::nlf, nDeplSB)
  | [] -> ([],depl)
  in
    aux lf deplSB


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
  let (nlg, deplSB) = analyse_placement_bloc lg 0 "SB" in
  let (nfs, ndeplSB) = analyse_placement_fonctions fonctions deplSB in
  
  print_string "\n déplacementSB post globale : "; print_int deplSB;
  print_string "\n déplacementSB post fonctions : "; print_int ndeplSB; print_string "\n";
  
  let (np, npTaille) = analyse_placement_bloc prog ndeplSB "SB" in
  AstPlacement.Programme ((nlg, deplSB), nfs,(np,npTaille))