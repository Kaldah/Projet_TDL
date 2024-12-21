(* Module de la passe de gestion des placements *)
(* doit être conforme à l'interface Passe *)
(* Erreur uniquement en cas d'erreur interne donc aucune n'est exception *)
(* n'est levé dans cette passe car rien n'est à vérifier. *)

open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* analyse_placement_instruction : AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre depl : le déplacement actuel dans le registres *)
(* Paramètre reg : le registre actuel *)
(* Met à jour les Infos de la TDS et renvoie l'instruction avec la taille de l'instruction *)
(* dans le registre reg et est de type AstPlacement.instruction * int *)

let rec analyse_placement_instruction i depl reg =
match i with
  | AstType.DeclarationStatic _ -> raise Exceptions.VariableStatiqueDansMain
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

(* analyse_placement_bloc : AstType.bloc -> int -> string -> AstPlacement.bloc *)
(* Paramètre li : la liste des instructions du bloc à analyser *)
(* Paramètre depl : le déplacement actuel dans le registres *)
(* Paramètre reg : le registre actuel *)
(* Met à jour les Infos de la TDS et renvoie le bloc avec la taille du bloc *)
(* dans le registre reg et est de type AstPlacement.bloc * int *)

analyse_placement_bloc li deplOrigine reg =
let rec aux depl lst = match lst with
      | h::q ->  let (i, taille) = analyse_placement_instruction h depl reg in 
        (i, taille)::(aux (depl + taille) q)
      | [] -> []
    in
      (* La liste des instructions avec leur taille *)
      let nliTaille = aux deplOrigine li in
      (* Liste des instructions *)
      let nli = List.map fst nliTaille in
      (* Liste des tailles des instructions *)
      let listeTaille  = List.map snd nliTaille in
      (* pour obtenir la taille il faut retirer la valeur du déplacement d'origine *)
      let taille = ((List.fold_left (fun acc n -> acc + n) 0 listeTaille)) in
        (nli, taille)


(* On défini des fonctions pour traiter spécifiquement certaines instructions des fonctions. *)
(* Notamment les variables statiques locales pour faire remonter le déplacement dans SB *)
(* et le prendre en compte dans la suite de la compilation (les autres fonctions et le programme principal) *)

(* analyse_placement_instruction_fonction : AstType.instruction -> int -> int -> (AstPlacement.instruction * int) * int *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre deplLB : le déplacement actuel dans le registres LB *)
(* Paramètre deplSB : le déplacement actuel dans le registre SB *)
(* Analyse une instruction d'une fonction, traite les variables statiques locale. *)
(* et les autres instructions sont traitées avec analyse_placement_instruction. *)
(* Met à jour les Infos de la TDS et renvoie l'instruction avec la taille de l'instruction *)
(* dans le registre LB et la taille prise dans le registre SB et est de type (AstPlacement.instruction * int) * int *)

let analyse_placement_instruction_fonction i deplLB deplSB = match i with
    | AstType.DeclarationStatic (info, e) -> 
      let (_, t, _,_) = info_var info in
      let taille = getTaille t in
        modifier_adresse_variable (deplSB) "SB" info;
        
        ((AstPlacement.DeclarationStatic(info, e), 0), taille)
    | _ -> (analyse_placement_instruction i deplLB ("LB"), 0)


(* analyse_placement_bloc_fonction : AstType.instruction list -> int -> int -> (AstPlacement.instruction list * int) * int *)
(* Paramètre li : la liste d'instructions à analyser *)
(* Paramètre deplLB : le déplacement actuel dans le registres LB *)
(* Paramètre deplSB : le déplacement actuel dans le registre SB *)
(* Analyse un bloc d'instructions d'une fonction *)
(* Met à jour les Infos de la TDS et renvoie le bloc d'instructions avec la taille du bloc dans le registre LB *)
(* et la taille prise dans le registre SB et est de type (AstPlacement.instruction * int) * int *)

let rec analyse_placement_bloc_fonction li deplLB deplSB = 
  match li with
      | h::q -> 
        (* On récupère le résultat du traitement de l'instruction de la fonction *)
        let ((i, tailleLB), tailleSB) = analyse_placement_instruction_fonction h deplLB deplSB in 
          (* On traite les instructions suivantes pour récupérer la liste des résultats des suivantes *)
          let ((listeInstructions, tailleBlocActuelleLB),tailleBlocActuelleSB) = 
          (analyse_placement_bloc_fonction q (deplLB + tailleLB) (deplSB + tailleSB))
          in
          (* On renvoie la mise à jour des variables : listes des instructions et les tailles dans les registres LB et SB *)
            ((i::listeInstructions, tailleLB + tailleBlocActuelleLB), tailleSB + tailleBlocActuelleSB)
      (* Cas final, on a traité toutes les instructions *)
      | [] -> ([], 0), 0

(* traiter_parametres_fonction : int -> info_ast list -> unit *)
(* Paramètre li : la liste d'instructions à trier *)
(* Traite la liste des infos des paramètres d'une fonction *)
(* Les fonctions partagent toutes le registre LB, le placement dépend *) 
(* donc uniquement des instructions de la fonction elle-même. *)
let rec traiter_parametres_fonction tailleActuelleParam lst = 
  (
  match lst with 
  (* S'il n'y a aucun paramètre, on ne fait rien *)
  | [] -> () 
  | h::q ->
    begin
      match (info_ast_to_info h) with
      | InfoVar(_, t, _, _) -> let nouvelleTailleParam = (tailleActuelleParam - (getTaille t)) in
      (* On met à jour l'info du paramètre pour le placer dans LB *)
      modifier_adresse_variable nouvelleTailleParam "LB" h;
      (* On traite les paramètres suivants *)
      traiter_parametres_fonction nouvelleTailleParam q;
      | _ -> failwith ("Erreur interne paramètres fonction")
    end
  )

(* recuperer_declaration_static : AstType.instruction list -> AstPlacement.instruction list * AstPlacement.instruction list *)
(* Paramètre li : la liste d'instructions à trier *)
(* Renvoie la liste des instructions de type AstPlacement.DeclarationStatic et les autres *)
(* pour les traiter séparément dans la passe suivante, la passe de codage *)

let rec recuperer_declaration_static li =  
  match li with
  | h::q -> 
    let (lstFun, lstStatic) = recuperer_declaration_static q in 
    begin
      match h with 
        | AstPlacement.DeclarationStatic _ -> (lstFun, h::lstStatic)
        | _ -> (h::lstFun, lstStatic)
    end
  | [] -> ([],[])

(* analyse_placement_fonction : AstType.fonction -> int -> AstPlacement.fonction * (AstPlacement.instruction list * int) *)
(* Paramètre info : l'info_ast de la fonction analysée *)
(* Paramètre lp : la liste des info_ast des paramètes de la fonction *)
(* Paramètre li : le bloc des instructions de la fonction *)
(* Analyse le placement d'une fonction. *)
(* Renvoie la fonction et la liste des instructions des variables statiques avec leur taille *)

let analyse_placement_fonction (AstType.Fonction(info,lp, li )) deplSB = 
  match (info_ast_to_info info) with
  | InfoFun(_, _, _,_) -> 
    traiter_parametres_fonction 0 (List.rev lp);
      (* Lors de la création du registre, on décale de 3 places pour le registre *)
      let ((nli, tailleBlocLB), tailleBlocSB) = analyse_placement_bloc_fonction li 3 deplSB in
        let (nliFun, nliStatic) = recuperer_declaration_static nli in
          (AstPlacement.Fonction(info, lp, (nliFun, tailleBlocLB)), (nliStatic, tailleBlocSB))
  | _-> failwith "Erreur interne Placement Fonction"


(* analyse_placement_fonctions : AstType.fonction list -> int -> AstPlacement.fonction list * (AstPlacement.instruction list * int) *)
(* Paramètre info : l'info_ast de la fonction analysée *)
(* Paramètre lp : la liste des info_ast des paramètes de la fonction *)
(* Paramètre li : le bloc des instructions de la fonction *)
(* Analyse le placement des fonctions. *)
(* Renvoie la liste des fonctions et la liste des instructions des variables statiques avec sa taille totale *)

let analyse_placement_fonctions lf deplSB =
  (* On défini une fonction auxiliaire pour prendre en compte le décalage dans le registre SB*)
  let rec aux lst depl =
  match lst with
  | h::q ->
    let (niFun, (liStatic, tailleFonctionSB)) = analyse_placement_fonction h (deplSB + depl) in
      let (nlf, (nliStatic, tailleActuelleFonctionsSB)) = aux q (tailleFonctionSB + depl) in
        (niFun::nlf, (nliStatic@liStatic, tailleActuelleFonctionsSB))
  | [] -> ([],([], depl))
  in
    aux lf 0

let analyser (AstType.Programme (lg, fonctions, prog)) = 
  let (nlg, tailleVarGlobales) = analyse_placement_bloc lg 0 "SB" in
    let (nfs, (liStatic, tailleFonctionsSB)) = analyse_placement_fonctions fonctions tailleVarGlobales in
      let deplSB = tailleFonctionsSB + tailleVarGlobales in
        let blocVarStatique = (liStatic, deplSB) in
        let bloc_var_globale = (nlg, tailleVarGlobales) in
        let bloc_main = analyse_placement_bloc prog deplSB "SB" in
          AstPlacement.Programme (bloc_var_globale, nfs, blocVarStatique, bloc_main)