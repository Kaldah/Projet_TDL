(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)

open Tam
open Tds
open Ast
open Code
open Type

type t1 = Ast.AstPlacement.programme
type t2 = string

let concat_code liste_codes = List.fold_left ( ^ ) "" liste_codes;;

let rec get_type_affectable a = 
  match a with
  | AstTds.Ident info -> 
    (
    match info_ast_to_info info with
      | InfoVar(_, t, _, _) -> t
      | InfoConst(_, _) -> Int
      | _ -> failwith "Erreur interne Ident"
      )
  | AstTds.Deref a -> 
    let t = get_type_affectable a in
    (
    match t with
      | Pointeur d -> d
      | _ -> failwith "Erreur interne Deref"
    )
let rec analyse_code_affectable a en_ecriture = 
  match a with
  | AstTds.Ident info -> 
  (
    match (info_ast_to_info info)  with
      | InfoVar(_, t, d, reg) ->
      let taille_type = (getTaille t) in 
      (* On vérifie si on lit la variable ou si on l'affecte *)
        if en_ecriture then
          store taille_type d reg
        else
          load taille_type d reg
      | InfoConst(_, c) -> loadl_int c
      | _ -> failwith "Erreur interne Ident"
  )
  | AstTds.Deref na -> 
    (* On load tout ce qu'on va utiliser *)
    (* Obtient le type de na *)
    let t = get_type_affectable a in
    let taille = getTaille t in
    let code_a = analyse_code_affectable na false in
    
    (* On vérifie si on lit ou affecte la variable *)
    if en_ecriture then
      (* Faire attention, taille vaut la taille après déréférencement *)
      code_a ^ (storei taille)
    else
      code_a ^ (loadi taille)

let rec analyse_code_expression e =
  match e with
  | AstType.AppelFonction (info, le ) -> 
    (* On charge toutes les expressions de le *)
    let liste_codes = List.map analyse_code_expression le in
    let code_exp = concat_code liste_codes in 
    let nom, _, _, _ = info_fun info in
    code_exp ^ (call "SB" nom)
  | AstType.Affectable a -> analyse_code_affectable a false
  | AstType.New t -> 
    let taille_type = (getTaille t) in
    (loadl_int taille_type) ^ (subr "MAlloc")
  | AstType.Adresse info -> 
  (
    match (info_ast_to_info info) with
      | InfoVar(_, _, d, reg) -> loada d reg
      | _ -> failwith "Erreur interne Ident"
  )
  | AstType.Null -> ""

  | AstType.Booleen b ->
    if b then 
      loadl_int 1
    else
      loadl_int 0
  | AstType.Entier i -> loadl_int i
  | AstType.Unaire (op, e) -> let code_e = analyse_code_expression e in
    (
      match op with
        | Numerateur -> code_e ^ (pop 0 1)
        | Denominateur -> code_e ^ (pop 1 1)
    ) 
  | AstType.Binaire (op, e1, e2) -> 
    let code_e1 = analyse_code_expression e1 in
    let code_e2 = analyse_code_expression e2 in
    let code = code_e1 ^ code_e2 in
  (
    match op with
      | Fraction -> code
      | PlusInt -> code ^ (subr "IAdd")
      | PlusRat -> code ^ (call "ST" "RAdd")
      | MultInt -> code ^ (subr "IMul")
      | MultRat -> code ^ (call "ST" "RMul")
      | EquBool | EquInt -> code ^ (subr "IEq")
      | Inf -> code ^ (subr "ILss")
  ) 

  let rec analyse_code_instruction i =
    match i with
    | AstPlacement.Declaration ( info , e) | AstPlacement.DeclarationStatic (info, e)  -> let (_, t, d, reg) = info_var info in
      let taille_type_e = (getTaille t) in 
        (push taille_type_e) ^ (analyse_code_expression e) ^ (store taille_type_e d reg)
    | AstPlacement.Affectation ( a , e) ->  
      
    let sa = analyse_code_affectable a true in
    analyse_code_expression e ^ sa

    | AstPlacement.AffichageInt e -> analyse_code_expression e ^ (subr "IOut")
    | AstPlacement.AffichageRat e -> analyse_code_expression e ^ (call "ST" "ROut")
    | AstPlacement.AffichageBool e -> analyse_code_expression e ^ (subr "BOut")
    | AstPlacement.AffichagePointeur (e, t) -> 
      let taille_type = (getTaille t) in
      analyse_code_expression e ^ (loadi taille_type) ^ (subr "IOut")
    | AstPlacement.AffichageNull e -> analyse_code_expression e ^ (subr "SOut")

    | AstPlacement.Conditionnelle (c, t , e) -> 
      let etiquetteE = getEtiquette () in 
      let etiquetteFin = getEtiquette () in
      let code_e = analyse_code_expression c in
      let code_bloc_t = analyse_code_bloc t in
      let code_bloc_e = analyse_code_bloc e in
      code_e ^ (jumpif 0 etiquetteE) ^ code_bloc_t ^ (jump etiquetteFin) ^ (label etiquetteE) ^ code_bloc_e ^ (label etiquetteFin)

    | AstPlacement.TantQue (c, b) -> 
      let etiquetteTantQue = getEtiquette () in 
      let etiquetteFin = getEtiquette () in
      let code_condition = analyse_code_expression c in
      let code_bloc = analyse_code_bloc b in
      (label etiquetteTantQue) ^ code_condition ^ (jumpif 0 etiquetteFin) ^ code_bloc ^ (jump etiquetteTantQue) ^ (label etiquetteFin)
    
    | AstPlacement.Retour (e, tailleRet , tailleParam) -> (analyse_code_expression e) ^ (return tailleRet tailleParam)
    | AstPlacement.Empty -> ""
    and analyse_code_bloc (li , _ ) = let liste_codes = List.map analyse_code_instruction li in
    concat_code liste_codes

  let analyse_code_fonction (AstPlacement.Fonction (info ,_ , bloc)) = 
    let nom, _, _, _ = info_fun info in
      (label nom) ^ analyse_code_bloc bloc ^ halt 

  let analyse_code_variables_globales (AstType.DeclarationGlobale (ia, e)) = 
    let (_, t, d, reg) = info_var ia in
    let taille_type = (getTaille t) in 
    (push taille_type) ^ (analyse_code_expression e) ^ (store taille_type d reg)
    

let analyser (AstPlacement.Programme (vg, fonctions, varStatic, prog)) = 
  let code_vg = analyse_code_bloc vg in
  let code_fonctions = concat_code (List.map analyse_code_fonction fonctions) in 
  let code_vars_static = analyse_code_bloc varStatic in
  let code_prog = analyse_code_bloc prog in
  let code_complet = (getEntete ()) ^ code_fonctions ^ "main\n" ^ code_vg ^ code_vars_static ^ code_prog ^ halt in
  (*
  print_string ("\n \n CODE \n" ^ code_fonctions ^ "main\n" ^ code_vg 
  ^ "Fin Variables globales \n" ^ code_prog ^ "\n  CODE \n \n");
 *)
  (*print_string code_complet;   *)
  code_complet