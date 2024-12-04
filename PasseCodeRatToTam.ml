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


let rec analyse_code_expression e =
  match e with
  | AstType.AppelFonction (info, le ) -> 
    (* On charge toutes les expressions de le *)
    let liste_codes = List.map analyse_code_expression le in
    let code_exp = concat_code liste_codes in 
    let nom, _, _ = triplet_info_fun info in
    code_exp ^ (call "SB" nom)
    | AstType.Ident info -> 
    (
    match (info_ast_to_info info) with
      | InfoVar(_, t, d, reg) -> 
        let taille_type = (getTaille t) in load taille_type d reg
      | InfoConst(_, cst) -> loadl_int cst
      | _ -> failwith "Erreur interne Ident"
    )

  | AstType.Booleen b ->
    if b then 
      loadl_int 1
    else 
      loadl_int 0
  | AstType.Entier i -> loadl_int i
  | AstType.Unaire (op, e) -> let code_e = analyse_code_expression e in
    (
      match op with
        | Numerateur -> print_string (code_e ^ (pop 0 1)); code_e ^ (pop 0 1)
        | Denominateur -> print_string (code_e ^ (pop 0 1)); code_e ^ (pop 1 1)
    ) 
  | AstType.Binaire (op, e1, e2) -> 
    let code_e1 = analyse_code_expression e1 in
    let code_e2 = analyse_code_expression e2 in
    let code = code_e1 ^ code_e2 in
  (
    match op with
      | Fraction -> code
      | PlusInt -> code ^ subr "IAdd"
      | PlusRat -> code ^ (call "ST" "RAdd")
      | MultInt -> code ^ subr "IMul"
      | MultRat -> code ^ (call "ST" "RMul")
      | EquBool -> code ^ subr "BAnd"
      | EquInt -> code ^ subr "IEq"
      | Inf -> code ^ (subr "ILss")
  ) 

  let rec analyse_code_instruction i =
    match i with
    | AstPlacement.Declaration ( info , e) -> let (_, t, d, reg) = quadruplet_info_var info in
    let taille_type_e = (getTaille t) in 
    (push taille_type_e) ^ (analyse_code_expression e) ^ (store taille_type_e d reg)

    | AstPlacement.Affectation ( info , e) ->  let (_, t, d, reg) = quadruplet_info_var info in
    let taille_type_e = (getTaille t) in 
    analyse_code_expression e ^ (store taille_type_e d reg)

    | AstPlacement.AffichageInt e -> analyse_code_expression e ^ (subr "IOut")
    | AstPlacement.AffichageRat e -> analyse_code_expression e ^ (call "ST" "ROut")
    | AstPlacement.AffichageBool e -> analyse_code_expression e ^ (subr "BOut")

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
    and analyse_code_bloc (li , taille ) = let liste_codes = List.map analyse_code_instruction li in
    (concat_code liste_codes) ^ (pop 0 taille)

  let analyse_code_fonction (AstPlacement.Fonction (info ,_ , bloc)) = 

  let nom, _, _ = triplet_info_fun info in
  (label nom) ^ analyse_code_bloc bloc ^ halt

let analyser (AstPlacement.Programme (fonctions, prog)) = 
  let code_fonctions = concat_code (List.map analyse_code_fonction fonctions) in 
  let code_prog = "main\n" ^ (analyse_code_bloc prog) ^ halt in
  let code_complet = (getEntete ()) ^ code_fonctions ^ code_prog in
  (*
  print_string ("\n \n CODE \n" ^ code_complet ^ "\n  CODE \n \n");
  *)
  code_complet