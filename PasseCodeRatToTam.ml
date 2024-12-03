(* Module de la passe de génération de code *)
(* doit être conforme à l'interface Passe *)

open Tam
open Tds
open Ast
open Code
open Type

type t1 = Ast.AstPlacement.programme
type t2 = string

let rec analyse_code_expression e =
  match e with
  | AstType.AppelFonction (info, le ) -> 
    (* On charge toutes les expressions de le *)
    let liste_code = List.map analyse_code_expression le in
    let code_exp = List.fold_left ( ^ ) "" liste_code in 
    let nom, _, _ = triplet_info_fun info in
    code_exp ^ (call "SB" nom)

  | AstType.Ident info -> "" (* load (taille type) @var (voir dans info) *)
  | AstType.Booleen b ->
    if b then 
      loadl_int 1
    else 
      loadl_int 0
  | AstType.Entier i -> loadl_int i
  | AstType.Unaire (op, e) -> let code_e = analyse_code_expression e in
    (
      match op with
        | Numerateur -> code_e ^ (pop (0) 1)
        | Denominateur -> code_e ^ (pop (1) 1)
    ) 
  | AstType.Binaire (op, e1, e2) -> 
    let code_e1 = analyse_code_expression e1 in
    let code_e2 = analyse_code_expression e2 in
    let code = code_e1 ^ code_e2 in
  (
    match op with
      | Fraction -> code ^ ""
      | PlusInt -> code ^ subr "IAdd"
      | PlusRat -> code ^ call "SB" "RAdd"
      | MultInt -> code ^ subr "IMul"
      | MultRat -> code ^ subr "RMul"
      | EquBool -> code ^ subr "BAnd"
      | EquInt -> code ^ subr "IEq"
      | Inf -> code ^ (subr "ILss")
  ) 

  let rec analyse_code_instruction i =
    match i with
    | AstPlacement.Declaration ( info , e) -> 
    (* PUSH (taille du type) *)
    analyse_code_expression e ^ (store 0 0 "@var")
    | AstPlacement.Affectation ( info , e) -> analyse_code_expression e ^ (store 0 0 "@var")
    | AstPlacement.AffichageInt e -> analyse_code_expression e ^ (subr "IOut")
    | AstPlacement.AffichageRat e -> ""
    | AstPlacement.AffichageBool e -> ""

    | AstPlacement.Conditionnelle (c, t , e) -> 
      let etiquetteE = getEtiquette () in 
      let etiquetteFin = getEtiquette () in
      let code_e = analyse_code_expression c in
      let code_bloc_t = analyse_code_bloc t in
      let code_bloc_e = analyse_code_bloc e in
      code_e ^ (jumpif (0) etiquetteE) ^ code_bloc_t ^ (jump etiquetteFin) ^ etiquetteE ^ code_bloc_e ^ etiquetteFin

    | AstPlacement.TantQue (c, b) -> 
      let etiquetteTantQue = getEtiquette () in 
      let etiquetteFin = getEtiquette () in
      let code_condition = analyse_code_expression c in
      let code_bloc = analyse_code_bloc b in
      etiquetteTantQue ^ code_condition ^ (jumpif (0) etiquetteFin) ^ code_bloc ^ (jump etiquetteTantQue) ^ etiquetteFin
    
    | AstPlacement.Retour (e, tailleRet , tailleParam) -> (analyse_code_expression e) ^ (return tailleRet tailleParam)
    | AstPlacement.Empty -> ""
    and analyse_code_bloc (li , taille ) = "";;

let analyse_code_fonction (AstPlacement.Fonction (info ,_ , ( li , _ ))) = ""

let analyser (AstPlacement.Programme (fonctions, prog)) = ""