open Rat
open Tds
open Ast
open PasseTypeRat
open Type

exception ErreurNonDetectee

(* Exemple de test pour `analyse_type_affectable` *)

let%test "analyse_type_affectable1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let a = AstTds.Ident(ia) in
  let (na, ta) = analyse_type_affectable a in
  match na with
  | AstTds.Ident(n) -> (n = ia) && (ta = Int)
  | _ -> false

let%test "analyse_type_affectable2" = 
  let ia = info_to_info_ast (InfoVar("a", Pointeur(Int), 0, "rbp")) in
  let a = AstTds.Deref(AstTds.Ident(ia)) in
  let (na, ta) = analyse_type_affectable a in
  match na with
  | AstTds.Deref(AstTds.Ident(n)) -> (n = ia) && (ta = Int)
  | _ -> false

(* Exemple de test pour `analyse_type_expression` *)

let%test "analyse_type_expression1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let e = AstTds.Affectable(AstTds.Ident(ia)) in
  let (ne, te) = analyse_type_expression e in
  match ne with
  | AstType.Affectable(AstTds.Ident(n)) -> (n = ia) && (te = Int)
  | _ -> false

let%test "analyse_type_expression2" = 
  let ia = info_to_info_ast (InfoVar("a", Pointeur(Int), 0, "rbp")) in
  let e = AstTds.Affectable(AstTds.Deref(AstTds.Ident(ia))) in
  let (ne, te) = analyse_type_expression e in
  match ne with
  | AstType.Affectable(AstTds.Deref(AstTds.Ident(n))) -> (n = ia) && (te = Int)
  | _ -> false

(* Exemple de test pour `analyse_type_instruction` *)

let%test "analyse_type_instruction1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let e = AstTds.Entier(5) in
  let i = AstTds.Declaration(Int, ia, e) in
  let ni = analyse_type_instruction i in
  match ni with
  | AstType.Declaration(n, AstType.Entier(5)) -> (n = ia)
  | _ -> false

let%test "analyse_type_instruction2" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let e = AstTds.Entier(5) in
  let i = AstTds.Affectation(AstTds.Ident(ia), e) in
  let ni = analyse_type_instruction i in
  match ni with
  | AstType.Affectation(AstTds.Ident(n), AstType.Entier(5)) -> (n = ia)
  | _ -> false

(* Exemple de test pour `analyse_type_variable_globale` *)

let%test "analyse_type_variable_globale1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let e = AstTds.Entier(5) in
  let vg = AstTds.DeclarationGlobale(ia, e) in
  let nvg = analyse_type_variable_globale vg in
  match nvg with
  | AstType.DeclarationGlobale(n, AstType.Entier(5)) -> (n = ia)
  | _ -> false

(* Exemple de test pour `analyser` *)

let%test "analyser1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  let e = AstTds.Entier(5) in
  let vg = AstTds.Declaration(Int, ia, e) in
  let iaf = info_to_info_ast (InfoFun("f", Int, [Int], [Some (Defaut (Entier 10))])) in
  let p = (Int, info_to_info_ast (InfoVar("t", Bool, 0, "SB"))) in
  let p2 = (Rat, info_to_info_ast (InfoVar("a", Rat, 0, "rbp"))) in
  let li = [AstTds.Retour(AstTds.Entier(5), iaf)] in
  let f = AstTds.Fonction(Int, iaf, [p;p2], li) in
  let prog = AstTds.Programme([vg], [f], []) in
  let nprog = analyser prog in
  match nprog with
  | AstType.Programme([AstType.Declaration(n, AstType.Entier(5))], [AstType.Fonction(nf, _, [AstType.Retour(AstType.Entier(5), _)])], []) -> (n = ia) && (nf = iaf)
  | _ -> false

