open Rat
open Tds
open Ast
open PasseTdsRat
open Type

(* Définitions de variables pour les tests *)
let tds = creerTDSMere ()

let a = AstSyntax.Ident("a")

exception ErreurNonDetectee

(* Exemple de test pour `info_fun` *)

let%test "analyse_tds_affectable1" = 
  let ia = info_to_info_ast (InfoVar("a", Int, 0, "rbp")) in
  ajouter tds "a" ia;
  let res = analyse_tds_affectable tds a true in
  match res with
  | Affectable (AstTds.Ident(n)) -> (n = ia)
  | _ -> false


let %test _= verifier_param [] true = []

let %test _= 
  let l= [(Int,"n",None);(Int,"n",None);(Int,"n",None)] in 
  verifier_param l false=[None;None;None]

  let %test _= 
    let l= [(Int,"n",Some (AstSyntax.Defaut(Entier(5))));(Pointeur(Int),"n",Some (AstSyntax.Defaut(New(Int))));(Bool,"n",Some(AstSyntax.Defaut(Booleen(true))))] in 
    verifier_param l false=[Some (AstSyntax.Defaut(Entier(5)));Some (AstSyntax.Defaut(New(Int)));Some(AstSyntax.Defaut(Booleen(true)))]

  let %test _= 
    let l= [(Int,"n",None);(Int,"n",Some (AstSyntax.Defaut(Entier(5))));(Pointeur(Int),"n",Some (AstSyntax.Defaut(New(Int))));(Bool,"n",Some(AstSyntax.Defaut(Booleen(true))))] in
    verifier_param l false=[None; Some (AstSyntax.Defaut(Entier(5)));Some (AstSyntax.Defaut(New(Int)));Some(AstSyntax.Defaut(Booleen(true)))]

  let %test_unit _=
    try 
      let l= [(Int,"n",Some (AstSyntax.Defaut(Entier(5))));(Int, "n",None);(Pointeur(Int),"n",Some (AstSyntax.Defaut(New(Int))));(Bool,"n",Some(AstSyntax.Defaut(Booleen(true))))] in 
      let _= verifier_param l false in
      raise ErreurNonDetectee
    with
    |Exceptions.ParametreObligatoireInterdit("n")-> ()

let %test _= completer_arguments tds [] [] = []

let %test_unit _= 
    try 
      let l= [Some (AstSyntax.Defaut(Entier(5)))] in
      let li= [AstTds.Entier(5); AstTds.Booleen(true); AstTds.Entier(2)] in 
      let _= completer_arguments (creerTDSMere()) l li in 
      raise ErreurNonDetectee
  with 
  |Exceptions.TypesParametresInattendus ([], [])-> ()

  let %test_unit _= 
    try 
      let l= [Some (AstSyntax.Defaut(Entier(5))); None] in
      let li= [AstTds.Entier(5)] in 
      let _= completer_arguments (creerTDSMere()) l li in 
      raise ErreurNonDetectee
  with 
  |Exceptions.TypesParametresInattendus ([], [])-> ()

  let %test _=  
    let tdsM= creerTDSMere() in 
    let l= [Some (AstSyntax.Defaut(Entier(5))); None] in
    let li= [AstTds.Entier(5); AstTds.Booleen(true)] in 
    completer_arguments tdsM l li = li

  let %test _=  
    let tdsM= creerTDSMere() in 
    let l= [Some (AstSyntax.Defaut(Entier(5))); None; Some(AstSyntax.Defaut(Booleen(true)))] in
    let li= [AstTds.Entier(5); AstTds.Booleen(true)] in 
    completer_arguments tdsM l li = [AstTds.Entier(5); AstTds.Booleen(true); AstTds.Booleen(true)]

  let %test _=  
    let tdsM= creerTDSMere() in 
    let l= [Some (AstSyntax.Defaut(Entier(5))); None; Some(AstSyntax.Defaut(Binaire(Plus,Entier(5),Entier(2))))] in
    let li= [AstTds.Entier(5); AstTds.Booleen(true)] in 
    completer_arguments tdsM l li = [AstTds.Entier(5); AstTds.Booleen(true); AstTds.Binaire(Plus,Entier(5),Entier(2))]

let %test _= 
  let elem= (Int,"n", None) in 
  let elem_info= info_to_info_ast(InfoVar("n",Int,0,"")) in
  let res= infos_param tds elem in
  (res= (Int,elem_info )) && (chercherGlobalement tds "n")= (Some elem_info)

  let %test _= 
  let elem= (Int,"m", None) in 
  let elem_info= info_to_info_ast(InfoVar("m",Int,0,"")) in
  let elem_info_false= info_to_info_ast(InfoVar("p",Int,0,"")) in
  let res= infos_param tds elem in
  (res= (Int,elem_info )) && not ((chercherGlobalement tds "n")= (Some elem_info_false))

let %test_unit _= 
    try 
      let elem= (Int,"n", None) in 
      let _= infos_param tds elem in 
      raise ErreurNonDetectee
  with 
    | Exceptions.DoubleDeclaration("n")->()

