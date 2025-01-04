open Rat
open Tds
open Ast
open PassePlacementRat
open Type 


let %test _= separer_declaration_static [] = ([],[])

let %test _= 
  let li= [AstPlacement.Declaration(info_to_info_ast(InfoFun("n",Int,[],[])),Entier(5)) ; 
            AstPlacement.Declaration(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5))] in 
  separer_declaration_static li = ([AstPlacement.Declaration(info_to_info_ast(InfoFun("n",Int,[],[])),Entier(5)) ; 
  AstPlacement.Declaration(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5))],[])


  let %test _= 
  let li= [AstPlacement.Declaration(info_to_info_ast(InfoFun("n",Int,[],[])),Entier(5)) ; 
            AstPlacement.Declaration(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5));
            AstPlacement.DeclarationStatic(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5))] in 
  separer_declaration_static li = ([AstPlacement.Declaration(info_to_info_ast(InfoFun("n",Int,[],[])),Entier(5)) ; 
  AstPlacement.Declaration(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5))],[AstPlacement.DeclarationStatic(info_to_info_ast(InfoVar("n",Int,0,"Sb")),Entier(5))])


let %test _= 
  traiter_parametres_fonction 0 []= ()

let %test _= 
  try 
    let info = info_to_info_ast (InfoFun ("add", Rat, [], [])) in
    let _= traiter_parametres_fonction 0 [info] in false 
with 
  |Failure _-> true
  |_-> false 

let %test _= 
  let info1= info_to_info_ast(InfoVar("n",Int, 1, "SB")) in 
  let info2= info_to_info_ast(InfoVar( "n2",Rat,2,"SB")) in 
  traiter_parametres_fonction 3 [info1;info2];
  (match info_ast_to_info info1 with
  |InfoVar("n",Int, 2, "LB")-> true
  |_-> false )
  &&
  (match info_ast_to_info info2 with
  |InfoVar( "n2",Rat, 0, "LB")-> true
  |_-> false)

  let %test _= 
  let info1= info_to_info_ast(InfoVar("n",Bool, 1, "SB")) in 
  let info2= info_to_info_ast(InfoVar( "n2",(Pointeur Int),2,"SB")) in 
  traiter_parametres_fonction 2 [info1;info2];
  (match info_ast_to_info info1 with
  |InfoVar("n",Bool, 1, "LB")-> true
  |_-> false )
  &&
  (match info_ast_to_info info2 with
  |InfoVar( "n2",(Pointeur Int), 0, "LB")-> true
  |_-> false)

  let %test _= 
  let info1= info_to_info_ast(InfoVar("n",Undefined, 1, "SB")) in 
  let info2= info_to_info_ast(InfoVar( "n2",Null,2,"SB")) in 
  traiter_parametres_fonction 0 [info1;info2];
  (match info_ast_to_info info1 with
  |InfoVar("n",Undefined, 0, "LB")-> true
  |_-> false )
  &&
  (match info_ast_to_info info2 with
  |InfoVar( "n2",Null, 0, "LB")-> true
  |_-> false)