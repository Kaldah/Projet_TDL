open Rat
open Tds
open Ast
open PasseCodeRatToTam

let %test _= 
  let infoast= info_to_info_ast(InfoVar("n",Rat, 0, "SB")) in 
  get_type_affectable(AstTds.Ident(infoast))= Rat

let %test _= 
  let infoast= info_to_info_ast(InfoConst("n",2)) in 
  get_type_affectable(AstTds.Ident(infoast))= Int 

let %test _= 
  try 
    let infoast= info_to_info_ast(InfoFun("add",Rat, [], [])) in 
    let _=get_type_affectable(AstTds.Ident(infoast)) in false 
with
  |Failure _-> true 
  |_-> false 

  let %test _= 
  let infoast = info_to_info_ast(InfoVar("n",(Pointeur Int), 0, "SB")) in 
  let de= AstTds.Deref(AstTds.Ident(infoast)) in 
  get_type_affectable de =  Int

let %test _= 
  let infoast = info_to_info_ast(InfoVar("n",(Pointeur(Pointeur Int)), 0, "SB")) in 
  let de= AstTds.Deref(AstTds.Deref(AstTds.Ident(infoast))) in 
  get_type_affectable de = Int

  let %test _= 
  let infoast = info_to_info_ast(InfoVar("n",Pointeur((Pointeur(Pointeur Int))), 0, "SB")) in 
  let de= AstTds.Deref(AstTds.Deref(AstTds.Deref(AstTds.Ident(infoast)))) in 
  get_type_affectable de = Int


  let %test _= 
  try 
    let infoast = info_to_info_ast(InfoVar("n",Int, 0, "SB")) in 
    let de= AstTds.Deref(AstTds.Deref(AstTds.Ident(infoast))) in
    let _=get_type_affectable de in false 
with
  |Failure _-> true 
  |_-> false 

  let %test _= 
  try 
    let infoast = info_to_info_ast(InfoVar("n",(Pointeur Int), 0, "SB")) in 
    let de= AstTds.Deref(AstTds.Deref(AstTds.Ident(infoast))) in
    let _=get_type_affectable de in false 
with
  |Failure _-> true 
  |_-> false 