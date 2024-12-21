open Hashtbl
open Type

(* Définition du type des informations associées aux identifiants *)
type info =
  (* nom, valeur *)
  | InfoConst of string * int
  (* nom, type, déplacement, registre *)
  | InfoVar of string * typ * int * string
  (* nom, type, déplacement, registre, est_initialisée *)
  | InfoStaticVar of string * typ * int * string * bool
  (* nom, type retour, liste des paramètres *)
  | InfoFun of string * typ * typ list * (AstSyntax.defaut option) list

(* Données stockées dans la tds  et dans les AST : pointeur sur une information *)
type info_ast = info ref

(* Table des symboles hiérarchique *)
(* Les tables locales sont codées à l'aide d'une hashtable *)
type tds =
  | Nulle
  (* Table courante : la table mère - la table courante *)
  | Courante of tds * (string,info_ast) Hashtbl.t


(* Créer une information à associer à l'AST à partir d'une info *)
let info_to_info_ast i = ref i

(* Récupère l'information associée à un noeud *)
let info_ast_to_info i = !i

(* Création d'une table des symboles à la racine *)
let creerTDSMere () = Courante (Nulle, Hashtbl.create 100)

(* Création d'une table des symboles fille *)
(* Le paramètre est la table mère *)
let creerTDSFille mere = Courante (mere, Hashtbl.create 100)


(* Ajoute une information dans la table des symboles locale *)
(* tds : la tds courante *)
(* string : le nom de l'identificateur *)
(* info : l'information à associer à l'identificateur *)
(* Si l'identificateur est déjà présent dans TDS, l'information est écrasée *)
(* retour : unit *)
let ajouter tds nom info =
  match tds with
  | Nulle -> failwith "Ajout dans une table vide"
  | Courante (_,c) -> Hashtbl.add c nom info

(* Recherche les informations d'un identificateur dans la tds locale *)
(* Ne cherche que dans la tds de plus bas niveau *)
let chercherLocalement tds nom =
  match tds with
  | Nulle -> None
  | Courante (_,c) ->  find_opt c nom 

(* TESTS *)
let%test _ = chercherLocalement (creerTDSMere()) "x" = None
let%test _ = 
  let tds = creerTDSMere() in
  let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
  let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
  ajouter tds "x" ix;
  ajouter tds "y" iy;
  chercherLocalement tds "x" = Some ix
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    chercherLocalement tds "y" = Some iy
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    chercherLocalement tds "z" = None
let%test _ = 
  let tds = creerTDSMere() in
  let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
  let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
  ajouter tds "x" ix;
  ajouter tds "y" iy;
  let tdsf = creerTDSFille(tds) in
  let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
  let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
  ajouter tdsf "x" ix2;
  ajouter tdsf "z" iz;
  chercherLocalement tds "x" = Some ix
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tds "y" = Some iy
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tds "z" = None
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tdsf "y" = None
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tdsf "x" = Some ix2
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tdsf "z" = Some iz
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherLocalement tdsf "a" = None

(* Recherche les informations d'un identificateur dans la tds globale *)
(* Si l'identificateur n'est pas présent dans la tds de plus bas niveau *)
(* la recherche est effectuée dans sa table mère et ainsi de suite *)
(* jusqu'à trouver (ou pas) l'identificateur *)
let rec chercherGlobalement tds nom =
  match tds with
  | Nulle -> None
  | Courante (m,c) ->
    match find_opt c nom with
      | Some _ as i -> i
      | None -> chercherGlobalement m nom 

(* TESTS *)

let%test _ = chercherGlobalement (creerTDSMere()) "x" = None
let%test _ = 
  let tds = creerTDSMere() in
  let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
  let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
  ajouter tds "x" ix;
  ajouter tds "y" iy;
  chercherGlobalement tds "x" = Some ix
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    chercherGlobalement tds "y" = Some iy
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    chercherGlobalement tds "z" = None
let%test _ = 
  let tds = creerTDSMere() in
  let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
  let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
  ajouter tds "x" ix;
  ajouter tds "y" iy;
  let tdsf = creerTDSFille(tds) in
  let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
  let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
  ajouter tdsf "x" ix2;
  ajouter tdsf "z" iz;
  chercherGlobalement tds "x" = Some ix
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tds "y" = Some iy
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tds "z" = None
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tdsf "y" = Some iy
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tdsf "x" = Some ix2
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tdsf "z" = Some iz
let%test _ = 
    let tds = creerTDSMere() in
    let ix = info_to_info_ast (InfoVar ("x", Rat, 0, "SB")) in
    let iy = info_to_info_ast (InfoVar ("y", Int, 2, "SB")) in
    ajouter tds "x" ix;
    ajouter tds "y" iy;
    let tdsf = creerTDSFille(tds) in
    let ix2 = info_to_info_ast (InfoVar ("x", Bool, 3, "LB")) in
    let iz = info_to_info_ast (InfoVar ("z", Rat, 4, "LB")) in
    ajouter tdsf "x" ix2;
    ajouter tdsf "z" iz;
    chercherGlobalement tdsf "a" = None


(* Convertie une info en une chaine de caractère - pour affichage *)
let string_of_info info =
  match info with
  | InfoConst (n,value) -> "Constante "^n^" : "^(string_of_int value)
  | InfoVar (n,t,dep,base) -> "Variable "^n^" : "^(string_of_type t)^" "^(string_of_int dep)^"["^base^"]"
  | InfoStaticVar (n,t,dep,base,init) -> "Variable statique "^n^" : "^(string_of_type t)^" "^(string_of_int dep)^"["^base^"] initialisée : "^(string_of_bool init)
  | InfoFun (n,t,tp, _) -> "Fonction "^n^" : "^(List.fold_right (fun elt tq -> if tq = "" then (string_of_type elt) else (string_of_type elt)^" * "^tq) tp "" )^
                      " -> "^(string_of_type t)

(* Affiche la tds locale *)
let afficher_locale tds =
  match tds with
  | Nulle -> print_newline ()
  |Courante (_,c) -> Hashtbl.iter ( fun n info -> (print_string (n^" : "^(string_of_info (info_ast_to_info info))^"\n"))) c

(* Affiche la tds locale et récursivement *)
let afficher_globale tds =
  let rec afficher tds indent =
    match tds with
    | Nulle -> print_newline ()
    | Courante (m,c) -> if Hashtbl.length c = 0
                        then print_string (indent^"<empty>\n")
                        else Hashtbl.iter ( fun n info -> (print_string (indent^n^" : "^(string_of_info (info_ast_to_info info))^"\n"))) c ; afficher m (indent^"  ")
  in afficher tds ""

(* Modifie le type si c'est une InfoVar, ne fait rien sinon *)
let modifier_type_variable t i =
    match !i with
    | InfoVar (n,_,dep,base) -> i:= InfoVar (n,t,dep,base)
    | InfoStaticVar (n,_,dep,base, b) -> i:= InfoStaticVar (n,t,dep,base, b)
    | _ -> failwith "Appel modifier_type_variable pas sur un InfoVar"

let%test _ = 
  let info = InfoVar ("x", Undefined, 4 , "SB") in
  let ia = info_to_info_ast info in
  modifier_type_variable Rat ia;
  match info_ast_to_info ia with
  | InfoVar ("x", Rat, 4 , "SB") -> true
  | _ -> false
 
(* Modifie les types de retour et des paramètres si c'est une InfoFun, ne fait rien sinon *)
let modifier_type_fonction t tp i =
       match !i with
       (* On retire les paramètres par défaut car ils sont devenus inutiles après la passe de gestion des ID *)
       | InfoFun(n,_,_,_) -> i:= InfoFun(n,t,tp, [])
       | _ -> failwith "Appel modifier_type_fonction pas sur un InfoFun"

let%test _ = 
  let info = InfoFun ("f", Undefined, [],[]) in
  let ia = info_to_info_ast info in
  modifier_type_fonction Rat [Int ; Int] ia;
  match info_ast_to_info ia with
  | InfoFun ("f", Rat, [Int ; Int], []) -> true
  | _ -> false
 
(* Modifie l'emplacement (dépl, registre) si c'est une InfoVar, ne fait rien sinon *)
 let modifier_adresse_variable d b i =
     match !i with
     |InfoVar (n,t,_,_) -> i:= InfoVar (n,t,d,b)
     |InfoStaticVar (n,t,_,_,estDecl) -> i:= InfoStaticVar (n,t,d,b,estDecl)
     | _ -> failwith "Appel modifier_adresse_variable pas sur un InfoVar ou InfoStaticVar"

let%test _ = 
  let info = InfoVar ("x", Rat, 4 , "SB") in
  let ia = info_to_info_ast info in
  modifier_adresse_variable 10 "LB" ia;
  match info_ast_to_info ia with
  | InfoVar ("x", Rat, 10 , "LB") -> true
  | _ -> false
    
(* Ajout de fonctions supplémentaires *)


(* obtenir_type_info : info_ast -> typ *)
(* Paramètre ia : l'information dont on veut obtenir le type *)
(* Renvoie le type associé à l'information *)
let obtenir_type_info ia = 
  match (info_ast_to_info ia) with
  | InfoFun (_,t,_,_) -> t            (* Si c'est une fonction, on renvoie son type *)
  | InfoVar (_,t,_,_) -> t            (* Si c'est une variable, on renvoie son type *)
  | InfoStaticVar (_,t,_,_,_) -> t    (* Si c'est une variable statique, on renvoie son type *)
  | InfoConst (_,_) -> Int            (* Si c'est une constante, le type est Int *)

(* obtenir_nom_info : info_ast -> string *)
(* Paramètre ia : l'information dont on veut obtenir le type *)
(* Renvoie le nom associé à l'information *)
  let obtenir_nom_info ia = 
    match (info_ast_to_info ia) with
    | InfoFun (n,_,_,_) -> n              (* Si c'est une fonction, on renvoie son nom *)
    | InfoVar (n,_,_,_) -> n            (* Si c'est une variable, on renvoie son nom *)
    | InfoStaticVar (n,_,_,_,_) -> n    (* Si c'est une variable statique, on renvoie son nom *)
    | InfoConst (n,_) -> n              (* Si c'est une constante, on renvoie son nom *)


let info_const ia = match (info_ast_to_info ia) with
  | InfoConst(nom, v) -> (nom, v)
  | _ -> failwith "Mauvaise utilisation de la fonction pour récupérer les infos const"

(* Récupère le nom d'une fonction *)

let info_fun ia = match (info_ast_to_info ia) with
  | InfoFun(nom, t, tp, lp) -> (nom, t, tp, lp)
  | _ -> failwith "Mauvaise utilisation de la fonction pour récupérer les infos fun"

(* Renvoie si une Variable static a déjà été initialisée et les autres infos *)
let info_static_var ia = match (info_ast_to_info ia) with
| InfoStaticVar(nom, t, d, reg, b) -> (nom, t, d, reg,b)
| _ -> failwith "Mauvaise utilisation de la fonction pour récupérer les infos d'une variable statique"

(* Récupère  directement le quadruplet d'information d'un InfoVar, InfoStaticVar ou InfoFun*)
let info_var ia = match (info_ast_to_info ia) with
  | InfoVar(nom, t, d, reg) -> (nom, t, d, reg, false)
  | InfoStaticVar _ -> info_static_var ia
  | _ -> failwith "Mauvaise utilisation de la fonction pour récupérer les infos var"

(* Modifie la valeur du booleen d'une variable statique pour noter la déclaration *)
let declaration_variable estDeclaree i =
  match !i with
  | InfoVar _ -> ()
  | InfoStaticVar (n,t,d,b,_) -> i:= InfoStaticVar (n,t,d,b, estDeclaree)
  | _ -> failwith "Appel modifier_booleen_variable_statique pas sur un InfoStaticVar"

let rec obtenir_tds_originelle tds = match tds with
  | Courante (mere, _) -> 
    if (mere == Nulle) then tds else 
      obtenir_tds_originelle mere
  | Nulle ->  Nulle