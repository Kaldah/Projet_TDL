(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


let rec afficher_expression_ast e = 
  begin
  match e with
    | AstSyntax.AppelFonction (nomf, params) -> 
      print_string (nomf^" : \n");
      (List.iter afficher_expression_ast params);
      print_string ("fin appel fonction : "^nomf);
      | AstSyntax.Entier (ent) -> print_int ent
      | AstSyntax.Ident (nom) -> print_string nom
      | AstSyntax.Booleen (b) -> if b then print_string "True" else print_string "False"
      | AstSyntax.Unaire (op, e) ->
      begin
      if (op = Numerateur) then 
      print_string "Numerateur :" 
      else 
      print_string "Denominateur : "
      end;
      afficher_expression_ast e;
      | Binaire(_, e1, e2) -> print_string "Binaire : ";
      afficher_expression_ast e1; 
      afficher_expression_ast e2;
    end;
    print_newline ();;

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e = 
match e with
  | AstSyntax.AppelFonction(n, l) -> 
    begin
      match chercherGlobalement tds n with
        | None -> 
          (* L'identifiant n'est pas trouvé dans la tds globale,
          il n'a donc pas été déclaré dans le programme *)
          raise (Exceptions.IdentifiantNonDeclare n)      
          (* L'identifiant existe donc on récupère et renvoie la référence sur l'info associée *)
        | Some info_tds -> 
          begin
          match (info_ast_to_info info_tds) with
          | InfoFun(_, _, types_param) ->
            
            if (List.length types_param = List.length l) then
            let dts_l = List.map (analyse_tds_expression tds) l in
            AstTds.AppelFonction(info_tds, dts_l)
            else raise (MauvaiseUtilisationIdentifiant n)
          | _ -> raise (MauvaiseUtilisationIdentifiant n)
          end
      end
  | AstSyntax.Ident(n) -> 
    begin
    match chercherGlobalement tds n with
      | None -> 
        (* L'identifiant n'est pas trouvé dans la tds globale,
        il n'a donc pas été déclaré dans le programme *)
        raise (Exceptions.IdentifiantNonDeclare n)      
        (* L'identifiant existe donc on récupère et renvoie la référence sur l'info associée *)
      | Some info -> match (info_ast_to_info info) with
      | InfoFun(n,_,_) -> raise (MauvaiseUtilisationIdentifiant n)
      | _ -> AstTds.Ident(info)
    end
  | AstSyntax.Booleen(b) -> AstTds.Booleen(b)
  | AstSyntax.Entier(ent) -> AstTds.Entier(ent)
  | AstSyntax.Unaire(op, e1) -> let exp = analyse_tds_expression tds e1 in AstTds.Unaire(op, exp)
  | AstSyntax.Binaire(op, e1, e2) -> 
    let exp1 = analyse_tds_expression tds e1 in 
    let exp2 = analyse_tds_expression tds e2 in AstTds.Binaire(op, exp1, exp2)

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (n,e) ->
      begin
        match chercherGlobalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds globale. *)
          raise (IdentifiantNonDeclare n)
        | Some info ->
          (* L'identifiant est trouvé dans la tds globale,
          il a donc déjà été déclaré. L'information associée est récupérée. *)
          begin
            match info_ast_to_info info with
            | InfoVar _ ->
              (* Vérification de la bonne utilisation des identifiants dans l'expression *)
              (* et obtention de l'expression transformée *)
              let ne = analyse_tds_expression tds e in
              (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
                 et l'expression remplacée par l'expression issue de l'analyse *)
              AstTds.Affectation (info, ne)
            |  _ ->
              (* Modification d'une constante ou d'une fonction *)
              raise (MauvaiseUtilisationIdentifiant n)
          end
      end
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          (* L'identifiant n'est pas trouvé dans la tds locale,
             il n'a donc pas été déclaré dans le bloc courant *)
          (* Ajout dans la tds de la constante *)
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds oia t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds oia e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds oia b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
      (* On récupère l'information associée à la fonction à laquelle le return est associée *)
      match oia with
        (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
      | None -> raise RetourDansMain
        (* Il y a une information -> l'instruction est dans une fonction *)
      | Some ia ->
        (* Analyse de l'expression *)
        let ne = analyse_tds_expression tds e in
        AstTds.Retour (ne,ia)
      end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  match chercherGlobalement maintds n with
  | Some _ -> 
    (* La fonction a déjà été déclarée *)
    raise (Exceptions.DoubleDeclaration n)

  | None -> (* La fonction n'a pas encore été déclarée *)
  (* On récupère les infos *)
  (* Récupère les types dans lp *)
  let tlp = List.map fst lp in
  let info_fun = info_to_info_ast (InfoFun(n, t, tlp)) in
  ajouter maintds n info_fun;
  (* On récupère la liste (type * Tds.info_ast) des paramètres *)
  (* On crée une TDS fille de mainTDS pour contenir les paramètres de la fonction *)
  let tds_param = creerTDSFille maintds in 

  let aux_infos_param (t, p) = 
    (* créer une référence pour le paramètre dans la table *)
    let info = InfoVar(p, t, 0, "") in
    let ref_param = info_to_info_ast info in
    begin
    (* On ajoute la référence dans la TDS fille *)
    let test_info_p = chercherLocalement tds_param p in
    match test_info_p with
    | Some _ -> raise (DoubleDeclaration p)
    | _ -> ajouter tds_param p ref_param;
    (* On renvoie le couple du type et la référence associée pour les récupérer *)
    (t, ref_param);
    end
  in
  (* On utilise la fonction aux_infos_param pour initialiser la TDS fille *)
  (* et on construit la liste des paramètres avec leur type et la référence de leurs infos dans la TDS fille *)
  let infos_param = List.map aux_infos_param lp in

  let bloc = analyse_tds_bloc tds_param (Some info_fun) li in
  AstTds.Fonction (t, info_fun, infos_param, bloc)



(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf,nb)
