(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast
(* open PrinterAst.PrinterAstSyntax *)

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


(* Types permettant à analyse_tds_affectable de renvoyer un résultat sous la forme d'expressions ou d'affectables *)
(* Cela évite de garder les Entier sous la forme d'Affectable *)
type affouexpTds =
  | Affectable of AstTds.affectable
  | Expression of AstTds.expression


(* analyse_tds_affectable : tds -> AstSyntax.affectable -> bool -> affouexpTds *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectable à analyser *)
(* Paramètre en_ecriture : le mode, vaut "true" en lecture et "false" en ecriture *)
(* Vérifie la bonne utilisation des identifiants des affectables et tranforme l'expression *)
(* en une expression de type AstTds.expression *)
let rec analyse_tds_affectable tds a en_ecriture =
  match a with
  | AstSyntax.Ident id ->
    begin
      match chercherGlobalement tds id with
      (* L'identifiant n'est pas trouvé dans la tds globale. *)
      | None -> raise (IdentifiantNonDeclare id)
      | Some ia ->
        (* L'identifiant est trouvé dans la tds globale, *)
        (* il a donc déjà été déclaré. L'information associée est récupérée. *)
        begin
          match info_ast_to_info ia with
          | InfoVar _ -> Affectable (AstTds.Ident ia)
          | InfoConst (n, ent) ->
            if en_ecriture then
              (* Modification d'une constante ou d'une fonction *)
              raise (Exceptions.MauvaiseUtilisationIdentifiant n)
            else
              Expression (AstTds.Entier(ent))
          | _ -> raise (MauvaiseUtilisationIdentifiant id)
        end
    end
  | AstSyntax.Deref a ->
    let na = analyse_tds_affectable tds a false in
    begin
      match na with
      | Affectable(AstTds.Ident ia) -> Affectable (AstTds.Deref (AstTds.Ident ia))
      | Affectable(AstTds.Deref a2) -> Affectable (AstTds.Deref (AstTds.Deref a2))
      | Expression (AstTds.Entier ent) -> Expression (AstTds.Entier ent)
      | _ -> failwith "Erreur de déréférencement"
    end

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
          | InfoFun(_, _, _,lod) ->
            let tds_l = List.map (analyse_tds_expression tds) l in
            (* On complète les paramètres avec les paramètres par défaut *)
            let ntds_l = completer_arguments tds lod tds_l in
            AstTds.AppelFonction(info_tds, ntds_l)
          | _ -> raise (MauvaiseUtilisationIdentifiant n)
          end
      end

  | AstSyntax.Affectable(a) ->
    begin
      match analyse_tds_affectable tds a false with
      | Affectable na ->  AstTds.Affectable na
      | Expression e -> e
    end
  | AstSyntax.Null -> AstTds.Null
  | AstSyntax.New(t) -> AstTds.New(t)
  | AstSyntax.Adresse(id) ->
    begin
      match (chercherGlobalement tds id) with
      | None -> raise (Exceptions.IdentifiantNonDeclare id)
      | Some ia ->
        (
          match info_ast_to_info ia with
          | InfoVar _ -> AstTds.Adresse ia
          | _ -> raise (MauvaiseUtilisationIdentifiant id)
        )
      end
  | AstSyntax.Booleen(b) -> AstTds.Booleen(b)
  | AstSyntax.Entier(ent) -> AstTds.Entier(ent)
  | AstSyntax.Unaire(op, e1) -> let exp = analyse_tds_expression tds e1 in AstTds.Unaire(op, exp)
  | AstSyntax.Binaire(op, e1, e2) ->
    let exp1 = analyse_tds_expression tds e1 in
    let exp2 = analyse_tds_expression tds e2 in AstTds.Binaire(op, exp1, exp2)


(* completer_arguments : tds -> AstTds.expression list -> AstTds.expression list *)
(* Paramètre tds : la table des symboles courante - celle du bloc dans lequel la fonction est appelée *)
(* Paramètre params : les paramètres attendus de la fonction *)
(* Paramètre args : les arguments effectivement passés à la fonction *)
(* Vérifie la bonne utilisation de la fonction et ajoute aux arguments *)
(* la valeur des paramètres par défaut, y compris la valeur des variables globales *)
(* Erreur s'il manque un argument obligatoire ou s'il y a trop d'arguments *)
and completer_arguments tds params args =
  (* On récupère la tds originelle car les valeurs par défauts sont enregistrées dedans, sinon elles ne seraient pas accessibles *)
  (* car les fonctions et leurs paramètres sont définis dans la tds originelle *)
  let tdsOriginelle = obtenir_tds_originelle tds in
  let rec aux params args =
    match (params, args) with
    | [], [] -> [] (* Aucun paramètre restant, aucun argument manquant *)
    | (Some (AstSyntax.Defaut(e)))::q, [] ->
        (* Paramètre avec valeur par défaut et argument manquant => utiliser la valeur par défaut *)
        (analyse_tds_expression tdsOriginelle e) :: aux q []
    | None :: _, [] ->
        (* Paramètre obligatoire manquant : Exceptions.ParametreObligatoireManquant*)
        raise (Exceptions.TypesParametresInattendus ([], []))
    | _:: q, arg :: args_q ->
        (* Argument fourni : on continue *)
        arg :: aux q args_q
        (* Trop d'arguments : Exceptions.TropArguments *)
    | [], _ :: _ -> raise (Exceptions.TypesParametresInattendus ([], []))
      in
  aux params args
    

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
            let info = InfoVar (n, t, 0, "") in
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
    | AstSyntax.DeclarationStatic (n, t, e) ->
      let ne = analyse_tds_expression tds e in
        let info = InfoVar(n, t, 0, "") in
          let ia = info_to_info_ast info in
            ajouter tds n ia;
            AstTds.DeclarationStatic (t, ia, ne)

    | AstSyntax.Affectation (a,e) ->
      let nae = analyse_tds_affectable tds a true in
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      begin
      match nae with
      (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
      et l'expression remplacée par l'expression issue de l'analyse *)
        | Affectable(na) -> AstTds.Affectation(na, ne)
        | Expression _ -> failwith "Erreur interne Affectation"
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

(* verifier_param : ('a * string * 'b option) list -> bool -> 'b option list *)
(* dans les faits - verifier_param : (typ * string * AstSyntax.defaut option) list -> bool -> AstSyntax.defaut option list *)
(* Paramètre params : la liste des paramètres attendu avec leurs types et possiblement une valeur par défaut *)
(* Paramètre defaut_present : boolen permettant de savoir si on a déjà un paramètre avec une valeur par défaut dans la liste*)
(* Vérifie la bonne utilisation des paramètres par défaut *)
(* Erreur si mauvaise utilisation des paramètres par défaut *)
let rec verifier_param params defaut_present =
  match params with
    | [] -> []
    (* Il y a un paramètre donné sans valeur par défaut alors qu'on a commencé à en avoir *)
    | (_, p, None)::_ when defaut_present -> raise (Exceptions.ParametreObligatoireInterdit p)
    (* Il y a un paramètre donné avec une valeur par défaut *)
    | (_,_,Some d )::q -> (Some d)::(verifier_param q true)
    (* Il y a un paramètre donné sans valeur par défaut *)
    | (_,_,None)::q -> None::(verifier_param q false)

(* infos_param : tds -> typ * string * 'a option -> typ * info_ast *)
(* Paramètre tds_param : la table des symboles courante *)
(* Paramètre (t, p, _) : un tuple contenant le type, le nom et une option pour le paramètre *)
(* Crée une référence pour le paramètre dans la table des symboles et ajoute cette référence à la table *)
(* Renvoie un couple du type et de l'info_ast associée *)

let infos_param tds_param (t, p, _) =
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
  let tlp = List.map (fun (x, _, _) -> x) lp in
  (* On vérifie que les paramètres par défauts sont bien utilisés et on récupère la liste des defaut option *)
  (* C'est donc la liste des valeurs par défaut sous forme d'option avec None s'il n'y en a pas *)
  let lod = verifier_param lp false in

  (* On crée l'information associée à la fonction *)
  let info_fun = info_to_info_ast (InfoFun(n, t, tlp,lod)) in
  ajouter maintds n info_fun;
  (* On récupère la liste (type * Tds.info_ast) des paramètres *)
  (* On crée une TDS fille de mainTDS pour contenir les paramètres de la fonction *)
  let tds_param = creerTDSFille maintds in

  (* On utilise la fonction aux_infos_param pour initialiser la TDS fille *)
  (* et on construit la liste des paramètres avec leur type et la référence de leurs infos dans la TDS fille *)
  let infos_param = List.map (infos_param tds_param) lp in
  (* On analyse le bloc de la fonction *)
  let bloc = analyse_tds_bloc tds_param (Some info_fun) li in
  AstTds.Fonction (t, info_fun, infos_param, bloc)

(* analyse_tds_variable_globale : tds -> AstSyntax.variable_globale -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre AstSyntax.DeclarationGlobale(t,n,e): la déclaration d'une variable globale à analyser *)
(* Vérifie la bonne utilisation des identifiants et transforme la variable globale *)
let analyse_tds_variable_globale tds (AstSyntax.DeclarationGlobale(t, n, e)) =
  match chercherLocalement tds n with
  | None ->
    let ne = analyse_tds_expression tds e in
    let info = InfoVar(n, t, 0, "") in
    let ia = info_to_info_ast info in
    ajouter tds n ia;
    AstTds.Declaration(t, ia, ne)
  | Some _ -> raise (DoubleDeclaration n)


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (lg, fonctions,prog)) =
  let tds = creerTDSMere () in
  let nlg = List.map (analyse_tds_variable_globale tds) lg in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nlg, nf, nb)
