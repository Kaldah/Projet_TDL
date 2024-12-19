open Type

(* Interface des arbres abstraits *)
module type Ast =
sig
   type expression
   type instruction
   type fonction
   type programme
   type affectable
   type variable_globale
end


(* *************************************** *)
(* AST après la phase d'analyse syntaxique *)
(* *************************************** *)

module AstSyntax = AstSyntax


(* ********************************************* *)
(* AST après la phase d'analyse des identifiants *)
(* ********************************************* *)
module AstTds =
struct

type affectable = 
  (* Accès à un identifiant représenté par son nom *)
 | Ident of Tds.info_ast (* le nom de l'identifiant est remplacé par ses informations *)
  (* Déréférencement d'un identifiant *)
 | Deref of affectable


  (* Expressions existantes dans notre langage *)
  (* ~ expression de l'AST syntaxique où les noms des identifiants ont été
  remplacés par les informations associées aux identificateurs *)
  type expression =
    | AppelFonction of Tds.info_ast * expression list
    | Booleen of bool
    | Entier of int
    | Unaire of AstSyntax.unaire * expression
    | Binaire of AstSyntax.binaire * expression * expression
    | Affectable of affectable
    | New of typ
    | Adresse of Tds.info_ast
    | Null

(* le nom de l'identifiant est remplacé par ses informations *)
type variable_globale = DeclarationGlobale of Tds.info_ast * expression

type defaut = Defaut of expression

  (* instructions existantes dans notre langage *)
  (* ~ instruction de l'AST syntaxique où les noms des identifiants ont été
  remplacés par les informations associées aux identificateurs
  + suppression de nœuds (const) *)
  type bloc = instruction list
  and instruction =
    | DeclarationStatic of Tds.info_ast * expression
    | Declaration of typ * Tds.info_ast * expression (* le nom de l'identifiant est remplacé par ses informations *)
    | Affectation of  affectable * expression (* le nom de l'identifiant est remplacé par ses informations *)
    | Affichage of expression
    | Conditionnelle of expression * bloc * bloc
    | TantQue of expression * bloc
    | Retour of expression * Tds.info_ast  (* les informations sur la fonction à laquelle est associé le retour *)
    | Empty (* les nœuds ayant disparus: Const *)


  (* Structure des fonctions dans notre langage *)
  (* type de retour - informations associées à l'identificateur (dont son nom) - liste des paramètres (association type et information sur les paramètres) - corps de la fonction *)
  type fonction = Fonction of typ * Tds.info_ast * (typ * Tds.info_ast ) list * bloc

  (* Structure d'un programme dans notre langage *)
  (* Le premier bloc ne contient que des instructions de déclaration de variables globales *)
  type programme = Programme of bloc * fonction list * bloc

end


(* ******************************* *)
(* AST après la phase de typage *)
(* ******************************* *)
module AstType =
struct

(* Opérateurs unaires de Rat - résolution de la surcharge *)
type unaire = Numerateur | Denominateur

(* Opérateurs binaires existants dans Rat - résolution de la surcharge *)
type binaire = Fraction | PlusInt | PlusRat | MultInt | MultRat | EquInt | EquBool | Inf

type affectable = AstTds.affectable

(* Expressions existantes dans Rat *)
(* = expression de AstTds *)
type expression =
  | AppelFonction of Tds.info_ast * expression list
  | Booleen of bool
  | Entier of int
  | Unaire of unaire * expression
  | Binaire of binaire * expression * expression
  | Affectable of AstTds.affectable
  | New of typ
  | Adresse of Tds.info_ast
  | Null

type variable_globale = DeclarationGlobale of Tds.info_ast * expression

type defaut = Defaut of expression


(* instructions existantes Rat *)
(* = instruction de AstTds + informations associées aux identificateurs, mises à jour *)
(* + résolution de la surcharge de l'affichage *)
type bloc = instruction list
 and instruction =
  | DeclarationStatic of Tds.info_ast * expression
  | Declaration of Tds.info_ast * expression
  | Affectation of affectable * expression
  | AffichageInt of expression
  | AffichageRat of expression
  | AffichageBool of expression
  | AffichagePointeur of expression * typ
  | AffichageNull of expression
  | Conditionnelle of expression * bloc * bloc
  | TantQue of expression * bloc
  | Retour of expression * Tds.info_ast
  | Empty (* les nœuds ayant disparus: Const *)

(* informations associées à l'identificateur (dont son nom), liste des paramètres, corps *)
type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc

(* Structure d'un programme dans notre langage *)
type programme = Programme of bloc * fonction list * bloc

end

(* ******************************* *)
(* AST après la phase de placement *)
(* ******************************* *)
module AstPlacement =
struct

(* Expressions existantes dans notre langage *)
(* = expression de AstType  *)
type expression = AstType.expression

type affectable = AstTds.affectable

type variable_globale = AstType.variable_globale

type defaut = AstType.defaut


(* instructions existantes dans notre langage *)
type bloc = instruction list * int (* taille du bloc *)
 and instruction =
 | DeclarationStatic of Tds.info_ast * expression
 | Declaration of Tds.info_ast * expression
 | Affectation of AstTds.affectable * expression
 | AffichageInt of expression
 | AffichageRat of expression
 | AffichageBool of expression
 | AffichagePointeur of expression * typ
 | AffichageNull of expression
 | Conditionnelle of expression * bloc * bloc
 | TantQue of expression * bloc
 | Retour of expression * int * int (* taille du retour et taille des paramètres *)
 | Empty (* les nœuds ayant disparus: Const *)

(* informations associées à l'identificateur (dont son nom), liste de paramètres, corps, expression de retour *)
(* Plus besoin de la liste des paramètres mais on la garde pour les tests du placements mémoire *)
type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc

(* Structure d'un programme dans notre langage *)
type programme = Programme of bloc * fonction list * bloc * bloc

end
