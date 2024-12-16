open Type

(* Interface des arbres abstraits *)
module type AstSyntax =
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

(* Opérateurs unaires de Rat *)
type unaire = Numerateur | Denominateur

(* Opérateurs binaires de Rat *)
type binaire = Fraction | Plus | Mult | Equ | Inf

type affectable = 
  (* Accès à un identifiant représenté par son nom *)
 | Ident of string
  (* Déréférencement d'un identifiant *)
 | Deref of affectable

(* Expressions de Rat *)
type expression =
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AppelFonction of string * expression list
  (* Booléen *)
  | Booleen of bool
  (* Entier *)
  | Entier of int
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | Unaire of unaire * expression
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | Binaire of binaire * expression * expression
  (* Affectable - Pointeur et Ident *)
  | Affectable of affectable
  (* Nouveau pointeur vers une variable de type typ *)
  | New of typ
  (* Adresse mémoire *)
  | Adresse of string
  (* Type Null *)
  | Null

type variable_globale = Var of string * typ * expression

type defaut = Defaut of expression


(* Instructions de Rat *)
type bloc = instruction list
and instruction =
  (* Déclaration et affectation d'une variable static *)
  | DeclarationStatic of string * typ * expression
  (* Déclaration de variable représentée par son type, son nom et l'expression d'initialisation *)
  | Declaration of typ * string * expression
  (* Affectation d'une variable représentée par son affectable et la nouvelle valeur affectée *)
  | Affectation of affectable * expression
  (* Déclaration d'une constante représentée par son nom et sa valeur (entier) *)
  | Constante of string * int
  (* Affichage d'une expression *)
  | Affichage of expression
  (* Conditionnelle représentée par la condition, le bloc then et le bloc else *)
  | Conditionnelle of expression * bloc * bloc
  (*Boucle TantQue représentée par la conditin d'arrêt de la boucle et le bloc d'instructions *)
  | TantQue of expression * bloc
  (* return d'une fonction *)
  | Retour of expression

(* Structure des fonctions de Rat *)
(* type de retour - nom - liste des paramètres (association type et nom) - corps de la fonction *)
type fonction = Fonction of typ * string * (typ * string * defaut option) list * bloc

(* Structure d'un programme Rat *)
(* liste de fonction - programme principal *)
type programme = Programme of variable_globale list * fonction list * bloc
