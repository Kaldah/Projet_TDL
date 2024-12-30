/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token RETURN
%token VIRG
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token EOF
%token NEW
%token NULL
%token REF
%token STATIC

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <variable_globale> globale
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction> i
%type <typ> typ
%type <defaut> d
%type <typ*string*(defaut option)> param
%type <expression> e
%type <affectable> a

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

globale : STATIC t=typ n=ID EQUAL e1=e PV {DeclarationGlobale (t,n,e1)}

main : lfi=prog EOF     {lfi}

prog : lg=globale* lf=fonc* ID li=bloc  {Programme (lg,lf,li)}

fonc : t=typ n=ID PO lp=separated_list(VIRG,param) PF li=bloc {Fonction(t,n,lp,li)}

param : t=typ n=ID  dp=option(d)  {(t,n,dp)}

bloc : AO li=i* AF      {li}

d : EQUAL e1=e         {Defaut e1}

a :
| MULT a=a                          {Deref a}
| n=ID                              {Ident n}


i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| n=a EQUAL e1=e PV                 {Affectation (n,e1)}
| CONST n=ID EQUAL e1=ENTIER PV     {Constante (n,e1)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| STATIC t=typ n=ID EQUAL e1=e PV   {DeclarationStatic (n,t,e1)}


typ :
| t=typ MULT {Pointeur t}
| BOOL    {Bool}
| INT     {Int}
| RAT     {Rat}

e :
| n=a                     {Affectable n}
| n=ID PO lp=separated_list(VIRG,e) PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Binaire(Fraction,e1,e2)}
| TRUE                    {Booleen true}
| FALSE                   {Booleen false}
| e=ENTIER                {Entier e}
| NUM e1=e                {Unaire(Numerateur,e1)}
| DENOM e1=e              {Unaire(Denominateur,e1)}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO NEW t=typ  PF        {New t}
| REF n=ID                {Adresse n}
| NULL                    {Null}
| PO exp=e PF             {exp}
