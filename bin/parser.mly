/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR
%token REF ASSOC BANG
%token BEGIN END SEP
%token OR AND NOT
%token IF THEN ELSE
%token PLUS TIMES MINUS DIV
%token GEQ LEQ DIFF EQUAL LESS GREATER
%token COMMA
%token LPAREN RPAREN
%token LET IN IN_SEP
%token FUN ARROW
%token FUNCTION
%token MATCH WITH PIPE
%token EOF             /* fin de fichier */

%left IN 
%left VAR_EQUAL
%left SEP
%left ASSOC
%left WITH
%left PIPE
%left ARROW
%nonassoc ELSE
%left COMMA /* a,b,c c'est (a,b),c */
%left OR /* associativité gauche: a||b||c, c'est (a||b)||c */
%left AND /* associativité gauche: a&&b&&c, c'est (a&&b)&&c */
%left GEQ LEQ DIFF EQUAL LESS GREATER /* Prioritaires sur le ELSE, et c'est tout */
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV /* associativité gauche: a*b*c, c'est (a*b)*c */
%nonassoc NOT
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%left REF

%{ open Ast %}

%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Ast.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression_s EOF              { $1 }  /* on veut reconnaître une expression */  
  ;

expression:			    /* règles de grammaire pour les expressions */
  | INT                                                                 { Const $1 }
  | BOOL                                                                { Bool $1 }
  | VAR                                                                 { Var $1 }
  | LPAREN expression RPAREN                                            { $2 } /* on récupère le deuxième élément */
  
  | expression PLUS expression                                          { Arth(Add,$1,$3) }
  | expression TIMES expression                                         { Arth(Mul,$1,$3) }
  | expression MINUS expression                                         { Arth(Min,$1,$3) }
  | expression DIV expression                                           { Arth(Div,$1,$3) }
  | MINUS expression %prec UMINUS                                       { Arth(Min,Const 0, $2) }
  
  | expression OR expression                                            { Boop(Or,$1, $3) }
  | expression AND expression                                           { Boop(And,$1, $3) }
  | expression GEQ expression                                           { Comp(Geq,$1, $3) }
  | expression LEQ expression                                           { Comp(Leq,$1, $3) }
  | expression DIFF expression                                          { Comp(Diff,$1, $3) }
  | expression EQUAL expression                                         { Comp(Equal,$1, $3) }
  | expression LESS expression                                          { Comp(Less,$1, $3) }
  | expression GREATER expression                                       { Comp(Greater,$1, $3) }
  | NOT expression                                                      { Not($2) }

  | IF expression THEN expression ELSE expression                       { IfThenElse($2, $4, $6) }

  | LET expr_match EQUAL expression IN expression %prec VAR_EQUAL       { LetIn($2, $4, $6) }
  | LET VAR var_list IN expression                                      { LetIn(EVar($2), $3, $5) }
  | FUN var_list                                                        { $2 }
  | FUNCTION expr_match ARROW expression                                { Fun($2, $4) }
  | expr_r_list                                                         { $1 }
  
  | REF expression                                                      { Ref($2) }
  | VAR ASSOC expression                                                { Assoc($1, $3) }
  | BANG VAR                                                            { Bang(Var($2)) }
  | BANG LPAREN expression RPAREN                                       { Bang($3) }
  | BEGIN expression END                                                { $2 }
  | expression SEP expression                                           { LetIn(EVar("_"), $1, $3) }
  | expression COMMA expression                                         { Couple($1, $3) }

  | MATCH expression WITH matching_expr                                 { Match($2, $4) }
;

  expr_r_list:
  | expression_r expression_r                                           { Appl($1, $2) }
  | expr_r_list expression_r                                            { Appl($1, $2) }
  ;

var_list:
  | VAR EQUAL expression %prec VAR_EQUAL                { Fun(EVar($1), $3) } 
  | VAR ARROW expression                                { Fun(EVar($1), $3) }
  | VAR var_list                                        { Fun(EVar($1), $2) }
;

expr_match:
  | VAR                                                 { EVar($1) }
  | expr_match COMMA expr_match                         { ECouple($1, $3) }
;

expression_r:
  | INT                                                 { Const $1 }
  | BOOL                                                { Bool $1 }
  | VAR                                                 { Var $1 }
  | BANG VAR                                            { Bang(Var($2)) }
  | BANG LPAREN expression RPAREN                       { Bang($3) }
  | LPAREN expression RPAREN                            { $2 } /* on récupère le deuxième élément */
/* on ne se permet pas d'avoir des f ref 4 ou des f prInt 4, on ne voit ceux ci que comme des lexèmes, et non des fonctions */
;

matching_expr:
  | expr_match ARROW expression                         { [$1,$3] }
  | matching_expr PIPE matching_expr                    { $1@$3 }
;

expression_s:
  | expression                                          { $1 }
  | LET expr_match EQUAL expression IN_SEP expression_s { LetIn($2, $4, $6) }
;
 