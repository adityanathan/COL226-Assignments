%{
    open A1
%}

/* Tokens are defined below.  */
%token COMMA TILDA LP RP IF THEN ELSE FI DELIMITER EOF EQ GTA LTA GEQ LEQ ABS EXP DIV MOD MUL PLUS MINUS AND OR NOT
%token <int> INT
%token <string> ID
%token <bool> BOOL

%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
    ifte_def_expr DELIMITER                     { $1 }
    | EOF                                       { NULL }
;

ifte_def_expr:
    IF sub_expr THEN sub_expr ELSE sub_expr FI  {IfThenElse($2,$5,$7)}
    | sub_expr                                  {$1}
    /* | DEF   */

sub_expr:
    sub_expr MINUS add_expr                     {Minus ($1, $3)}
    | add_expr                                  {$1}
;
add_expr:
    add_expr PLUS mult_expr                     {Plus ($1, $3)}
    | mult_expr                                 {$1}
;
mult_expr:
    mult_expr MUL div_expr                      {Mult ($1, $3)}
    | div_expr                                  {$1}
;
mod_expr:
    mod_expr MOD div_expr                       {Rem ($1, $3)}
    | div_expr                                  {$1}
;
div_expr:
    div_expr DIV exponent_expr                  {Div ($1, $3)}
    | exponent_expr                             {$1}
;
exponent_expr:
    abs_expr EXP exponent_expr                  {Exp ($1, $3)}
    | abs_expr                                  {$1}
;
abs_expr:
    ABS comparison_expr                         {Abs ($2)}
    | comparison_expr                           {$1}

comparison_expr:
    comparison_expr GEQ and_expr                {GreaterTE ($1, $3)}
    | comparison_expr LEQ and_expr              {LessTE ($1, $3)}
    | comparison_expr GTA and_expr              {GreaterT ($1, $3)}
    | comparison_expr LTA and_expr              {LessT ($1, $3)}
    | comparison_expr EQ and_expr               {Equals ($1, $3)}
    | and_expr                                  {$1}
;

and_expr:
    and_expr DIV or_expr                        {Conjunction ($1, $3)}
    | or_expr                                   {$1}
;

or_expr:
    or_expr DIV not_expr                        {Disjunction ($1, $3)}
    | not_expr                                  {$1}
;

not_expr:
    NOT paren_expr                              {Not ($2)}
    | paren_expr                                {$1}

paren_expr:
    LP paren_expr RP                            {InParen($2)}
    | constant                                  {$1}

constant:
    ID                                          {Var ($1)}
    | INT                                       {N ($1)}
    | BOOL                                      {B ($1)}
