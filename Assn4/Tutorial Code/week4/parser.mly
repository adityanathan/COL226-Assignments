/* File parser.mly */
%{
    open Expression
%}

%token LP RP ADD MULT EOL EOF
%token <int> INT
%token <string> ID
%start main             /* the entry point */
%type <Expression.expr_tree> main                     /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    add_expression EOL                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { NULL }
;
add_expression:
    add_expression ADD mult_expression { PLUS($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | mult_expression                  { $1 }
;
mult_expression:
    mult_expression MULT constant      { INTO($1,$3) }
    | constant                         { $1 }
;
constant:
    ID                                 { VAR($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { NUM($1) }      /* To be interpreted as an integer with its value as tokenised   */
;

/*TODO
 * Add support in the grammar for parenthesis
 *  - Adding the parenthesis should be able to change the parse tree to effectively modify precedence.
 *  E.g. 1+2*3  ==>        PLUS
 *                        /    \
 *                      NUM1   INTO
 *                            /    \
 *                         NUM 2  NUM 3
 *
 *  vs (1+2)*3  ==>        INTO
 *                        /    \
 *                     PLUS     NUM 3
 *                    /    \
 *                 NUM 1   NUM 2
 *
 * Try completing the calculator for basic arithmetic by adding division and subtraction, while respecting precedence
 * This will require changes right from the lexer.mll and parser.mly to the definition of print and evaluation functions in expression.ml
 *
 * ADVANCED
 * Try creating an expression for assigning new variables in the variable_set in the expression.ml file, so that they can be reused in a later evaluation statement.
 * E.g. myVar:=4.
 *      // Stores the integer value 4 corresponding to the string myVar in variable_set
 *
 *      myVar*3+1
 *      Answer: 13
 * */
