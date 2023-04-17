module Syntax
    type varName    = string
    type funcName   = string


    // Expressions
    type exp =  | INT of int                        // i -> [0-9]
                | VAR of varName                    // x
                | NEG of exp                        // -(exp)
                | LET of varName * exp * exp        // let varname = exp in exp -> let x = 5 in 5+x 
                | MUL of exp * exp                  // exp * exp
                | ADD of exp * exp                  // exp + exp
                | SUB of exp * exp                  // exp - exp
                | EXP of exp * exp                  // exp**exp -> isnt used at this moment 
                | DIV of exp * exp                  // exp / exp
                | MOD of exp * exp                  // exp % exp
                | EQ  of exp * exp                  // exp==exp -> true/false (1/0)
                | LT  of exp * exp                  // exp < exp -> true/false (1/0)
                | IF  of exp * exp * exp            // if exp then exp else exp -> if 1 then 20 else 10
                | CALL of funcName * exp            // funcName exp -> f(e)
                | ABS of varName * exp              //
                | APP of exp * exp                  //

    (*
    type value = | VI of int 
                 | VC of varName * exp * value env*)

    type funcDef       = funcName * (varName * exp) // func funcame varName = exp -> func f(x) = e; 

