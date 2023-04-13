module Syntax
    type varName    = string
    type funcName   = string
    // Expressions
    type exp =  | INT of int 
                | VAR of varName
                | NEG of exp
                | LET of varName * exp * exp
                | MUL of exp * exp
                | ADD of exp * exp 
                | SUB of exp * exp
                | EXP of exp * exp
                | DIV of exp * exp
                | MOD of exp * exp
                | EQ  of exp * exp
                | LT  of exp * exp
                | IF  of exp * exp * exp
                | CALL of funcName * exp            // f(e)

    type funcDef       = funcName * (varName * exp) // func f x = e; func funcame varName = exp

