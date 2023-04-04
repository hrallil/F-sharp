module Interpreter

    type 'a environment = (Syntax.varName * 'a) list

    let rec lookUp x env=
        match env with
        | []                -> failwith ("unbound variable name: " + x)
        | (key, value)::env -> if key = x then value else lookUp x env


    let rec pow (a:int) (b:int) =
        if b > 0 then a * pow a b-1 else 1
  
    let evalProg (funcs, e) = 
        let rec eval env = function
            | Syntax.INT i           -> i
            | Syntax.NEG e           -> -(eval env e) 
            | Syntax.VAR e           -> lookUp e env
            | Syntax.LET (x, e1, e2) -> eval((x,eval env e1)::env) e2
            | Syntax.ADD (e1, e2)    -> eval env e1 + eval env e2
            | Syntax.MUL (e1, e2)    -> eval env e1 * eval env e2
            | Syntax.SUB (e1, e2)    -> eval env e1 - eval env e2
            | Syntax.DIV (e1, e2)    -> eval env e1 / eval env e2
            | Syntax.MOD (e1, e2)    -> eval env e1 % eval env e2
            //| Syntax.EXP (e1, e2)   -> pow (eval env e1) eval(env e2) - becomes a float which is NaN in A# 
            | Syntax.EQ  (e1, e2)    -> if eval env e1 = eval env e2 then 1 else 0
            | Syntax.LT  (e1, e2)    -> if eval env e1 < eval env e2 then 1 else 0
            | Syntax.IF  (e1, e2, e3)-> if eval env e1 = 1 then eval env e2 else eval env e3
            | Syntax.CALL(f, e)      -> let v = eval env e
                                        let (x, body) = lookUp f funcs 
                                        eval [(x,v)] body
        eval [("pi",3)] e


        //--- HOW TO RUN ---//
        //dotnet build
        //#load "All.fsx";;
        //Interpreter.evalProg ([],  (Parse.fromFile("code.txt")));; 