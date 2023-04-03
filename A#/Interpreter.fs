module Interpreter

    type 'a environment = (Syntax.varName * 'a) list

    let rec lookUp x env=
        match env with
        | []                -> failwith ("unbound variable name: " + x)
        | (key, value)::env -> if key = x then value else lookUp x env


    let rec pow (a:int) (b:int) =
        if b > 0 then a * pow a b-1 else 1
  

    let rec eval env = function
        | Syntax.INT i          -> i
        | Syntax.NEG e          -> -(eval env e) 
        | Syntax.VAR e          -> lookUp e env
        | Syntax.LET (x, e1, e2)-> eval((x,eval env e1)::env) e2
        | Syntax.ADD (e1, e2)   -> eval env e1 + eval env e2
        | Syntax.MUL (e1, e2)   -> eval env e1 * eval env e2
        | Syntax.SUB (e1, e2)   -> eval env e1 - eval env e2
        | Syntax.DIV (e1, e2)   -> eval env e1 / eval env e2
        | Syntax.MOD (e1, e2)   -> eval env e1 % eval env e2
        //| Syntax.EXP (e1, e2)   -> pow (eval env e1) eval(env e2)
        | Syntax.EQ  (e1, e2)   -> if eval env e1 = eval env e2 then 1 else 0
        | Syntax.LT  (e1, e2)   -> if eval env e1 < eval env e2 then 1 else 0

        //Interpreter.eval [("pi",3)]  (Parse.fromString("5 + pi"));; -interpreter> int : 8