module Interpreter

    type 'a env = (Syntax.varName * 'a) list

    let rec lookUp x env = function
        | []                -> failwith ("some error in lookUp")
        | (key, value)::env -> if key = x then value else lookUp x env



    let rec eval env = function
        | Syntax.INT i          -> i
        | Syntax.NEG e          -> -(eval e) 
        | Syntax.VAR e        -> loopUp e env
        | Syntax.ADD (e1, e2)   -> eval env e1 + eval env e2
        | Syntax.MUL (e1, e2)   -> eval e1 * eval e2
        | Syntax.SUB (e1, e2)   -> eval e1 - eval e2
        | Syntax.DIV (e1, e2)   -> eval e1 / eval e2
        | Syntax.MOD (e1, e2)   -> eval e1 % eval e2
        //| Syntax.EXP (e1, e2)   -> float(eval e1) **  float(eval e2)
        | Syntax.EQ  (e1, e2)   -> if eval e1 = eval e2 then 1 else 0
        | Syntax.LT  (e1, e2)   -> if eval e1 < eval e2 then 1 else 0