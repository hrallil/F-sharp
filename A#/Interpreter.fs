module Interpreter



    type 'a environment = (Syntax.varName * 'a) list

    let rec lookUp x env=
        match env with
        | []                -> failwith ("unbound variable name: " + x)
        | (key, value)::env -> if key = x then value else lookUp x env


    let rec pow (a:int) (b:int) =
        if b > 0 then a * pow a b-1 else 1

    type pType =    | PBool     of bool
                    | PInt      of int

    let evalProg (funcs, e) = 
        let rec eval env = function
            | Syntax.INT i           -> i
            | Syntax.NEG e           -> -(eval env e) 
            | Syntax.VAR e           -> lookUp e env
           (* | Syntax.ABS (x, e)      -> Syntax.VC (x, e, env) 
            | Syntax.APP (e1,e2)     -> let v2 = eval env e2
                                        match eval env e1 with
                                            | VC (x,e,env1) -> eval ((x,v2)::env1) e*)
            // Simple expressions
            | Syntax.LET (x, e1, e2) -> eval((x,eval env e1)::env) e2
            | Syntax.ADD (e1, e2)    -> eval env e1 + eval env e2
            | Syntax.MUL (e1, e2)    -> eval env e1 * eval env e2
            | Syntax.SUB (e1, e2)    -> eval env e1 - eval env e2
            | Syntax.DIV (e1, e2)    -> eval env e1 / eval env e2
            | Syntax.MOD (e1, e2)    -> eval env e1 % eval env e2

            // Bool expressions
            | Syntax.EQ  (e1, e2)    -> if eval env e1 = eval env e2 then 1 else 0
            | Syntax.NEQ (e1, e2)    -> if eval env e1 = eval env e2 then 0 else 1
            | Syntax.LT  (e1, e2)    -> if eval env e1 < eval env e2 then 1 else 0
            | Syntax.GT  (e1, e2)    -> if eval env e1 > eval env e2 then 1 else 0
            | Syntax.LTEQ(e1, e2)    -> if eval env e1 <= eval env e2 then 1 else 0
            | Syntax.GTEQ(e1, e2)    -> if eval env e1 >= eval env e2 then 1 else 0
           
            // Bigger expressions
            | Syntax.IF  (e1, e2, e3)-> if eval env e1 = 1 then eval env e2 else eval env e3
            | Syntax.CALL(f, e)      -> let v = eval env e
                                        let (x, body) = lookUp f funcs 
                                        eval [(x,v)] body
            (*| Syntax.COMMA(e1,e2)    -> eval env e1
                                        eval env e2*)
        eval [("pi",3)] e


    let run prog = evalProg (Parse.fromFile(prog))

        //--- HOW TO RUN ---//
        //dotnet build
        //dotnet fsi
        //#load "All.fsx";;
        //Interpreter.run "code.txt";; 


    (*  | Syntax.AND (e1, e2)    -> if eval env e1 == eval env e2 && eval env e2 == 1 then 1 else 0
        | Syntax.AND (e1, e2)    -> if eval env e1  && eval env e2 == 1 then 1 else 0
        | Syntax.OR  (e1, e2)    -> if eval env e1 || eval env e2 then 1 else 0
*)