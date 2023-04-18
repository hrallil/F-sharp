module Interpreter
open System



    type 'a environment = (Syntax.varName * 'a) list

    let rec lookUp x env=
        match env with
        | []                -> failwith ("unbound variable name: " + x)
        | (key, value)::env -> if key = x then value else lookUp x env


    let rec pow (a:int) (b:int) =
        if b > 0 then a * pow a b-1 else 1

    // binds i'th element of xs with the i'th element of vs to eachother
    let rec bindAll xs vs =
        match (xs,vs) with
            | ([], []) -> []
            | (x::xs, v::vs) -> [(x,v)]::bindAll xs vs

    let evalProg (funcs, e) = 
        let rec eval env = function
            // Simple expressions
            | Syntax.INT i           -> i
            | Syntax.NEG e           -> -(eval env e) 
            | Syntax.VAR e           -> lookUp e env
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
            | Syntax.AND (e1, e2)    -> if eval env e1 <> 0 then eval env e2 else 0
            | Syntax.OR  (e1, e2)    -> if eval env e1 <> 0 then 1 else eval env e2

            // Bigger expressions
            | Syntax.IF  (e1, e2, e3)-> if eval env e1 <> 0 then eval env e2 else eval env e3
            | Syntax.CALL(f, es)     -> let rec evalExps es = 
                                            match es with
                                                | []    -> []
                                                | e::es -> [eval env e]:: evalExps es
                                        let vs = evalExps es

                                        let (variableNames, body) = lookUp f funcs 
                                        eval (bindAll variableNames vs) body
            
            // A# I/O
            | Syntax.WRITE (e)       -> let v = eval env e
                                        printfn "%d" v
                                        v
            | Syntax.READ            -> let v = Console.ReadLine()
                                        Int32.Parse(v)
                                        


        eval [("pi",3)] e

    let rec evalExps es = 
        match es with
            | []    -> []
            | e::es -> [eval env e]:: evalExps es

 
    let run prog = evalProg (Parse.fromFile(prog))

        //--- HOW TO RUN ---//
        //dotnet build
        //dotnet fsi
        //#load "All.fsx";;
        //Interpreter.run "code.txt";; 


    (*  | Syntax.AND (e1, e2)    -> if eval env e1 = eval env e2 && eval env e2 == 1 then 1 else 0
        | Syntax.AND (e1, e2)    -> if eval env e1  && eval env e2 == 1 then 1 else 0
        | Syntax.OR  (e1, e2)    -> if eval env e1 || eval env e2 then 1 else 0
*)