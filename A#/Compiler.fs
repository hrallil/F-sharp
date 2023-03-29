module compiler
    type Instruction =  | IADD | ISUB | IMUL | IDIV | SIN
                        | COS | LOG | EXP | IPUSH of int
                        | IMOD | IEQ | ILT | ILOAD of int
                        | IPOP | ISWAP
    
    type 'a environment = (Syntax.varName * 'a) list

    let rec varpos x = function
        | [] -> failwith("unbound variable name: " + x)
        | y::env -> if x = y then 0 else 1 + varpos x env

    let addDummy env = ""::env

    let rec comp env = function
        | Syntax.INT i          -> [IPUSH i]
        | Syntax.NEG e          -> [IPUSH 0] @ comp env e @ [ISUB]
        | Syntax.VAR x          -> [ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)  -> comp env e1 @ comp (x::env) e2 @ [ISWAP] @ [IPOP]
        | Syntax.ADD (e1, e2)   -> comp env e1 @ comp (""::env) e2 @ [IADD]
        | Syntax.MUL (e1, e2)   -> comp env e1 @ comp env e2 @ [IMUL]
        | Syntax.SUB (e1, e2)   -> comp env e1 @ comp env e2 @ [ISUB]
        | Syntax.DIV (e1, e2)   -> comp env e1 @ comp env e2 @ [IDIV]
        | Syntax.MOD (e1, e2)   -> comp env e1 @ comp env e2 @ [IMOD]
        | Syntax.EQ  (e1, e2)   -> comp env e1 @ comp env e2 @ [IEQ]
        | Syntax.LT  (e1, e2)   -> comp env e1 @ comp env e2 @ [ILT]

        //compiler.comp ["pi";"3"] (Parse.fromString("5+1+pi"));; -comp>[IPUSH 5; IPUSH 1; IADD; ILOAD 1; IADD]