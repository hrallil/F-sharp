module compiler
    type label = string
    type Instruction =  | IADD | ISUB | IMUL | IDIV | SIN
                        | COS | LOG | EXP | IMOD | IEQ | ILT 
                        | ISWAP | IPOP | IHALT
                        | IPUSH     of int
                        | ILOAD     of int
                        | IJMP      of label 
                        | IJMPIF    of label 
                        | ILAB      of label
                        | ICALL     of label
                        | IRETN     
    
    type 'a environment = (Syntax.varName * 'a) list

    let mutable labelCount = 0
    let newLabel _ = 
        let this = labelCount + 1
        "L"+string(this)

    let rec varpos x = function
        | [] -> failwith("unbound variable name: " + x)
        | y::env -> if x = y then 0 else 1 + varpos x env

    let addDummy env = ""::env

    let rec comp env = function
        | Syntax.INT i              -> [IPUSH i]
        | Syntax.NEG e              -> [IPUSH 0] @ comp env e @ [ISUB]
        | Syntax.VAR x              -> [ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)      -> comp env e1 @ comp (x::env) e2 @ [ISWAP] @ [IPOP]
        | Syntax.ADD (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [IADD]
        | Syntax.MUL (e1, e2)       -> comp env e1 @ comp env e2 @ [IMUL]
        | Syntax.SUB (e1, e2)       -> comp env e1 @ comp env e2 @ [ISUB]
        | Syntax.DIV (e1, e2)       -> comp env e1 @ comp env e2 @ [IDIV]
        | Syntax.MOD (e1, e2)       -> comp env e1 @ comp env e2 @ [IMOD]
        | Syntax.EQ  (e1, e2)       -> comp env e1 @ comp env e2 @ [IEQ]
        | Syntax.LT  (e1, e2)       -> comp env e1 @ comp env e2 @ [ILT]
        | Syntax.IF  (e1, e2, e3)   -> comp env e1 @ [IJMPIF "_then"] @ comp env e3  @ [IJMP "_after"] @ [ILAB "_then"] @ comp env e2 @ [IJMP "_after"]
        | Syntax.CALL (f,e)         -> comp env e1 @ [ICALL f] @ [ISWAP] @ [IPOP]
        
        //compiler.comp ["pi";"3"] (Parse.fromString("5+1+pi"));; -comp>[IPUSH 5; IPUSH 1; IADD; ILOAD 1; IADD]

        //VM.exec (asm (compiler.comp ["pi";"3"] (Parse.fromString("5+1+pi"))));; -> 9

    let rec compProg = function
        | ([],         e1)       -> comp [] e1 @ [IHALT]
        | ((f,(x,e))::funcs, e1) -> compProg (funcs, e1) @ [ILAB f] @ comp ["";x] e @ [ISWAP] @ [IRETN]