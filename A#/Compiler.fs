module compiler
    type label = string
    type inst =         | IADD | ISUB | IMUL | IDIV | SIN
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

    type types = | TINT | TFUN



    let mutable labelCount = 0
    let newLabel _ = 
        let this = labelCount + 1
        "L"+string(this)

    let rec varpos x = function
        | [] -> failwith("unbound variable name: " + x)
        | y::env -> if x = y then 0 else 1 + varpos x env

    let addDummy env = ""::env

    let rec comp env = function
        | Syntax.INT i              -> [Asm.IPUSH i]
        | Syntax.NEG e              -> [Asm.IPUSH 0] @ comp env e @ [Asm.ISUB]
        | Syntax.VAR x              -> [Asm.ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)      -> comp env e1 @ comp (x::env) e2 @ [Asm.ISWAP] @ [Asm.IPOP]
        | Syntax.ADD (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IADD]
        | Syntax.MUL (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IMUL]
        | Syntax.SUB (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ISUB]
        | Syntax.DIV (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IDIV]
        | Syntax.MOD (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IMOD]
        | Syntax.EQ  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IEQ]
        | Syntax.LT  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ILT]
        | Syntax.IF  (e1, e2, e3)   -> comp env e1 @ [Asm.IJMPIF "_then"] @ comp env e3  @ [Asm.IJMP "_after"] @ [Asm.ILAB "_then"] @ comp env e2 @ [Asm.IJMP "_after"]
        | Syntax.CALL (f,e)         -> comp env e @ [Asm.ICALL f] @ [Asm.ISWAP] @ [Asm.IPOP]
        
        //compiler.comp ["pi";"3"] (Parse.fromString("5+1+pi"));; -comp>[IPUSH 5; IPUSH 1; IADD; ILOAD 1; IADD]

        //VM.exec (asm (compiler.comp ["pi";"3"] (Parse.fromString("5+1+pi"))));; -> 9

    let rec compProg = function
        | ([],         e1)       -> comp [] e1 @ [Asm.IHALT]
        | ((f,(x,e))::funcs, e1) -> compProg (funcs, e1) @ [Asm.ILAB f] @ comp ["";x] e @ [Asm.ISWAP] @ [Asm.IRETN]

(*
    let rec check env = function
        | INT i     -> TINT
        | VAR x     -> lookUp x env
        | ABS (x,t,e) -> let t' = check ((x,t)::env) e
                                        TFUN (t,t')
        | APP (e1,e2) -> match check env e1 with
                                    |TFUN (t2',t) -> let t2 = check env e2 in if t2=t2' then t else failwith "Type error" 
*)

    let run prog = VM.exec ( asm ( compProg (Parse.fromFile(prog))))

    // HOW TO RUN // 
    // dotnet build
    // dotnet fsi
    // #r Asm.dll;;
    // open Asm;;
    // #load "All.fsx";;
    // compiler.run "code.txt";;