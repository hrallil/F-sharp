module Compiler

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
        // Simple expressions
        | Syntax.INT i              -> [Asm.IPUSH i]
        | Syntax.NEG e              -> [Asm.IPUSH 0] @ comp env e @ [Asm.ISUB]
        | Syntax.VAR x              -> [Asm.ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)      -> comp env e1 @ comp (x::env) e2 @ [Asm.ISWAP] @ [Asm.IPOP]
        | Syntax.ADD (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IADD]
        | Syntax.MUL (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IMUL]
        | Syntax.SUB (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ISUB]
        | Syntax.DIV (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IDIV]
        | Syntax.MOD (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IMOD]

        // Boolean expressions 
        | Syntax.EQ  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IEQ]
        //| Syntax.NEQ (e1, e2)       -> 
        | Syntax.LT  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ILT]
        //| Syntax.GT  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.IGT]
        //| Syntax.LTEQ(e1, e2)       -> 
        //| Syntax.GTEQ(e1, e2)       -> 
        //| Syntax.AND (e1, e2)       -> 
        //| Syntax.OR  (e1, e2)       -> 

        // Bigger expressions
        | Syntax.IF  (e1, e2, e3)   -> comp env e1 @ [Asm.IJMPIF "_then"] @ comp env e3  @ [Asm.IJMP "_after"] @ [Asm.ILAB "_then"] @ comp env e2 @ [Asm.IJMP "_after"]
        | Syntax.CALL (f,[e])         -> comp env e @ [Asm.ICALL f] @ [Asm.ISWAP] @ [Asm.IPOP]
        


    // compiler
    let rec compProg = function
        | ([],         e1)       -> comp [] e1 @ [Asm.IHALT]
        | ((f,([x],e))::funcs, e1) -> compProg (funcs, e1) @ [Asm.ILAB f] @ comp ["";x] e @ [Asm.ISWAP] @ [Asm.IRETN]


    let run prog = VM.exec ( asm ( compProg (Parse.fromFile(prog))))

    // HOW TO RUN // 
    // dotnet build
    // dotnet fsi
    // #r "asm.dll";;
    // open Asm;;
    // #load "All.fsx";;
    // compiler.run "code.txt";;