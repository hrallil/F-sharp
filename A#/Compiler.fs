module Compiler

    
    type 'a environment = (Syntax.varName * 'a) list

    type types = | TINT | TFUN



    type label = string
    let mutable labelCount = 0

    let newLabel _ = 
        labelCount <- labelCount + 1
        "L"+string(labelCount)

    let rec varpos x = function
        | [] -> failwith("unbound variable name: " + x)
        | y::env -> if x = y then 0 else 1 + varpos x env

    let addDummy env = ""::env

    let rec comp env = function
        // Simple expressions
        | Syntax.INT i              -> [Asm.IPUSH i]
        | Syntax.NEG e              -> [Asm.IPUSH 0] @ comp (""::env) e @ [Asm.ISUB]
        | Syntax.VAR x              -> [Asm.ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)      -> comp env e1 @ comp (x::env) e2 @ [Asm.ISWAP] @ [Asm.IPOP]
        | Syntax.ADD (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IADD]
        | Syntax.MUL (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IMUL]
        | Syntax.SUB (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.ISUB]
        | Syntax.DIV (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IDIV]
        | Syntax.MOD (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IMOD]

        // Boolean expressions 
        | Syntax.EQ  (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IEQ]
        | Syntax.NEQ (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IEQ ; Asm.IPUSH 0 ; Asm.IEQ] // comp env e1 @ comp env e2 @ [Asm.ISUB] gives 0 for not equal but random numbers for equal
        | Syntax.LT  (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.ILT]
        
        | Syntax.GT  (e1, e2)       -> comp env (Syntax.OR(Syntax.LT(e2,e1), Syntax.EQ(e1,e2))) // gives 1 when e1 = e2 should give 0
        | Syntax.LTEQ(e1, e2)       -> comp env (Syntax.OR(Syntax.LT(e1,e2), Syntax.EQ(e1,e2)))
        
        | Syntax.GTEQ(e1, e2)       -> comp env e2 @ comp (""::env) e1 @ [Asm.ILT]
        | Syntax.AND (e1, e2)       -> let Ltrue = newLabel ()
                                       let Lafter = newLabel () // Could this not just be: comp env e1 @ comp env e2 @ [Asm.IMUL]? since all non-zero numbers are true
                                       comp env e1 @ comp (""::env) e2 @ [Asm.IMUL ; Asm.IJMPIF Ltrue ; Asm.IPUSH 0 ; Asm.IJMP Lafter ; Asm.ILAB Ltrue ; Asm.IPUSH 1; Asm.ILAB Lafter]
        | Syntax.OR  (e1, e2)       -> comp env e1 @ comp (""::env) e2 @ [Asm.IADD] // if e1 = -5 and e2 = 5 -> false, should be true 

        // Bigger expressions
        | Syntax.IF  (e1, e2, e3)   -> let Lthen = newLabel ()
                                       let Lafter = newLabel ()
                                       comp env e1 @ [Asm.IJMPIF Lthen] @ comp env e3  @ [Asm.IJMP Lafter] @ [Asm.ILAB Lthen] @ comp env e2 @ [Asm.IJMP Lafter]

        | Syntax.CALL (f,[e])       -> comp env e @ [Asm.ICALL f] @ [Asm.ISWAP] @ [Asm.IPOP]
        


    // compiler
    let rec compProg prog  =
        match prog with    
            | ([],         prog_e)       -> comp [] prog_e @ [Asm.IHALT]
            | ((f,([x],func_e))::funcs, prog_e) -> compProg (funcs, prog_e) @ [Asm.ILAB f] @ comp ["";x] func_e @ [Asm.ISWAP] @ [Asm.IRETN]
            //| ((f,([x1]::[x2],func_e))::funcs, prog_e) -> compProg (funcs, prog_e) @ [Asm.ILAB f] @ comp ["";x] func_e @ [Asm.ISWAP] @ [Asm.IRETN]


    let run prog = VM.exec ( asm ( compProg (Parse.fromFile(prog))))

    // HOW TO RUN // 
    // dotnet build
    // dotnet fsi
    // #r "asm.dll";;
    // open Asm;;
    // #load "All.fsx";;
    // compiler.run "code.txt";;