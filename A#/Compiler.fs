module Compiler

    
    type 'a environment = (Syntax.varName * 'a) list

    type types = | TINT | TFUN



    type label = string
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
        //| Syntax.NEQ (e1, e2)       -> if comp env e1 = comp env e2 then [Asm.IPUSH 0] else [Asm.IPUSH 1] // feels wrong since i dont think the compiler should do any evaluations
        | Syntax.LT  (e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ILT]
        | Syntax.GT  (e1, e2)       -> comp env e2 @ comp env e1 @ [Asm.ILT]
        | Syntax.LTEQ(e1, e2)       -> comp env e1 @ comp env e2 @ [Asm.ILT] 
        | Syntax.GTEQ(e1, e2)       -> comp env e2 @ comp env e1 @ [Asm.ILT]
        //| Syntax.AND (e1, e2)       -> 
        //| Syntax.OR  (e1, e2)       -> 

        // Bigger expressions
        | Syntax.IF  (e1, e2, e3)   -> comp env e1 @ [Asm.IJMPIF "_then"] @ comp env e3  @ [Asm.IJMP "_after"] @ [Asm.ILAB "_then"] @ comp env e2 @ [Asm.IJMP "_after"]
        | Syntax.CALL (f,[e])         -> comp env e @ [Asm.ICALL f] @ [Asm.ISWAP] @ [Asm.IPOP]
        


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