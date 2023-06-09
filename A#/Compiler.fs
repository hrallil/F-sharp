module Compiler
open System
open Asm
open Check
    type 'a environment = (Syntax.varName * 'a) list

    // function that returns an unused label for the compiler
    type label = string
    let mutable labelCount = 0
    let newLabel _ = 
        labelCount <- labelCount + 1
        "L"+string(labelCount)

    // Function that returns the stack position of a variable
    let rec varpos x = function
        | [] -> failwith (sprintf "\x1b[31mUnbound variable name: %s  \x1b[31m" x)
        | y::env -> if x = y then 0 else 1 + varpos x env

    //Compiler for one expression
    let rec comp env = function
        // Simple expressions
        | Syntax.INT i              ->  [Asm.IPUSH i]
        | Syntax.TRUE               ->  [Asm.IPUSH 1]
        | Syntax.FALSE              ->  [Asm.IPUSH 0]
        | Syntax.NEG e              ->  [Asm.IPUSH 0] @ comp (""::env) e @ [Asm.ISUB]
        | Syntax.VAR x              ->  [Asm.ILOAD (varpos x env)]
        | Syntax.LET (x,e1,e2)      ->  comp env e1 @ comp (x::env) e2 @ [Asm.ISWAP] @ [Asm.IPOP]
        | Syntax.ADD (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IADD]
        | Syntax.MUL (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IMUL]
        | Syntax.SUB (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.ISUB]
        | Syntax.DIV (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IDIV]
        | Syntax.MOD (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IMOD]

        // Boolean expressions 
        | Syntax.EQ  (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IEQ]
        | Syntax.NEQ (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.IEQ ; Asm.IPUSH 0 ; Asm.IEQ] 
        | Syntax.LT  (e1, e2)       ->  comp env e1 @ comp (""::env) e2 @ [Asm.ILT]
        | Syntax.GT  (e1, e2)       ->  comp env e2 @ comp (""::env) e1 @ [Asm.ILT] 
        | Syntax.LTEQ(e1, e2)       ->  comp env (Syntax.OR(Syntax.LT(e1,e2), Syntax.EQ(e1,e2))) // maybe should be not(b<a) -> not(e) = 1-e || e==0 
        | Syntax.GTEQ(e1, e2)       ->  comp env (Syntax.OR(Syntax.LT(e2,e1), Syntax.EQ(e1,e2))) // maybe should be not(a<b) -> not(e) = 1-e || e==0
        | Syntax.AND (e1, e2)       ->  let Ltrue = newLabel () 
                                        let Lafter = newLabel () 
                                        comp env e1 @ [Asm.IJMPIF Ltrue; Asm.IPUSH 0; Asm.IJMP Lafter ; Asm.ILAB Ltrue] @ comp (""::env) e2 @ [Asm.ILAB Lafter] 
        | Syntax.OR  (e1, e2)       ->  let Ltrue = newLabel ()
                                        let Lafter = newLabel ()
                                        comp env e1 @ [Asm.IJMPIF Ltrue] @ comp env e2 @[Asm.IJMP Lafter ; Asm.ILAB Ltrue ; Asm.IPUSH 1; Asm.ILAB Lafter]

        // Bigger expressions
        | Syntax.IF  (e1, e2, e3)   ->  let Lthen = newLabel ()
                                        let Lafter = newLabel ()
                                        comp env e1 @ [Asm.IJMPIF Lthen] @ comp env e3  @ [Asm.IJMP Lafter] @ [Asm.ILAB Lthen] @ comp env e2 @ [Asm.ILAB Lafter]

        | Syntax.CALL (f,es)        -> let rec compExps env es = 
                                            match es with
                                                | [] -> []
                                                | e::es -> comp env e @ compExps (""::env) es  
                                    let rec countExps es = 
                                        match es with
                                            | [] -> 0
                                            | e::es -> 1 + countExps es
                                    let rec addSWPO count = 
                                        match count with
                                            | 0 -> []
                                            | _ -> [Asm.ISWAP] @ [Asm.IPOP] @ addSWPO (count-1)
                                    compExps env es @ [Asm.ICALL f] @ addSWPO (countExps es)
        // A# I/O
        | Syntax.READ               ->  [Asm.IREAD]
        | Syntax.WRITE (e)          ->  comp env e @ [Asm.ILOAD 0] @ [Asm.IWRITE]

        


    // Compiler of a full program (AST)
    let rec compProg prog  =
        match prog with    
            | ([],         prog_e)                  -> comp [] prog_e @ [Asm.IHALT]
            | ((f,([],func_e))::funcs, prog_e)      -> compProg (funcs, prog_e) @ [Asm.ILAB f] @ comp [""] func_e @ [Asm.ISWAP] @ [Asm.IRETN]
            | ((f,(x::xs, func_e))::funcs, prog_e)  -> compProg (funcs, prog_e) @ [Asm.ILAB f] @ comp (""::List.rev(xs) @ [x]) func_e @ [Asm.ISWAP] @ [Asm.IRETN]
            


    // Running Mortens test file (tests.txt), Containting one test pr. line
    let testLines file =
        for p in System.IO.File.ReadLines file do 
            printf "Testing program\n    %s\n" p
            let ast = Parse.fromString p
            let instrs = compProg ast
            let code = Asm.asm instrs
            VM.exec code
            printf "Done\n\n"


    // helper function which will lex and parse a file into an AST, type check and compile it and send the instructions to the VM to return a result.
    let run file = 
        printf "\x1b[32mRunning code: \n\x1b[0m"
        let ast = Parse.fromFile file
        let typeOf = Check.typeOfProg ast
        let instList = compProg ast
        let binary = asm instList
        VM.exec binary

    // helper function which will return instructions of file
    let instOf file = compProg(Parse.fromFile file)

    // helper function which will return AST of file
    let AstOf file = Parse.fromFile file

    // helper function which will return binary of file
    let binaryOf file = asm (compProg(Parse.fromFile file))
    
    // HOW TO RUN COMPILER // 
    // dotnet build
    // dotnet fsi
    // #load "All.fsx";;
    // compiler.run "code.txt";;