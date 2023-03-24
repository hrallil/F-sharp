module calculator

    type Instruction =  | ADD | SUB | MUL | DIV | SIN
                        | COS | LOG | EXP | PUSH of float

    type Stack = float list


    let applyInst (st:Stack) inst=
        match inst, st with
        | ADD, a::b::_st -> a+b::_st
        | SUB, a::b::_st -> a-b::_st
        | MUL, a::b::_st -> a*b::_st
        | DIV, a::b::_st -> a/b::_st
        | COS, a::_st    -> System.Math.Cos(a)::_st
        | LOG, a::_st    -> System.Math.Log(a,10)::_st
        | SIN, a::_st    -> System.Math.Sin(a)::_st
        | EXP, a::_st -> a**2::_st
        | PUSH i, st -> i::st
        | _ -> []
    
    type instStack = Instruction list


    // VM ? takes a list of instructions and a stack of floats and returns a result -> float // suposed to be list of instructions -> float, is list of instructions*Stack -> float
    let rec intpProg (fStack:Stack, instList)=
        match instList, fStack with
        | [],[] -> 0.0
        | [],y::ys -> y
        | x::_il, fStack -> intpProg (applyInst fStack x, _il)

    // intpProg [],[PUSH 1.0; PUSH 2.0; ADD]-> 3.0
    // intpProg [],[PUSH 1.0; PUSH 2.0; ADD;PUSH 4.0; ADD]-> 7.0


    type Fexpr =    | Const of float
                    | X
                    | Add of Fexpr * Fexpr
                    | Sub of Fexpr * Fexpr
                    | Mul of Fexpr * Fexpr
                    | Div of Fexpr * Fexpr
                    | Sin of Fexpr
                    | Cos of Fexpr
                    | Log of Fexpr
                    | Exp of Fexpr

    // Fexpr * float -> Instruction list ----- i guess this is a compiler, that makes a list of instructions from an AST
    let rec trans Fexpr =
        match Fexpr with 
        | Add (e1,e2) -> trans e1 @ trans e2 @ [ADD]
        | Sub (e1,e2) -> trans e1 @ trans e2 @ [SUB]
        | Mul (e1,e2) -> trans e1 @ trans e2 @ [MUL]
        | Div (e1,e2) -> trans e1 @ trans e2 @ [DIV]
        | Const f            -> [PUSH f]
        | Sin e              -> trans e @ [SIN]
        | Cos e              -> trans e @ [COS]
        | Log e              -> trans e @ [LOG]
        | Exp e              -> trans e @ [EXP]


    let prg exp = intpProg ([], trans(exp)) 