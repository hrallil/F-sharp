module calculator

    type Instruction =  | ADD | SUB | MUL | DIV | SIN
                        | COS | LOG | EXP  | PUSH of float

    type Stack = float list

    let intpInstr (st:Stack) inst=
        match inst, st with
        | ADD, a::b::_st -> a+b::_st
        | SUB, a::b::_st -> a-b::_st
        | MUL, a::b::_st -> a*b::_st
        | DIV, a::b::_st -> a/b::_st
        | COS, a::_st -> System.Math.Cos(a)
        | LOG, a::_st -> System.Math.Log(a,10)
        | SIN, a::_st -> System.Math.Sin(a)
        | EXP, a::b::_st -> a**b::_st
        | PUSH i, st -> i::st
        | _ -> []
    
    type instStack = Instruction list

    //intpProg list of inst -> float 
    let rec intpProg = function
        | [x] -> x
        | [PUSH i] -> i
        | ADD::a::b::_il -> intpInstr (PUSH (a + b)::_il)
        | SUB::a::b::_il -> intpInstr (PUSH (a - b)::_il)
        | MUL::a::b::_il -> intpInstr (PUSH (a * b)::_il)
        | DIV::a::b::_il -> intpInstr (PUSH (a / b)::_il)
        | COS::a::_il    -> intpInstr (PUSH (System.Math.Cos(a))::_il)
        | LOG::a::_il    -> intpInstr (PUSH (System.Math.Log(a,10))::_il)
        | SIN::a::_il    -> intpInstr (PUSH (System.Math.Sin(a))::_il)
        | EXP::a::b::_il -> intpInstr (PUSH a**b::_il)

        
    // intpProg [PUSH 1.0; PUSH 2.0; ADD]-> 3.0
    // intpProg [PUSH 1.0; PUSH 2.0; ADD;PUSH 4.0; ADD]-> 7.0

        

