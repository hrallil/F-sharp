module opg4
    (* 4. Add abstract syntax of multiplication expression to the language of arithmetic expressions (represented by a new node MUL)
    and add a new instruction IMUL to the low-level instruction set. Then extend the interpreter, 
    the virtual machine, and the compiler to handle the new constructs correctly.*)

    // Expressions
    type exp =  | INT of int 
                | NEG of exp
                | ADD of exp * exp 
                | MUL of exp * exp
                | SUB of exp * exp
                | DIV of exp * exp
                | MOD of exp * exp
                | EQ  of exp * exp
                | LT  of exp * exp


    //instructions available in the stack.
    type inst = 
        | IPUSH of int
        | IADD
        | IMUL 
        | ISUB
        | IDIV
        | IMOD
        | IEQ
        | ILT

    // interpreter, returns a value.
    let rec eval  = function
            | INT i -> i
            | NEG e -> -(eval e) 
            | ADD (e1, e2)   -> eval e1 + eval e2
            | MUL (e1, e2)   -> eval e1 * eval e2
            | SUB (e1, e2)   -> eval e1 - eval e2
            | DIV (e1, e2)   -> eval e1 / eval e2
            | MOD (e1, e2)   -> eval e1 % eval e2
            | EQ  (e1, e2)   -> if eval e1 = eval e2 then 1 else 0
            | LT  (e1, e2)   -> if eval e1 < eval e2 then 1 else 0
 

    // compiler, how we append to the stack, returns a stack of instructions.
    let rec comp = function
        | INT i-> [IPUSH i]
        | NEG e-> [IPUSH 0] @ comp e @ [ISUB]
        | ADD (e1, e2)   -> comp e1 @ comp e2 @ [IADD]
        | MUL (e1, e2)   -> comp e1 @ comp e2 @ [IMUL]
        | SUB (e1, e2)   -> comp e1 @ comp e2 @ [ISUB]
        | DIV (e1, e2)   -> comp e1 @ comp e2 @ [IDIV]
        | MOD (e1, e2)   -> comp e1 @ comp e2 @ [IMOD]
        | EQ  (e1, e2)   -> comp e1 @ comp e2 @ [IEQ]
        | LT  (e1, e2)   -> comp e1 @ comp e2 @ [ILT]


    //4*5 -lex> ["4","*","5"] -parse> MUL(INT 4,INT 5) -compiler> [321,324,1542]
    //                                                 -interpreter> 20