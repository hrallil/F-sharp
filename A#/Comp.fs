module comp
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