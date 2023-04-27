module Check
    type typ = 
        | TINT 
        | TBOOL 
        | TFUN 

    let rec lookup x = function
        | []            -> failwith ("unbound: " + x)
        | (y, w) :: env -> if x = y then w else lookup x env


    let rec typeError env = function
        //Simple type checks (MISSING Syntax.LET)
        | Syntax.INT i          ->  TINT
        | Syntax.TRUE           ->  TBOOL
        | Syntax.FALSE          ->  TBOOL
        | Syntax.VAR x          ->  lookup x env
        | Syntax.NEG e          ->  typeError env e
        | Syntax.ADD (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "TYPE ERROR: Syntax.ADD, only takes exp of TINT"
        | Syntax.SUB (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "TYPE ERROR: Syntax.SUB, only takes exp of TINT."
        | Syntax.MUL (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "TYPE ERROR: Syntax.MUL, only takes exp of TINT."
        | Syntax.DIV (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "TYPE ERROR: Syntax.DIV, only takes exp of TINT."  
        | Syntax.MOD (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "TYPE ERROR: Syntax.MOD, only takes exp of TINT."   
        | Syntax.LET (x, e1, e2) -> let typeOfe1 = typeError env e1
                                    typeError ((x, typeOfe1) :: env) e2   
        // Boolean type checks      
        | Syntax.EQ (e1, e2)    ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.LT, only takes types exp of TINT."
        | Syntax.NEQ (e1, e2)   ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.LT, only takes types exp of TINT."
        | Syntax.LT (e1, e2)    ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.LT, only takes types exp of TINT."
        | Syntax.GT (e1, e2)    ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.GT, only takes types exp of TINT."

        | Syntax.LTEQ (e1, e2)  ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.LTEQ, only takes types exp of TINT."

        | Syntax.GTEQ (e1, e2)  ->  match (check env e1, check env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.GTEQ, only takes types exp of TINT."

        | Syntax.AND (e1, e2)   ->  match (check env e1, check env e2) with
                                        | (TBOOL, TBOOL) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.AND, only takes types exp of TBOOL."

        | Syntax.OR (e1, e2)    -> match (check env e1, check env e2) with
                                        | (TBOOL, TBOOL) -> TBOOL
                                        | (_,_)        -> failwith "TYPE ERROR: Syntax.OR, only takes types exp of TBOOL."
        //Bigger type checks
        | Syntax.IF (e1, e2, e3)->  match typeError env e1 with
                                        | TBOOL ->  let t2 = typeError env e2
                                                    let t3 = typeError env e3
                                                    if t2 = t3 then
                                                        t2
                                                    else
                                                        failwith "TYPE ERROR: Syntax.IF, both branches must be same type."
                                        | _ -> failwith "TYPE ERROR: Syntax.IF, first element must be a TBOOL."
        | Syntax.WRITE e        ->  match typeError env e with
                                        | TINT
                                        | _     -> failwith "right now syntax.WRITE can only write TINT"
        | Syntax.READ           ->  TINT
        //| Syntax.CALL (name, es)->  
        //| Syntax.funcDef(f,x,e) ->  typeError (x::env) e
    