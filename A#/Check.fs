module Check
    type typ = 
        | TINT 
        | TBOOL 

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
                                        | (_, _)       -> failwith "\x1b[31mTYPE ERROR: Syntax.ADD (+), only takes exp of TINT (int)\x1b[0m"
        | Syntax.SUB (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "\x1b[31mTYPE ERROR: Syntax.SUB (-), only takes exp of TINT (int).\x1b[0m"
        | Syntax.MUL (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "\x1b[31mTYPE ERROR: Syntax.MUL (*), only takes exp of TINT (int).\x1b[0m"
        | Syntax.DIV (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "\x1b[31mTYPE ERROR: Syntax.DIV (/), only takes exp of TINT (int).\x1b[0m"  
        | Syntax.MOD (e1,e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TINT
                                        | (_, _)       -> failwith "\x1b[31mTYPE ERROR: Syntax.MOD (%), only takes exp of TINT (int).\x1b[0m"   
        | Syntax.LET (x, e1, e2) -> let typeOfe1 = typeError env e1
                                    typeError ((x, typeOfe1) :: env) e2   
        // Boolean type checks      
        | Syntax.EQ (e1, e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (TBOOL,TBOOL)-> TBOOL
                                        | (TBOOL, TINT)-> failwith "\x1b[31mTYPE ERROR: Syntax.EQ (=), both arguments have to be of same type.\x1b[0m"
                                        | (TINT, TBOOL)-> failwith "\x1b[31mTYPE ERROR: Syntax.EQ (=), both arguments have to be of same type.\x1b[0m"
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.EQ (=), unkown type. This operator only allows for TINT (int) and TBOOL (bool)\x1b[0m"
        | Syntax.NEQ (e1, e2)   ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (TBOOL,TBOOL)-> TBOOL
                                        | (TBOOL, TINT)-> failwith "\x1b[31mTYPE ERROR: Syntax.NEQ (!=), both arguments have to be of same type.\x1b[0m"
                                        | (TINT, TBOOL)-> failwith "\x1b[31mTYPE ERROR: Syntax.NEQ (!=), both arguments have to be of same type.\x1b[0m"
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.EQ (!=), unkown type. This operator only allows for TINT (int) and TBOOL (bool)\x1b[0m"
        | Syntax.LT (e1, e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.LT (<), only takes types exp of TINT (int).\x1b[0m"
        | Syntax.GT (e1, e2)    ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.GT (>), only takes types exp of TINT (int).\x1b[0m"

        | Syntax.LTEQ (e1, e2)  ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.LTEQ (>=), only takes types exp of TINT (int).\x1b[0m"

        | Syntax.GTEQ (e1, e2)  ->  match (typeError env e1, typeError env e2) with
                                        | (TINT, TINT) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.GTEQ (<=), only takes types exp of TINT (int).\x1b[0m"

        | Syntax.AND (e1, e2)   ->  match (typeError env e1, typeError env e2) with
                                        | (TBOOL, TBOOL) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.AND (&&), only takes types exp of TBOOL (bool).\x1b[0m"

        | Syntax.OR (e1, e2)    -> match (typeError env e1, typeError env e2) with
                                        | (TBOOL, TBOOL) -> TBOOL
                                        | (_,_)        -> failwith "\x1b[31mTYPE ERROR: Syntax.OR (||), only takes types exp of TBOOL (bool).\x1b[0m"
        //Bigger type checks
        | Syntax.IF (e1, e2, e3)->  match typeError env e1 with
                                        | TBOOL ->  let t2 = typeError env e2
                                                    let t3 = typeError env e3
                                                    if t2 = t3 then
                                                        t2
                                                    else
                                                        failwith "\x1b[31mTYPE ERROR: Syntax.IF (if bool then 'a else 'a), both branches must be same type.\x1b[0m"
                                        | _ -> failwith "\x1b[31mTYPE ERROR: Syntax.IF (if bool then 'a else 'a), first element must be a TBOOL (bool).\x1b[0m"
        | Syntax.WRITE e        ->  match typeError env e with
                                        | TINT
                                        | _     -> failwith "\x1b[31mright now syntax.WRITE can only write TINT\x1b[0m"
        | Syntax.READ           ->  TINT
        
        (*| Syntax.CALL (name, es)->  let rec findTypes env = function
                                        |[]     -> []
                                        | e::es -> typeError env e @ findTypes env es  
                                    findTypes es

        | Syntax.funcDef(f,x,e) ->  typeError (x::env) e*)
    