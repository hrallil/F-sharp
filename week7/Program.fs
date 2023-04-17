

    let rec check env = function
        | INT i     -> TINT
        | VAR x     -> lookUp x env
        | ABS (x,t,e) -> let t' = check ((x,t)::env) e
                                        TFUN (t,t')
        | APP (e1,e2) -> match check env e1 with
                                    |TFUN (t2',t) -> let t2 = check env e2 in if t2=t2' then t else failwith "Type error" 
