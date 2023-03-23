module program
    // look ahead then look at excercise 6.8

    type exp =  | INT of int 
                | ADD of exp * exp 

    let rec eval  = function
            |INT i -> i
            |ADD(e1,e2) -> eval e1 + eval e2



    type inst = 
        | IPUSH of int
        | IADD

    let rec comp = function
        | INT i -> [IPUSH i]
        | ADD (e1,e2) -> comp e1 @ comp e2 @ [IADD]
