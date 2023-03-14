module Recursion
    let rec fac n = 
        if n = 0 then 1 else n * fac(n-1)

    let rec sumUpTo n:int = 
        if n = 0 
        then 0
        else n + sumUpTo (n - 1)

    //pattern machting
    let rec patSum l = 
        match l with 
            | [] -> 0 
            | x :: l -> x + patSum l 
    
    let rec patProd l = 
        match l with 
            | [] -> 1 
            | x :: l -> x * patProd l 

    let rec append l1 l2 = 
        match l1 with 
            | [] -> l2
            | x::l1 -> x :: append l1 l2

    //function keyword 
    let rec funSum : int list -> int = function
        | [] -> 0
        | x::l -> x + funSum l
    
    
    // Quicksort
    let rec partition p = function 
    | []    -> ([],[])
    | y :: ys -> let (l,r) = partition p ys  in if y <= p then (y::l,r) else (l,y::r)

    let rec quickSort = function
        | [] -> []
        | [x] -> [x]
        | p :: xs -> let (l,r) = partition p xs in quickSort l @ [p] @ quickSort r


  (*
    //reverse 
    let rec reverse : 'a list -> 'a list = function
        | [] -> []
        | [x,y] -> [y,x]
        | x :: xs ->  (reverse xs) :: x  

        *)