module Week2

    //1. [Code-together] Modify the solution to exercise 2 from last week so that it detects when there are no solutions to the equation
    type roots = |Zero | One of float|Two of float*float
    let supSolve (a:float,b:float,c:float) = 
        let d = b**2 - 4.0*a*c
        if d < 0 then Zero 
        else if d = 0 then One (-b / 2.0*a)
        else Two (-b - sqrt d / 2.0*a, -b + sqrt d / 2.0*a)

    //2. [Code-together]  Implement a function called product that takes a list of numbers as input and returns the product of these numbers

    let rec patProd l = 
        match l with 
            | [] -> 1 
            | x :: l -> x * patProd l 
    
    let rec funProd = function
        | [] -> 1
        | x::l -> x * funProd l

    // 4. [hand-in] Rewrite the definition of function fac from exercise 4 from last week so that it becomes iterative
    let rec loop i r=
        if i > 0 then 
            loop (i-1)(r*i)
        else
            r
    let fac n = loop n 1 

    //5. Implement a function count x ys that returns the number of times x occurs in the list ys. 
    let rec count x = function  
        | [] -> 0
        | n::_l -> if n = x  then 1 + count x _l else count x _l

    //> count 5 [1;5;6];; -> 1

    // this is a function 'f' that takes one input and adds 5 to its output. > f 5 -> 10, f 1 -> 6
    let f = (+) 5

    // a -> b -> c oversæt:
    // (a -> b) -> c NEJ
    // a -> (b -> c) JA

    // 6. implement a function countMatches f ys that returns the number of elements in the list ys for which the function f returns true.
    List.map (fun x -> x<5)[1;3;6]

    let rec countMatches f = function
        | [] -> 0
        | n::_x -> if f n = true then 1 + countMatches f _x else countMatches f _x
        
    //countMatches (fun x -> x<5)[1;3;6];; -> 2
    
    (* 7. (4.12) Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
    integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
    Test the function on different predicates (e.g., p(x) = x > 0).

    let rec sum f = function
        | [] -> []
        | n::_x -> if f n = true then true::sum f _x else false::sum f _x
    
    let f x = x<0;;
    sum f [1;3;2;5;-2;-5];;  -> [false; false; false; false; true; true]
    *)

    let rec sum f = function
        | [] -> 0
        | n::_x -> if f n = true then n + sum f _x else sum f _x

    // let f x = x<0;;
    // sum f [1;3;2;5;-2;-5];; -> -7


    //8. [Hand-in] Implement a function split such that

    let rec split1 = function
        | []-> ([],[])
        | x::_l -> let (l1,l2) = split1 _l in if x % 2 = 0 then (x::l1,l2) else (l1,x::l2)


    let rec split = function
        |[] -> ([],[])
        |[x] -> ([x],[])
        |x::y::_l -> let (l,r) = split _l in (x::l, y::r)

    //9. 


