module Week2

    //1. [Code-together] Modify the solution to exercise 2 from last week so that it detects when there are no solutions to the equation
    type roots = |Zero | One of float|Two of float*float
    let solve (a:float,b:float,c:float) = 
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
    let rec count5 x = function  
        | [] -> 0
        | n::_l -> if n = x  then 1 + count5 x _l else count5 x _l

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
        | n::_x -> if f n then 1 + countMatches f _x else countMatches f _x
        
    //countMatches (fun x -> x<5)[1;3;6];; -> 2
    
    (* 7. (4.12) Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
    integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
    Test the function on different predicates (e.g., p(x) = x > 0).

    let rec sum f = function
        | [] -> []
        | n::_x -> (f n)::sum f _x 
    
    let f x = x<0;;
    sum f [1;3;2;5;-2;-5];;  -> [false; false; false; false; true; true]
    *)

    let rec sum f = function
        | [] -> 0
        | n::_x -> if f n = true then n + sum f _x else sum f _x

    // let f x = x<0;;
    // sum f [1;3;2;5;-2;-5];; -> -7


    //8. [Hand-in] Implement a function split such that: split [x1; x2; x3; x4; ...] = ([x1; x3; ...], [x2; x4; ...])

    let rec splitOddEven = function
        | []-> ([],[])
        | x::_l -> let (l1,l2) = splitOddEven _l in if x % 2 = 0 then (x::l1,l2) else (l1,x::l2)

    


    let rec split = function
        |[] -> ([],[])
        |[x] -> ([x],[])
        |x::y::_l -> let (l,r) = split _l in (x::l, y::r)

    //9. A list of integers [x1; x2; x3; ...; xn] is weakly ascending if  x1 ≤ x2 ≤ x3 ≤ ... ≤ xn. 
    //a) Implement a function isWeaklyAscending xs that returns true if the list xs is weakly ascending (sorted), and false otherwise

    let rec isWeaklyAscending = function 
        | [] -> true
        | [_] -> true
        | x::y::_l -> if x<=y then isWeaklyAscending (y::_l) else false

    //b) Implement a function count x ys that returns the number of times the integer x occurs in the weakly ascending list ys. 
    let rec count x = function
        | [] -> 0
        | n::_l -> if n = x then 1 + count x _l else count x _l

    // > count 5 [1;3;5;6;34;5;7];; -> 2
    // > count 5 [1;5;5;6;5;5;7];; -> 4
    // > count 5 [1;2;2;6;2;25;7];; -> 2

    //c) Implement a function union xs ys that returns a weakly ascending list containing the elements that occur in either of the weakly ascending lists xs or ys (or both). For instance 
    // EXAPLE:  > union [2; 3; 3; 5; 8] [1; 3; 3; 3; 4; 5] -> [1; 2; 3; 3; 3; 3; 3; 4; 5; 5; 8]

    let rec union xs ys = 
        match (xs,ys) with 
            | (_,[]) -> xs
            | ([],_) -> ys
            | (x::_lx, y::_ly) -> if x<y then x::union _lx ys else y::union xs _ly

    //10. [Hand-in] Notice that union takes two sorted lists as input and returns a sorted list containing all the elements of the two inputs. This operation is also called merging. 
    let rec mergesort = function
        | [] -> []
        | [x] -> [x]
        | xs -> let l,r = split xs in union (mergesort l)(mergesort r) 

    //> mergesort [1;5;3;4;7;2;6;8;7;4;9];; -> [1; 2; 3; 4; 4; 5; 6; 7; 7; 8; 9]
    //> mergesort [] -> []
    //> mergesort [1] -> [1]
    //> mergesort [1;1] -> [1;1]
    //> mergesort [5;4;3;2;1] -> [1;2;3;4;5]