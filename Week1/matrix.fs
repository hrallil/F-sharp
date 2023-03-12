module Matrix
(*OPGAVE 3*)
(*
|a b|
|c d|
*)

let smult x ((a, c), (b, d)) = 
    ((x*a, x*c), (x*b, x*d))

// smult 5 ((6,7),(0,2));; -> ((30, 35), (0, 10))
(*
|30  35|
|0   10|  
*)

let det ((a, c), (b, d)) = 
    a * d - b * c

let transpose ((a:int, b:int), (c:int, d:int)) = 
    ((a,c),(b,d))

let add ((a1:int, b1:int), (c1:int, d1:int)) ((a2:int, b2:int), (c2:int, d2:int)) = 
    ((a1 + a2, b1 + b2), (c1 + c2, d1 + d2))

let ( .* ) ((a1:int, b1:int), (c1:int, d1:int)) ((a2:int, b2:int), (c2:int, d2:int)) =
    ((a1 * a2 + b1 * c2, a1 * b2 + b1 * d2), (c1 * a2 + d1 * c2, c1 * b2 + d1 * d2))
(*
       |a2 b2|
       |c2 d2|
|a1 b1||a2 b2|
|c1 d1||c2 d2|
*)

