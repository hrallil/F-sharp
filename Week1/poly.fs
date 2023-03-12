(*
    printfn "%A" (insert 4 [true]);;
*)


(*OPGAVE 2*)
module Poly
//2.grad polynomie fra opgave
let poly (x: float) = (2.0 * x **2.0 + 4.0 * x - 1.0)

//Find nulpunkt på 2.grad polynomie
let vertex (a, b, c) = 
    (-b / (2.0 * a), -b * b / (4.0 * a) + c)

//Find nulpunkt på 2.grad polynomie
let solve (a:float,b:float,c:float) =
    let d = b**2 - 4.0*a*c 
    if d<0 then printfn "d<0, pick new (a,b,c)"
    ((-b - sqrt d) / 2.0*a,(-b + sqrt d) / 2.0*a)


//genrelt 2.grad polynomie   
let eval (a,b,c) (x:float) = 
    a*x**2+b*x+c
// eval (2.0, 4.0, -1.0) 1 -> 5.0
// eval (2.0, 4.0, -1.0) -1 -> -3.0
