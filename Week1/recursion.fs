module Recursion
    let rec fac n = 
        if n = 0 then 1 else n * fac(n-1)

    let rec sumUpTo n:int = 
        if n = 0 
        then 0
        else n + sumUpTo (n - 1)