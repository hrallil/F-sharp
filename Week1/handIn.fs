module HandIn
    let tick (h, m, s) =
        if s + 1 <> 60 then (h, m, s + 1)   
        elif m + 1 <> 60 then (h, m + 1, 0)
        elif h + 1 <> 24 then (h + 1, 0, 0)
        else (0, 0, 0)

    let before (h1,m1,s1)(h2,m2,s2) = 
        let a = h1*60*60 + m1*60 + s1
        let b = h2*60*60 + m2*60 + s2
        (a<b)

    let rec tickFrom ((h, m, s) as t) =
        printf "%02d:%02d:%02d " h m s
        if System.Console.ReadLine() = "stop" then
          (h, m, s)
        else
          tickFrom (tick t)



        