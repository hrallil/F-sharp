module opg3
    //3. Any “uncurried” function f : A * B -> C can be translated into an equivalent “curried” function f' : 
    // A -> B -> C and vice versa, and this can be done automatically: 
    //a) Implement a function curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c) 
    //that takes an uncurried function as input an returns the corresponding curried function.
    let curry f = fun a -> fun b -> f (a,b)
    // f:('a * 'b -> 'c) ->a: 'a -> b:'b -> 'c

    //b. Implement a function uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c) that takes a curried function as input an returns the corresponding uncurried function. 
    let uncurry f = fun (a,b) -> f a b 
    // f: ('a -> 'b) -> a: 'a *b: 'c -> 'b

    