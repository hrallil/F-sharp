module week3
    //1. Given the following datatype declaration of binary trees with values in nodes:
    type 'a tree =  | LEAF
                    | NODE of 'a * 'a tree * 'a tree 
    //a) Define a value, t, that represents the following tree. (I apologize for the poor drawing.)
    let t = NODE(5, NODE(3, LEAF, NODE(2, LEAF, LEAF)), NODE(1,NODE(4, LEAF, LEAF),LEAF))

    //b) Implement a function "countLeaves" that returns the number of LEAF elements in a tree. (Test the function on t.)
    let rec countLeaves = function 
        | LEAF ->  1
        | NODE (x, y, z: 'a tree) -> 0 + countLeaves y + countLeaves z
    //countLeaves t -> 6

    //c) Implement a function countNodes that returns the number of NODEs in a tree.
    let rec countNodes = function 
        | LEAF ->  0
        | NODE (x, y, z: 'a tree) -> 1 + countNodes y + countNodes z
    //countNodes t -> 5
    
    //d) Implement a function sum that returns the sum of the elements in the nodes of a tree.
    let rec sum = function
        | LEAF -> 0 
        | NODE(x,y,z) -> x + sum y + sum z
    // sum t -> 15

    //e) Implement a function product that returns the product of the elements in the nodes of a tree.
    let rec prod = function
        | LEAF -> 1
        | NODE(x,y,z) -> x * prod y * prod z
    //prod t -> 120

    //f) Implement a function double that takes an int tree as input and returns a new int tree all of whose node vales are two times those of the input.
    let rec double = function
        | LEAF -> LEAF
        | NODE(x,y,z) -> NODE(x*2,double y, double z)

    //let f:('a->'b)

    //g)Implement a function
    (*treemap : ('a -> 'b) -> 'a tree -> 'b tree
    that is similar to List.map in that it takes a function f and a tree t and returns a new tree of the same shape as t
    but where f has been applied to all the values in the nodes of the original tree.

    Can you express the function double from the previous exercise using treemap? *)

    let rec treemap f = function
        | LEAF -> LEAF
        | NODE(x,y,z) -> NODE (f x, treemap f y, treemap f z) //?
    
    // treemap (( * )2) t ->  NODE (10, NODE (6, LEAF, NODE (4, LEAF, LEAF)), NODE (2, NODE (8, LEAF, LEAF), LEAF))
    // treemap (fun x -> x*2) t ->  NODE (10, NODE (6, LEAF, NODE (4, LEAF, LEAF)), NODE (2, NODE (8, LEAF, LEAF), LEAF))

    //h) Implement a function
    (* treefold : ('a -> 'b -> 'b -> 'b) -> 'b -> 'a tree -> 'b 
    that takes a function f of three arguments, a value b, and a tree t, and that recurses over the tree by returning b when it meets a LEAF and that applies f when it meets a NODE.
    *)


