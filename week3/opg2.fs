module opg2
    //#load "opg2.fs"; open opg2;;
     //2. Given the following datatype declaration of binary trees with values in leaves: 

    type 'a tree2 = | LEAF of 'a
                    | NODE of 'a tree2 * 'a tree2
    //a. Define a value, t, that represents the following tree. (Again, I apologize for the poor drawing.) 
    let t = NODE(NODE(LEAF(1),LEAF(2)),NODE(LEAF(4),LEAF(5)))

    //b. Implement a function sum that returns the sum of the elements in the leaves of a tree. (Test the function on t.) 

    let rec sum = function
        | LEAF x -> x
        | NODE (b1,b2) -> sum b1 + sum b2
    // sum t -> 12

    //c. Implement a function leaves that returns a list of the leaves of a tree, from left to right

    let rec leaves = function
        | LEAF x -> [x]
        | NODE (b1, b2) -> leaves b1 @ leaves b2
    //leaves t -> [1; 2; 4; 5]



