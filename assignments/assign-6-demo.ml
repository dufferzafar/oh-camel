(*

let program1 =
[
    Fact(Node("append", [C "[]"; V "L"; V "L"]));
    Rule(
        Node("append", [[X|Xs], V "M", [X|Z]),
        [
            Node("append", [V "Xs", V "M" , V "Z"])
        ]
    );
];;

let query1_1 = [Node("append", [ [1],[2,3],[1,3] ]];;
let query1_2 = [Node("append", [ [1,2],[2,3],X ]];;
let query1_3 = [Node("append", [ [],[] ]];;
let query1_4 = [Node("append", [ [4,5,6],X,[4,5,6,7,8,9,10] ]];;

*)

let program2 =
[
    Fact( Node("male", [C "pandu"]) );
    Fact( Node("male", [C "arjun"]) );
    Fact( Node("male", [C "nakul"]) );
    Fact( Node("female", [C "kunti"]) );
    Fact( Node("female", [C "madri"]) );
    Fact( Node("female", [C "draupadi"]) );

    Fact( Node("married", [C "pandu"; C "kunti"]) );
    Fact( Node("married", [C "pandu"; C "madri"]) );
    Fact( Node("married", [C "arjun"; C "draupadi"]) );
    Fact( Node("married", [C "nakul"; C "draupadi"]) );

    Rule(
        Node("married", [V "P"; V "Q"]),
        [
            Node("married", [V "Q"; V "P"])
        ]
    );

    Rule(
        Node("wife", [V "A"; V "B"]),
        [
            Node("married", [V "A"; V "B"]);
            Node("male", [V "A"]);
            Node("female", [V "B"])
        ]
    );

    Rule(
        Node("cowife", [V "C"; V "D"]),
        [
            Node("married", [V "E"; V "C"]);
            Node("married", [V "E"; V "D"]);
            Node("female", [V "C"]);
            Node("female", [V "D"]);
            (* X \= Y *)
        ]
    );

    Rule(
        Node("husband", [V "F"; V "G"]),
        [
            Node("married", [V "G"; V "F"]);
            Node("male", [V "G"]);
            Node("female", [V "F"])
        ]
    );

    Rule(
        Node("cohusband", [V "H"; V "I"]),
        [
            Node("married", [C "H"; C "J"]);
            Node("married", [C "I"; C "J"]);
            Node("male", [C "H"]);
            Node("male", [C "I"]);
            (* X \= Y *)
        ]
    );
]

let query2_1 = [Node("male", [C "krishna"])];;
let query2_2 = [Node("married", [C "arjun"; V "X"])];;
let query2_3 = [Node("married", [V "X"; C "draupadi"])];;
let query2_4 = [Node("married", [V "X"; V "Y"])];;
let query2_5 = [Node("cohusband", [C "arjun"; C "nakul"]); Node("cowife", [C "madri"; C "kunti"])];;
let query2_6 = [Node("cowife", [V "X"; C "kunti"])];;

let program3 =
[
    Fact( Node("h", [V "J"; C "d"; C "u"]) );
    Fact( Node("h", [V "J"; C "t"; C "b"]) );
    Fact( Node("h", [V "J"; C "f"; C "b"]) );

    Rule(
        Node("h", [V "J"; Node("p", [V "E"; V "F"]); Node("r", [V "T"; V "U"])]),
        [
            Node("h", [V "J"; V "E"; V "T"]);
            Node("h", [V "J"; V "F"; V "U"])
        ]
    );

    Rule(
        Node("h", [V "J"; Node("n", [V "K"]); V "M"]),
        [
            Node("h", [V "J"; V "K"; Node("r", [V "M"; V "N"])])
        ]
    );

    Rule(
        Node("h", [V "J"; Node("m", [V "K"]); V "N"]),
        [
            Node("h", [V "J"; V "K"; Node("r", [V "M"; V "N"])])
        ]
    );
];;

let query3_1 = [Node( "h", [ C "[a,b,c]"; Node("p", [C "t"; V "Z"]); Node("r", [C "b"; C "u"])] )];;
let query3_2 = [Node( "h", [ C "[xx,yy]"; Node("n", [Node("p", [C "t"; C "f"])]); C "u" ] )];;
let query3_3 = [Node( "h", [ C "[xx,yy]"; Node("n", [Node("p", [C "t"; V "Y"])]); V "X" ] )];;

(*
let test1 = solver program1 query1_1;;
let test2 = solver program1 query1_2;;
let test3 = solver program1 query1_3;;
let test4 = solver program1 query1_4;;
*)

let test5 = solver program2 query2_1;;
let test6 = solver program2 query2_2;;
let test7 = solver program2 query2_3;;
let test8 = solver program2 query2_4;;
let test9 = solver program2 query2_5;;
let test10 = solver program2 query2_6;;

let test11 = solver program3 query3_1;;
let test12 = solver program3 query3_2;;
let test13 = solver program3 query3_3;;

