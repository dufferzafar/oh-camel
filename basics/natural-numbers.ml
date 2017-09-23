(* Natural numbers are recursively defined *)

type nat =  O
        |   S of nat
    ;;

(* elements: 0, 1, 2 *)
O;; S(O);; S(S(O));;

(* Numeric value of a natural number *)
let rec value x = match x with
                O -> 0
            |   S y -> 1 + (value y)
    ;;

value ( S(S(S(S(S(O))))) );; (* 5 *)

(* Addition *)

let rec addition x y = match y with
                O -> x
            |   S z -> S ( addition x z )
    ;;

value ( addition ( S(S(O)) ) ( S(S(S(O))) ) );; (* 2 + 3 = 5 *)

(* Multiplication *)

let rec multiplication x y = match y with
                O -> O
            |   S z -> addition x ( multiplication x z )
    ;;

value ( multiplication ( S(S(O)) ) ( S(S(S(O))) ) );; (* 2 * 3 = 6 *)

(* Exponentiation *)

let rec exponentiation x y = match y with
                O -> S(O)
            |   S z -> multiplication x ( exponentiation x z )
    ;;

value ( exponentiation ( S(S(O)) ) ( S(S(S(O))) ) );; (* 2 ^ 3 = 8 *)

