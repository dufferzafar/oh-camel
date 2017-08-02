(* Baby Steps *)
"Hello, world"

(* Identity function *)
let id x = x;;

(* A function that always returns true *)
let ctrue x = true;;

(* Negation *)
let cnot x = match x with
            true -> false
        |   false -> true
    ;;

cnot true;; cnot false;;
cnot 1;;

(* Logical And *)
let cand x y = match (x, y) with
            (true, true) -> true
        |   (_, _) -> false
    ;;

cand true false;; cand true true;;
cand 1 2;;

(* Logical Or *)

let cor x y = match (x, y) with
            (false, false) -> false
        |   (_, _) -> true
    ;;

cor true false;; cor false false;;
cor 1 2;;

(* Custom Types *)
(* have to start with lower case letter *)
type color = R | G | B;;


(* Recursive functions need special syntax 'rec' *)
let rec factorial n = if n = 0 then 1
                 else n * (factorial(n-1))
        ;;
