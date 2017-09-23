(* Define a custom exception *)
exception EmptyList;;

(* Lists must be homogenous - all elements of same type *)
[1; 2];;
(* [1; true];; will err! *)


(* cons constructor is how lists are defined *)
(* elem::list *)
1::[2; 3]


(* A list of numbers that we'll use in examples *)
let nums = [1;2;3;4;5;6];;


(* ################################################ *)


(* Length of a list *)
let rec len l = match l with
                [] -> 0
            |   x::xs -> 1 + (len xs)
    ;;

len nums;; (* 3 *)


(* First element *)
let hd l = match l with
                [] -> raise EmptyList
            |   x::xs -> x
    ;;

hd nums;; (* 1 *)


(* Everything except the first element *)
let tl l = match l with
                [] -> raise EmptyList
            |   x::xs -> xs
    ;;

tl nums;; (* [2;3] *)

(* ################################################ *)


(* Append two lists *)
(* Non tail recursive *)
(* Still O(n) ? *)
let rec append l1 l2 = match l1 with
                [] -> l2
            |   x::xs -> x::(append xs l2)
    ;;

append [1;2;3] [4;5;6];;


(* Reversal *)
(* Non tail recursive *)
let rec reverse l = match l with
                [] -> []
            |   x::xs -> append (reverse xs) [x]
    ;;

reverse nums;;


(* Tail recursive *)
let rec rev_help l store = match l with
                [] -> store
            |   x::xs -> rev_help xs (x::store)
    ;;

let rec reverse_2 l = rev_help l [];;

reverse_2 nums;;


(* ################################################ *)


(* Map a function to a list *)
let rec map f l = match l with
                [] -> []
            |   x::xs -> (f x)::(map f xs)
    ;;

let add_1 x = x + 1;;

map add_1 nums;;


(* Filter elements that satisfy a predicate *)
let rec filter p l = match l with
                [] -> []
            |   x::xs -> if p x then
                            x::filter p xs
                        else
                            filter p xs
    ;;

let is_even x = x mod 2 = 0;;

filter is_even nums;;


(* Convert a pair of lists to list of pairs *)
let rec zip l1 l2 = match (l1, l2) with
                ([], []) -> []
            |   ([], l) -> raise EmptyList
            |   (l, []) -> raise EmptyList
            |   (x::xs, y::ys) -> (x, y)::(zip xs ys)
    ;;

let double x = 2 * x;;
let double_nums = map double nums;;

zip nums double_nums;;


(* ################################################ *)


(* Reduce type functions *)

(* Combine a list under some operation *)

(* Sum *)
let rec sum l = match l with
                [] -> 0
            |   x::xs -> x + sum xs
    ;;

sum nums;; (* 21 *)


(* Product *)
let rec prod l = match l with
                [] -> 1
            |   x::xs -> x * prod xs
    ;;

prod nums;; (* 720 *)


(* Fold is a generic reduce *)


(* Not tail recursive: BAD! *)
let rec foldr f e l = match l with
                [] -> e
            |   x::xs -> f x (foldr f e xs)
    ;;

let add x y = x + y;;
let sum_fold = foldr add 0;;
sum_fold nums;; (* 15 *)


(* Tail recursive *)
(* Stores current accumulation in e *)
let rec foldl f e l = match l with
                [] -> e
            |   x::xs -> foldl f (f e x) xs
    ;;

let mul x y = x * y;;
let prod_fold = foldl mul 1;;
prod_fold nums;; (* 120 *)

(* ################################################ *)
