(*
    Some helper functions.
*)

let print = print_endline;;

let print_bool b =
    if b then
    print "true"
else
    print "false"
;;

let rec print_list print_elem = function
    | [] -> ()
    | h::t ->
        print_elem h;
        print_string "; ";
        print_list print_elem t
;;

let rec print_list_int = print_list print_int;;
let rec print_list_char = print_list print_char;;


(*
    =====================================================
    ====================== Part A =======================
    =====================================================

    A set is represented as a list without any duplicates
*)

(*
    Example of some sets
    Used throught for testing, demo etc.
*)

let numbers = [0;1;2;3;4;5;6;7;8;9];;
let odds = [1;3;5;7;9];;
let evens = [0;2;4;6;8];;

let chars = ['a';'b';'c';'d'];;


(*
    emptyset - returns the representation of an empty set

    An empty set has no elements, hence no duplicates
    This trivially maintains the representation invariant
*)
let emptyset = [];;


(*
    member x s - returns true if and only if x is in s

    Could simply delegate this:
    let member x s = List.mem x s;
*)

let rec member x s =
    match s with
        [] -> false
    | h::t -> x = h || member x t
;;

print "Demo: member";;
print_bool (member 3 evens);; (* false *)
print_bool (member 3 odds);;  (* true *)
print "";;


(*
    cardinality s - returns the number of elements in the set s

    Could simply delegate this:
    let cardinality s = List.length x s;
*)

let rec length_left_recur s accu =
    match s with
        [] -> accu
    | h::t -> length_left_recur t 1+accu
;;

let cardinality s = length_left_recur s 0;;

print "Demo: cardinality";;
print (string_of_int (cardinality numbers));; (* 10 *)
print "";;

(*
    union s1 s2 - returns the union of sets s1 and s2

    TODO: Proof
*)

let rec union s1 s2 =
    match s1 with
        [] -> s2
    | h::t ->
        if member h s2 then
            union t s2
        else
            h :: (union t s2)
;;

print "Demo: union";;

(* Should print both odds & evens *)
print_list_int (union odds evens);;
print "";;

(* Should print all numbers only once *)
print_list_int (union evens numbers);;

print "";;
