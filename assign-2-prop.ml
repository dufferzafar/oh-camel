(*
    Printing related helper functions
*)

let print = print_endline;;

let print_bool b =
    if b then
        print "true"
    else
        print "false"
;;

let rec print_set print_elem s =
    match s with
        [] -> ()
    | h::t ->
        print_elem h;
        print_string "; ";
        print_set print_elem t
;;

let rec print_set_tuple fmt s =
    match s with
        [] -> ()
    | (a,b)::t ->
        Printf.printf fmt a b;
        print_string "; ";
        print_set_tuple fmt t
;;

let rec print_set_int = print_set print_int;;
let rec print_set_char = print_set print_char;;

let print_int i = print (string_of_int i);;

(*
    =====================================================
    =====================================================
*)

(* Proposition Data Type *)
type prop =
    | P of string
    | T | F
    | Not of prop
    | And of prop * prop
    | Or of prop * prop
    | Implies of prop * prop
;;

(*
    height: prop -> int

    returns the height of a proposition
    (height of the operator tree, counting from 0)
*)

let rec height p =
    match p with
    | P _              -> 0
    | T
    | F                -> 0
    | Not p1           -> 1 + height(p1)
    | And (p1, p2)
    | Or (p1, p2)
    | Implies (p1, p2) -> 1 + max (height p1) (height p2)
;;

(*
    size: prop -> int

    returns the number of nodes in a proposition
    (number of nodes in the operator tree)
*)

let rec size p =
    match p with
    | P _              -> 1
    | T
    | F                -> 1
    | Not p1           -> 1 + size(p1)
    | And (p1, p2)
    | Or (p1, p2)
    | Implies (p1, p2) -> 1 + (size p1) + (size p2)
;;


(*
    Examples of some propositions.
*)

(* ((a ^ b) v (c ^ d)) *)

let example_1 = Or( And( P("a"), P("b") ), And( P("c"), P("d") ) );;

print_int (size example_1);;
print_int (height example_1);;
