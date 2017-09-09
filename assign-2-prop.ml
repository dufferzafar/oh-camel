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
let rec print_set_str = print_set print_string;;

let print_int i = print (string_of_int i);;

(*
    Set related helper functions
*)

let member x s = List.mem x s;;
let rec union s1 s2 =
    match s1 with
        [] -> s2
    | h::t ->
        if member h s2 then
            union t s2
        else
            h :: (union t s2)
;;


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
    print_prop: prop -> unit

    prints a proposition using standard logic syntax
*)
let rec print_prop p =
    match p with
    | P s              -> print_string s
    | T                -> print_string "T"
    | F                -> print_string "F"
    | Not p1           -> print_string "~"; print_prop p1;
    | And (p1, p2)     -> print_string "("; print_prop p1; print_string " ^ "; print_prop p2; print_string ")";
    | Or (p1, p2)      -> print_string "("; print_prop p1; print_string " v "; print_prop p2; print_string ")";
    | Implies (p1, p2) -> print_string "("; print_prop p1; print_string " -> "; print_prop p2; print_string ")";
;;


(*
    height: prop -> int

    returns the height of a proposition
    (height of the operator tree, counting from 0)
*)

let rec height p =
    match p with
    | P _              -> 0
    | T | F            -> 0
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
    | T | F            -> 1
    | Not p1           -> 1 + size(p1)
    | And (p1, p2)
    | Or (p1, p2)
    | Implies (p1, p2) -> 1 + (size p1) + (size p2)
;;


(*
    letters: prop -> string set

    returns the set of propositional variables
    that appear in a proposition
*)

(* TODO: Use "real" sets instead of lists? *)
let rec letters p =
    match p with
    | P p1             -> [p1]
    | T | F            -> []
    | Not p1           -> letters p1
    | And (p1, p2)
    | Or (p1, p2)
    | Implies (p1, p2) -> union (letters p1) (letters p2)
;;


(*
    Examples of some propositions

    Used for testing throughout the assignment
*)

(* ((a ^ b) v (c ^ d)) *)

let example_1 = Or( And( P("a"), P("b") ), And( P("c"), P("a") ) );;

print_int (size example_1);;
print_int (height example_1);;
print_set_str (letters example_1);;
