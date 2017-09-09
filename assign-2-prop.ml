(*
    Printing related helper functions
*)

let print = print_endline;;

let print_bool b =
    match b with
    | true  -> print "true"
    | false -> print "false"
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

let rec letters p =
    match p with
    | P s              -> [s]
    | T | F            -> []
    | Not p1           -> letters p1
    | And (p1, p2)
    | Or (p1, p2)
    | Implies (p1, p2) -> union (letters p1) (letters p2)
;;


(*
    truth: prop -> (string -> bool) -> bool

    evaluates a proposition with respect to a given
    truth assignment to the propositional letters
*)

let rec truth p rho =
    match p with
    | P s              -> rho s
    | T                -> true
    | F                -> false
    | Not p1           -> not (truth p1 rho);
    | And (p1, p2)     -> (truth p1 rho) && (truth p2 rho);
    | Or (p1, p2)      -> (truth p1 rho) || (truth p2 rho);
    | Implies (p1, p2) -> (not (truth p1 rho)) || (truth p2 rho);
;;


(*
    nnf: prop -> prop

    converts a proposition into negation normal form where:
        1. all Not's appear just above only prop letters
           and strictly below And's and Or's
        2. all Implies have been replaced by its logically equivalent forms
*)
let rec nnf p =
    match p with
    | P s              -> P s
    | T                -> T
    | F                -> F
    | And (p1, p2)     -> And (nnf p1, nnf p2)
    | Or (p1, p2)      -> Or (nnf p1, nnf p2)
    | Implies (p1, p2) -> Or (nnf (Not p1), nnf p2)

    (* All logic below is "inverted" *)
    | Not q           -> match q with
        | P s              -> p
        | T                -> T
        | F                -> F
        | Not p1           -> nnf p1
        | And (p1, p2)     -> Or (nnf (Not p1), nnf (Not p2))
        | Or (p1, p2)      -> And (nnf (Not p1), nnf (Not p2))
        | Implies (p1, p2) -> And (nnf p1, nnf (Not p2))

(*
    Examples of some propositions

    Used for testing throughout the assignment
*)

(* Small chunks of props that are used to build bigger ones *)
let prop_1 = And( P("a"), Not(P("b")) )
let prop_2 = Implies( P("a"), P("c") )
let prop_3 = Or( Not(P("b")), P("d") )

let prop_ex_1 = Or(prop_1, prop_2);;
let prop_ex_2 = Not(And(prop_2, prop_3));;

let rho_ex_1 s =
    match s with
    | "a" -> true
    | "b" -> false
    | "c" -> false
    | "d" -> true
    | _   -> false
;;

print_string "Ex 1: ";          print_prop prop_ex_1;
print "";

print_string "NNF: ";           print_prop (nnf prop_ex_1);
print "\n";


print_string "Height: ";        print_int (size prop_ex_1);;
print_string "Size: ";          print_int (height prop_ex_1);;
print_string "Letters (set): "; print_set_str (letters prop_ex_1);;
print "\n";

print_string "Truth (rho_ex_1): "; print_bool (truth prop_ex_1 rho_ex_1);;
print "\n\n";

(* ============================================================================ *)

print_string "Ex 2: ";          print_prop prop_ex_2;
print "";

print_string "NNF: ";           print_prop (nnf prop_ex_2);
print "\n";

print_string "Height: ";        print_int (size prop_ex_2);;
print_string "Size: ";          print_int (height prop_ex_2);;
print_string "Letters (set): "; print_set_str (letters prop_ex_2);;
print "\n";

print_string "Truth (rho_ex_1): "; print_bool (truth prop_ex_2 rho_ex_1);;
print "\n";
