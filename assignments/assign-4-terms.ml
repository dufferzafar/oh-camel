(*
    Printing related functions
*)

let print s = print_endline s;;

let print_bool b =
    match b with
    | true  -> print "true"
    | false -> print "false"
;;

let print_int i = print (string_of_int i);;

let rec print_set print_elem s =
    match s with
        [] -> ()
    | h::t ->
        print_elem h;
        print_string "; ";
        print_set print_elem t
;;

(*
    Set related functions
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
    Typical list folding operations
*)
let list_max l = List.fold_left (fun acc x -> max acc x) 0 l;;
let list_sum l = List.fold_left (fun acc x -> acc + x) 0 l;;

(*
    =====================================================
    =====================================================
*)


(*
    Term Data Type
*)

type variable = string;;
type symbol   = string;;

type term =
    | V    of variable
    | Node of symbol * (term list)
;;


(*
    Representation of a Signature

type arity = int;;
type signature = (symbol * arity) list;;
*)

(* Count the number of times an element occurs in a list *)
let count e l =
    let count_list = List.map (fun x -> if x = e then 1 else 0) l
    in list_sum count_list
;;

(* Find all symbols occurring in a signature *)
let symbols sign = List.map (fun (symbol, arity) -> symbol) sign;;
let arities sign = List.map (fun (symbol, arity) -> arity) sign;;

(*
    check_sig : signature -> bool

    checks whether the signature is valid:
        1. No negative arities
        2. No repeated symbols
        3. Atleast 1 zero arity
*)
let check_sig sign =

    let rec check sign =
    (
        match sign with
        | [] -> true
        | (symbol, arity) :: rest ->

            (* No negative arity *)
            arity >= 0

            (* No repeated symbols *)
            && (count symbol (symbols sign)) == 1

            (* Recurse! *)
            && check rest
    )

    in

    check sign

    (* Atleast 1 zero arity *)
    && List.mem 0 (arities sign)
;;


(*
    wfterm : term -> signature -> bool

    checks that a given term is well-formed according to the signature
*)
let rec wfterm trm sign =

    match trm with

    | V _ -> true

    | Node (sym, trm_list) ->

        (* Ensure this symbol has proper number of children (based on arity) *)
        List.mem (sym, List.length trm_list) sign

        (* Ensure all the child terms are well formed *)
        && List.fold_left (fun acc t -> acc && (wfterm t sign)) true trm_list
;;

(*
    ht : term -> int

    returns height of a term tree (counting from 0)
*)
let rec ht trm =
    match trm with

    | V _                  -> 0

    | Node (sym, trm_list) -> 1 + list_max (List.map (fun t -> ht t) trm_list)
;;

(*
    size : term -> int

    returns size of a term (number of nodes in)
*)
let rec size trm =
    match trm with

    | V _                  -> 1

    | Node (sym, trm_list) -> 1 + list_sum (List.map (fun t -> size t) trm_list)
;;

(*
    vars : term -> string list

    return the variables occurring in the term
*)
let rec vars trm =
    match trm with

    | V v                -> [v]

    | Node (_, trm_list) -> List.fold_left (fun acc t -> union acc (vars t)) [] trm_list
;;

(*
    Define type for substitution

    A substitution is used to replaces a variable by a term.
*)
type substitution = (variable * term) list;;

(*
    subst : term -> substitution -> term

    Apply a substitution on a term
*)
let rec subst trm sub =
    match trm with

    | V v  ->
    (
        try
            (* Replace this term with its substitution *)
            let _, t = List.find (fun (x, t) -> v = x ) sub in t
        with
            (* If not found just return the variable un-modified *)
            Not_found -> V v
    )

    | Node (sym, trm_list) ->

        Node (sym, List.map (fun t -> subst t sub) trm_list)
;;

exception NOT_UNIFIABLE;;

(*
    occurs : variable -> term -> bool
    Check whether a variable occurs in a term.
*)
let rec occurs x t =
    match t with
      | V v -> v = x
      | Node (_, trm_lst) -> List.exists (occurs x) trm_lst

(*
    mgu : term -> term -> substitution

    returns the most general unifier of two terms (if it exists)
    otherwise raises an exception NOT_UNIFIABLE
*)
let mgu t1 t2 =
    let rec mgu_of_terms p1 p2 =

        match (p1, p2) with

        (* Two different variables *)
        | (V v, V w) ->
        (
            if v <> w then
                [(v, V w)]
            else
                []
        )

        (* A variable and a node *)
        | (V v, (Node _ as t)) | ( Node _ as t, V v) ->
        (
            if not (occurs v t) then
                [(v, t)]
            else
                raise NOT_UNIFIABLE
        )

        (* Two different nodes *)
        | (Node (f, trm_list_1), Node (g, trm_list_2)) ->
        (
            if (f = g) && (List.length trm_list_1 = List.length trm_list_2)
            then
                let paired_lists = List.combine trm_list_1 trm_list_2
                in mgu_of_children paired_lists
            else
                raise NOT_UNIFIABLE
        )

    and mgu_of_children pairs =
    (
        match pairs with
        | [] -> []
        | (c1, c2) :: rest -> union (mgu_of_terms c1 c2) (mgu_of_children rest)
    )

    in mgu_of_terms t1 t2
;;

(*
    compose : substitution -> substitution -> substitution

    Return a composition of two substitutions
*)
let rec compose s1 s2 =

    let rec compose_with_acc s1 s2 acc =
    (
        match (s1, s2) with

        | ([], []) -> acc
        | (l, []) | ([], l) -> union acc l
        | (((v1, t1) :: rest1), ((v2, t2) :: rest2)) ->

            if (occurs v2 t1) then
                compose_with_acc rest1 rest2
                (union acc [(v1, subst t1 [(v2, t2)]); (v2, t2)])
            else
                compose_with_acc rest1 rest2 (union [(v1, t1); (v2, t2)] acc)
    )
    in
    compose_with_acc s1 s2 []
;;

(* Represents a list of substituions that are applied from left to right *)
type composition = substitution list;;

(*
    resolve_composition : composition -> substitution
*)
let resolve_composition comp =
    List.fold_left (fun acc x -> compose acc x) (List.hd comp) comp
;;


(*
    =====================================================
    =====================================================
*)

(* Test Cases *)
let test_sig_ok = [("a", 0); ("b", 1)];;
let test_sig_no_zero = [("a", 1); ("b", 1)];;
let test_sig_negative = [("a", 0); ("b", -1)];;
let test_sig_repeated = [("a", 0); ("a", 1)];;

let sig1 = [("X", 0); ("Y", 0); ("f", 1); ("g", 2); ("h", 3); ("*", 2)];;
let sig2 = [("X", 0); ("Y", 0); ("Z", 0); ("f", 1); ("g", 2); ("f", 3); ("*", 2)];;


print "check_sig";;
print "";;
print_string "test_sig_ok:\t\t";;           print_bool (check_sig test_sig_ok);;
print_string "test_sig_no_zero:\t";;        print_bool (check_sig test_sig_no_zero);;
print_string "test_sig_negative:\t";;       print_bool (check_sig test_sig_negative);;
print_string "test_sig_repeated:\t";;       print_bool (check_sig test_sig_repeated);;
print "";;
print_string "sig1:\t\t\t";;                print_bool (check_sig sig1);;
print_string "sig2:\t\t\t";;                print_bool (check_sig sig2);;
print "\n----\n";;

let term1 = (Node ("f", [V "X"]));;
let term2 = (Node ("g", [V "X"; Node ("h",[Node("f",[V "X"]); V "Y"])]));;
let term3 = (Node ("g", [V "X"; Node ("*",[V "Y";Node ("*",[V "X"; V "Y"])])]));;

let term4 = (Node ("g", [V "X"; Node ("*", [V "Y"; V "X"] )]));;
let term5 = (Node ("g", [V "Z"; Node ("*", [V "X"; V "Z"] )]));;
let term6 = (Node ("g", [V "Z"; Node ("g", [V "X"; V "Z"] )]));;

print "wfterm";;
print "";;
print_string "term 1:\t\t\t";;           print_bool (wfterm term1 sig1);;
print_string "term 2:\t\t\t";;           print_bool (wfterm term2 sig1);;
print_string "term 3:\t\t\t";;           print_bool (wfterm term3 sig1);;
print_string "term 4:\t\t\t";;           print_bool (wfterm term4 sig1);;
print_string "term 5:\t\t\t";;           print_bool (wfterm term5 sig1);;
print_string "term 6:\t\t\t";;           print_bool (wfterm term6 sig1);;
print "\n----\n";;

print "ht";;
print "";;
print_string "term 1:\t\t\t";;           print_int (ht term1);;
print_string "term 2:\t\t\t";;           print_int (ht term2);;
print_string "term 3:\t\t\t";;           print_int (ht term3);;
print_string "term 4:\t\t\t";;           print_int (ht term4);;
print_string "term 5:\t\t\t";;           print_int (ht term5);;
print_string "term 6:\t\t\t";;           print_int (ht term6);;
print "\n----\n";;

print "size";;
print "";;
print_string "term 1:\t\t\t";;           print_int (size term1);;
print_string "term 2:\t\t\t";;           print_int (size term2);;
print_string "term 3:\t\t\t";;           print_int (size term3);;
print_string "term 4:\t\t\t";;           print_int (size term4);;
print_string "term 5:\t\t\t";;           print_int (size term5);;
print_string "term 6:\t\t\t";;           print_int (size term6);;
print "\n----\n";;

print "vars";;
print "";;
print_string "term 1:\t\t\t";;           print_set print_string (vars term1);; print "";;
print_string "term 2:\t\t\t";;           print_set print_string (vars term2);; print "";;
print_string "term 3:\t\t\t";;           print_set print_string (vars term3);; print "";;
print_string "term 4:\t\t\t";;           print_set print_string (vars term4);; print "";;
print_string "term 5:\t\t\t";;           print_set print_string (vars term5);; print "";;
print_string "term 6:\t\t\t";;           print_set print_string (vars term6);; print "";;
print "\n----\n";;

let sub1 = [("X", Node("*", [V "Y"; V "Y"]))];;
let sub2 = [("Y", V "Z")];;

(*
subst term1 sub1;
*)

(*
mgu term4 term5;;
mgu term4 term6;;
*)

(*
compose sub1 sub2;;
*)

(*
subst term3 (compose sub1 sub2);;
*)

(*
let sub = resolve_composition [sub1; sub2];;
subst term3 sub;;
*)
