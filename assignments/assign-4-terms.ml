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
    in List.fold_left (fun acc x -> acc + x) 0 count_list
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
    wfterm : term * signature -> bool

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

(* Find maximum from a list *)
let list_max l = List.fold_left (fun acc x -> max acc x) 0 l;;

(*
    ht : term -> int

    return height of a term tree (counting from 0)
*)
let rec ht trm =
    match trm with

    | V _ -> 0

    | Node (sym, trm_list) -> 1 + list_max (List.map (fun t -> ht t) trm_list)
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
