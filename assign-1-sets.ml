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
    ====================== Part A =======================
    =====================================================

    A set is represented as a list without any duplicates
*)

(*
    Examples of some sets used for testing
*)

let numbers = [0;1;2;3;4;5;6;7;8;9];;
let odds = [1;3;5;7;9];;
let evens = [0;2;4;6;8];;

let vowels = ['a';'e';'i';'o';'u'];;


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

(* Should print false *)
print_bool (member 3 evens);;

(* Should print true *)
print_bool (member 3 odds);;
print "\n";;


(*
    cardinality s - returns the number of elements in the set s

    Could simply delegate this:
    let cardinality s = List.length x s;
*)

let cardinality s =
    let rec length_left_recur s accu =
        match s with
            [] -> accu
        | h::t -> length_left_recur t 1+accu
    in
    length_left_recur s 0;;

print "Demo: cardinality";;

(* Should print 10 *)
print_int (cardinality numbers);;

(* Should print 0 *)
print_int (cardinality emptyset);;

print "\n";;

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
print_set_int (union odds evens);;
print "";;

(* Should print all numbers only once *)
print_set_int (union evens numbers);;
print "\n\n";;


(*
    intersection s1 s2 - returns the intersection of s1 and s2

    TODO: Proof
*)

let rec intersection s1 s2 =
    match s1 with
        [] -> []
    | h::t ->
        if member h s2 then
            h :: (intersection t s2)
        else
            intersection t s2
;;

print "Demo: intersection";;

(* Should only print evens *)
print_set_int (intersection evens numbers);;
print "";

(* Should print nothing - a blank line *)
print_set_int (intersection odds evens);;
print "";

(* Should print nothing - a blank line *)
print_set_int (intersection odds emptyset);;
print "";

(* Should print 2 *)
print_set_int (intersection [2;3] [2;4]);;
print "\n\n";;


(*
    difference s1 s2 - returns the set consisting of elements of s1
    which are not in s2

    TODO: Proof
*)

let rec difference s1 s2 =
    match s1 with
        [] -> []
    | h::t ->
        if not (member h s2) then
            h :: (difference t s2)
        else
            difference t s2
;;

print "Demo: difference";;

(* Should only print odds *)
print_set_int (difference numbers evens);;
print "";

(* Should print odds *)
print_set_int (difference odds emptyset);;
print "";

(* Should print odds *)
print_set_int (difference odds evens);;
print "";

(* Should print nothing - a blank line *)
print_set_int (difference odds odds);;
print "";

(* Should print 3 *)
print_set_int (difference [2;3] [2;4]);;
print "\n\n";;


(*
    subset s1 s2 - returns true if and only if s1 is a subset of s2

    TODO: Proof
*)

let rec subset s1 s2 =
    match s1 with
        [] -> true
    | h::t -> member h s2 && subset t s2
;;

print "Demo: subset";;

(* Should print true *)
print_bool (subset evens numbers);;

(* Should print false *)
print_bool (subset odds evens);;

(* Should print true *)
print_bool (subset odds odds);;
print "\n";;


(*
    equalset s1 s2 - returns true if and only if s1 is equal to s2

    Correctness:

        By definition, two sets s1 & s2 are equal if:
        s1 is a subset of s2 and s2 is a subset of s1
*)

let rec equalset s1 s2 = subset s1 s2 && subset s2 s1;;

print "Demo: equalset";;

(* Should print false *)
print_bool (equalset evens numbers);;

(* Should print false *)
print_bool (equalset odds evens);;

(* Should print true *)
print_bool (equalset evens evens);;

(* Should print true *)
print_bool (equalset [2;3;4] [4;2;3]);;

print "\n";;


(*
    product s1 s2 - returns the cartesian product of s1 and s2

    TODO: Proof
*)

let tuplify e x = (e, x);;
let element_product e s = List.map (tuplify e) s;;

let rec product s1 s2 =
    match s1 with
        [] -> []
    | h::t -> (element_product h s2) @ (product t s2)
 (* | h::t -> (List.map (fun x -> (h,x)) s2) @ (product t s2) *)
;;

print "Demo: product";;

(* Should print tuples of odds x vowels *)
print_set_tuple "(%d, %c)" (product odds vowels);;
print "\n";;

(* Should print tuples odds x evens *)
print_set_tuple "(%d, %d)" (product odds evens);;
print "\n\n";;


(*
    power s - returns the set of subsets of s

    TODO: Proof
*)

let listify e x = e :: x;;
let element_power e s = List.map (listify e) s;;

let rec power s =
    match s with
        [] -> [[]]
    | h::t ->
        let pwr = power t in
            pwr @ (element_power h pwr)
        (*  pwr @ (List.map (fun x -> h::x) pwr) *)
;;

print "Demo: power";;

(* Should print 1 *)
print_int (cardinality (power emptyset));;

(* Should print 32 *)
print_int (cardinality (power vowels));;

(* Should print 1024 *)
print_int (cardinality (power numbers));;

(* Should print true *)
print_bool ((cardinality (power odds)) = (cardinality (power odds)));;

print "\n";;

(*
    ===================================================
    ====================== Part B =====================
    ===================================================

    A set is represented by its characteristic function
*)

(*
    Examples of some sets used for testing
*)

(* let f_numbers x = List.elem x numbers *)
let f_numbers x =
    match x with
        | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> true
        | _ -> false
;;

(* let f_numbers x = List.elem x odds *)
let f_odds x =
    match x with
        | 1 | 3 | 5 | 7 | 9 -> true
        | _ -> false
;;

(* let f_numbers x = List.elem x evens *)
let f_evens x =
    match x with
        | 0 | 2 | 4 | 6 | 8 -> true
        | _ -> false
;;

(* let f_chars x = List.elem x vowels *)
let f_vowels x =
    match x with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false
;;


(*
    f_emptyset - returns the representation of an empty set
*)
let f_emptyset () = false;;


(*
    f_member x s - returns true if and only if x is in s
*)
let f_member x s = s x || false;;

print "Demo: f_member";;

(* Should print true *)
print_bool (f_member 3 f_odds);;

(* Should print false *)
print_bool (f_member 3 f_evens);;

print "\n";;


(*
    f_union s1 s2 - returns the union of sets s1 and s2

    Correctness:

        If s1, s2 are 'a -> bool functions then f_union returns
        another 'a -> bool function which is true if either of
        s1, s2 are true on some set element x - this is precisely
        how set union is defined.
*)

let f_union s1 s2 = fun x -> s1 x || s2 x;;

print "Demo: f_union";;

(* Should print true *)
print_bool (f_member 3 (f_union f_odds f_evens));;

(* Should print false *)
print_bool (f_member 33 (f_union f_odds f_evens));;

print "\n";;


(*
    f_intersection s1 s2 - returns the intersection of sets s1 and s2

    Correctness:

        If s1, s2 are 'a -> bool functions then f_intersection returns
        another 'a -> bool function which is true if both s1, s2 are
        true on some set element x - this is precisely how set intersection
        is defined.
*)

let f_intersection s1 s2 = fun x -> s1 x && s2 x;;

print "Demo: f_intersection";;

(* Should print true *)
print_bool (f_member 3 (f_intersection f_odds f_numbers));;

(* Should print false *)
print_bool (f_member 3 (f_intersection f_odds f_evens));;

print "\n";;


(*
    f_difference s1 s2 - returns the set consisting of elements of s1
    which are not in s2

    Correctness:

        If s1, s2 are 'a -> bool functions then f_difference returns
        another 'a -> bool function which is true when s1 is true
        but s2 is false on some set element x - this is precisely
        how set difference is defined.
*)

let f_difference s1 s2 = fun x -> s1 x && not (s2 x);;

print "Demo: f_difference";;

(* Should print true *)
print_bool (f_member 3 (f_difference f_odds f_evens));;

(* Should print false *)
print_bool (f_member 3 (f_difference f_odds f_numbers));;

print "\n";;


(*
    f_product s1 s2 - returns the cartesian product of s1 and s2

    Correctness:

        If s1: 'a -> bool and s2: 'b -> bool are characteristic
        functions of two sets then f_product returns a function
        ('a * 'b) -> bool which is true when s1 is true for some
        set element x and s2 is true for some set element y.

        Which is the definition of the cartesian product.
*)

let f_product s1 s2 = fun (x,y) -> s1 x && s2 y;;

print "Demo: f_product";;

(* Should print true *)
print_bool (f_member (1,2) (f_product f_odds f_evens));;

(* Should print true *)
print_bool (f_member (1,'a') (f_product f_odds f_vowels));;

(* Should print false *)
print_bool (f_member (2,'q') (f_product f_odds f_vowels));;

print "\n";;
