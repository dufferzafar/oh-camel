(*
    Data Types
*)

type variable = string;;
type predicate = string;;
type constant = string;;

type term =
(*
    Prolog uses implicit distinction between Variables (upper case first letter)
    and Constants (lower case first letter.)
*)
    | V    of variable
(*
    The other way of representing a constant is Node("", [])
*)
    | C    of constant
(*
    This will allow nested predicates, which aren't allowed in Prolog?
*)
    | Node of predicate * (term list)
;;

type atomic_formula = Node of predicate * (term list);;

type head = atomic_formula;;
type body = atomic_formula list;;

type clause =
    | Fact of head
    | Rule of head * body
;;

type goal = atomic_formula list;;
type program = clause list;;

(*
    =====================================================
    =====================================================
*)

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
    =====================================================
    =====================================================
*)

(* type substitution = (variable * term) list;; *)

(* Return a string representation of a term *)
let term_to_string t =
    match t with
    | C c -> c
    | V v -> v
    | _   -> ""

(* Print a substitution list: X = const *)
let print_subst s =
    let rec print_subs_helper s =
        match s with
        | [] -> ()
        | (v, t)::rest ->
            Printf.printf "\n%s = %s" v (term_to_string t);
            print_subs_helper rest
    in
    if s <> [] then
        print_subs_helper s;
        print_string " ";
;;

(* Print a goal *)
let print_goal g =
    match g with
    | Node (p, trm_lst) ->
        (*
            The term list will contain contain constants or variables
            so we join them with ,
        *)
        let joint =
            List.fold_left
                (fun acc x -> acc ^ ", " ^ term_to_string x)
                (term_to_string (List.hd trm_lst))
                (List.tl trm_lst)
        in
        Printf.printf "%s(%s)" p joint
;;

(*
    =====================================================
    =====================================================
*)

exception NOT_UNIFIABLE;;

(*
    occurs : variable -> term -> bool
    Check whether a variable occurs in a term.
*)
let rec occurs x t =
    match t with
      | V v -> v = x
      | C _ -> false
      | Node (_, trm_lst) -> List.exists (occurs x) trm_lst
;;

(*
    mgu : term -> term -> substitution

    returns the most general unifier of two terms (if it exists)
    otherwise raises an exception NOT_UNIFIABLE
*)
let rec mgu_of_terms p1 p2 =

    match (p1, p2) with

    | (C a, C b) ->
        if a = b then
            []
        else
            raise NOT_UNIFIABLE

    | (V v, C a) | (C a, V v) ->
        [(v, C a)]

    (* A constant can never be equal to a term *)
    | (C _, Node _) | (Node _, C _) ->
        raise NOT_UNIFIABLE

    (* Two different variables *)
    | (V v, V w) ->
        if v <> w then
            [(v, V w)]
        else
            []

    (* A variable and a node *)
    | (V v, (Node _ as t)) | ( Node _ as t, V v) ->
        if not (occurs v t) then
            [(v, t)]
        else
            raise NOT_UNIFIABLE

    (* Two different nodes *)
    | (Node (f, trm_lst_1), Node (g, trm_lst_2)) ->
        if (f = g) && (List.length trm_lst_1 = List.length trm_lst_2)
        then
            List.fold_left2 (fun acc c1 c2 -> union acc (mgu_of_terms c1 c2)) [] trm_lst_1 trm_lst_2
        else
            raise NOT_UNIFIABLE
;;

(*
    Term and Atomic Formulae have different types so we need this bit

    TODO: Any way to remove this duplication of code?
*)
let mgu_of_formula f1 f2 =
    match (f1, f2) with

    (Node (f, trm_lst_1), Node (g, trm_lst_2)) ->
        if (f = g) && (List.length trm_lst_1 = List.length trm_lst_2)
        then
            (* Debugging *)
            (* let _ = print_goal f1; print_string "\n"; in *)
            (* let _ = print_goal f2; print_string "\n"; in *)

            List.fold_left2 (fun acc c1 c2 -> union acc (mgu_of_terms c1 c2)) [] trm_lst_1 trm_lst_2
        else
            raise NOT_UNIFIABLE
;;

(*
    =====================================================
    =====================================================
*)

(*
    subst : term -> substitution -> term

    Apply a substitution on a term
*)
let rec apply_subst_to_term trm sub =
    match trm with

    | C c -> C c

    | V v  ->
    (
        try
            (* Replace this term with its substitution *)
            let _, t = List.find (fun (x, t) -> v = x ) sub in t
        with
            (* If not found just return the variable un-modified *)
            Not_found -> V v
    )

    | Node (pred, trm_list) ->
        Node (pred, List.map (fun t -> apply_subst_to_term t sub) trm_list)
;;

let apply_subst_to_formula f sub =
    match f with
    | Node (pred, trm_list) ->
        Node (pred, List.map (fun t -> apply_subst_to_term t sub) trm_list)
;;

(* Apply substitution to a body and return new body *)
let apply_subst_to_body body sub =
    List.fold_left (fun acc f -> union acc [(apply_subst_to_formula f sub)]) [] body
;;

(*
    =====================================================
    =====================================================
*)

(*
    solve : program -> goal -> bool

    Return whether a goal can be solved from a program

    This uses mutual recursion for backtracking
    (essentially using the Call-stack)
*)

(* Solve a list of goals *)
let rec solve program goals =
    match goals with

    | [] -> true
    | goal::rest ->

        (* Solve the first goal *)
        solve_one program goal

        (* And solve the rest of the goals *)
        && solve program rest

(* Solve one goal *)
and solve_one program goal =

    (* Debugging... *)
    let _ = print_string "\nSolving Goal: "; print_goal goal; print_string "\n"; in

    (* Define a new function so that original program value remains *)
    let rec resolve p g =

        match p with

        (* Can't prove a goal from an empty program *)
        | [] -> false

        | c::rest ->
        (
            match c with
            | Fact h ->
            (
                (* Try to find a substitution that makes the goal equal to head *)
                try
                    (* We found a substitution, print it and ask if we should continue *)
                    let t = mgu_of_formula g h in

                    (* Ask for choice only if the substitution was non empty *)
                    let choice =
                        if t <> [] then
                            let _ = print_subst t in
                            read_line()
                        else
                            "-"
                    in
                        (* If the user wants, then keep going *)
                        if choice = ";" then
                            resolve rest g || true
                        else if choice = "-" then
                            true
                        else (* choice = "." *)
                            let _ = print_string "\n" in
                            true
                with
                    (* Couldn't find a substitution, keep going *)
                    NOT_UNIFIABLE -> resolve rest g
            )

            | Rule (h, b) ->
            (
                try
                    let t = mgu_of_formula g h in

                        (* See if the body of this rule can be solved *)
                        solve program (apply_subst_to_body b t)

                        (* Otherwise keep trying! *)
                        || resolve rest g
                with
                    (* Couldn't find a substitution, keep going *)
                    NOT_UNIFIABLE -> resolve rest g
            )
        )

    in resolve program goal
;;

(*
    =====================================================
    =====================================================
*)

(*
    Test Cases
*)

(* Facts *)

(* let t1 = ("wizard", [Node("harry", [])]) *)
let wz1 = Fact( Node( "wizard", [C "harry"]) );;
let wz2 = Fact( Node( "wizard", [C "ron"]) );;
let wz3 = Fact( Node( "wizard", [C "arthur"]) );;
let wz4 = Fact( Node( "wizard", [C "bill"]) );;
let wz5 = Fact( Node( "wizard", [C "james"]) );;
let wz6 = Fact( Node( "wizard", [C "snape"]) );;

let wt1 = Fact( Node( "witch", [C "hermione"]) );;
let wt2 = Fact( Node( "witch", [C "ginny"]) );;
let wt3 = Fact( Node( "witch", [C "molly"]) );;
let wt4 = Fact( Node( "witch", [C "fleur"]) );;
let wt5 = Fact( Node( "witch", [C "lilly"]) );;

let mr1 = Fact( Node( "married", [C "harry"; C "ginny"]) );;
let mr2 = Fact( Node( "married", [C "ron"; C "hermione"]) );;
let mr3 = Fact( Node( "married", [C "arthur"; C "molly"]) );;
let mr4 = Fact( Node( "married", [C "james"; C "lilly"]) );;
let mr5 = Fact( Node( "married", [C "bill"; C "fleur"]) );;

let lv1 = Fact( Node( "loves", [C "james"; C "lilly"]) );;
let lv2 = Fact( Node( "loves", [C "snape"; C "lilly"]) );;
let lv3 = Fact( Node( "loves", [C "harry"; C "ginny"]) );;
let lv4 = Fact( Node( "loves", [C "ron"; C "hermione"]) );;

let pt1 = Fact( Node( "parent", [C "arthur"; C "ron"]) );;
let pt2 = Fact( Node( "parent", [C "arthur"; C "ginny"]) );;
let pt3 = Fact( Node( "parent", [C "arthur"; C "bill"]) );;
let pt4 = Fact( Node( "parent", [C "molly"; C "ron"]) );;
let pt5 = Fact( Node( "parent", [C "molly"; C "ginny"]) );;
let pt6 = Fact( Node( "parent", [C "molly"; C "bill"]) );;
let pt7 = Fact( Node( "parent", [C "james"; C "harry"]) );;
let pt8 = Fact( Node( "parent", [C "lilly"; C "harry"]) );;

(* Rules: head :- body *)
let r1 = Rule(
    Node("father", [V "F"; V "C"]),
    [
        Node("wizard", [V "F"]);
        Node("parent", [V "F"; V "C"])
    ]
);;

let r2 = Rule(
    Node("mother", [V "M"; V "C"]),
    [
        Node("witch", [V "M"]);
        Node("parent", [V "M"; V "C"])
    ]
);;

let r3 = Rule(
    Node("son", [V "S"; V "P"]),
    [
        Node("wizard", [V "S"]);
        Node("parent", [V "P"; V "S"])
    ]
);;

let r4 = Rule(
    Node("daughter", [V "D"; V "P"]),
    [
        Node("witch", [V "D"]);
        Node("parent", [V "P"; V "D"])
    ]
);;

(* Another definition of loves *)
let r5 = Rule(
    Node("loves", [V "X"; V "Y"]),
    [
        Node("married", [V "X"; V "Y"])
    ]
)

(* Programs *)

let p1 = [
    (* facts *)
    wz1; wz2; wz3; wz4; wz5; wz6;
    wt1; wt2; wt3; wt4; wt5;
    mr1; mr2; mr3; mr4; mr5;
    lv1; lv2; lv3; lv4;
    pt1; pt2; pt3; pt4; pt5; pt6; pt7; pt8;

    (* rules *)
    r1; r2; r3; r4; r5
];;

(* Goals *)

(* True / False ? *)

(* Comes directly from the facts *)
let g0 = Node("loves", [C "snape"; C "lilly"])

(* Comes from the rule involving marriage  *)
let g1 = Node("loves", [C "bill"; C "fleur"])

(* Who is father of harry? *)
let g1 = Node("father", [V "X"; C "harry"]);;

(* Who loved lilly? *)
let g2 = Node("loves", [V "Z"; C "lilly"]);;

(* Who loves each other? *)
let g3 = Node("loves", [V "Z"; V "T"]);;

(*
    The answer can be true/false OR a substitution list.
    But the list has to be evaluated lazily (if stdin == ";" etc.)

    Substitution
    MGU
    Solver
*)
