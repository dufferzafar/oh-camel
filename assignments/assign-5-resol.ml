(*
    Data Types
*)

type atom = P of string;;

type head = atom;;
type body = atom list;;

type clause = Fact of head | Rule of head * body;;

type program = clause list;;
type goal = atom list;;

(* For debugging *)
let s atom = match atom with P s -> s;;

(*
    =====================================================
    =====================================================
*)

(*
    Return whether a goal can be proven from a program?

    This uses mutual recursion for backtracking
    (essentially using the Call-stack)

    solve : program -> goal -> bool
*)
let rec solve_mrec (program, goals) =
    match goals with

    | [] -> true
    | g::rest ->

        (* Solve the first goal *)
        solve_one_mrec (program, g)

        (* And solve the rest of the goals *)
        && solve_mrec (program, rest)

and solve_one_mrec (program, goal) =

    (* Define a new function so that original program value remains *)
    let rec resolve (p, g) =

    match p with

    (* Can't prove a goal from an empty program *)
    | [] -> false

    | c::rest ->
    (
        match c with
        | Fact h ->

            (*
                Matching a goal with the head of a Fact / Rule
                is the resolution step
            *)
            if g = h then
                (* let () = Printf.printf "%s matched with %s \n" (s g) (s h) in *)
                true
            else
                resolve (rest, g)

        | Rule (h, b) ->
            if g = h then
                (* See if the body of this rule can be solved *)
                solve_mrec (program, b)

                (* Otherwise keep trying! *)
                || resolve (rest, g)
            else
                resolve (rest, g)
    )

    in resolve (program, goal)
;;

(*
    Get head of a clause

    get_head : clause -> head
*)
let get_head cls = match cls with Fact h | Rule (h, _) -> h

(*
    Find all clauses whose head matches the goal

    find_matches : program -> goal -> bool
*)
let find_matching program goal =
    List.filter (fun cls -> get_head cls = goal) program
;;

(*
    Test Cases
*)

let f1 = Fact (P "s");;
let f2 = Fact (P "t");;
let f3 = Fact (P "u");;

let r1 = Rule (P "q", [P "s"; P "t"]);;
let r2 = Rule (P "q", [P "t";]);;
let r3 = Rule (P "q", [P "z"]);;

let p1 = [f1; f2; f3; r1];;
let p2 = [r3; r2; f2];;

let g1 = [P "s"; P "q"];;
let g2 = [P "z"];;
let g3 = [P "q"];;

(* Run test cases on a solver *)
let run_test_cases solver =

    (* Should return true *)
    assert (solver ([], []));

    assert (solver (p1, []));

    (* Should return false *)
    assert (not (solver ([], g1)));

    (* Should return true *)
    assert (solver (p1, g1));

    (* Should return false *)
    assert (not (solver (p1, g2)));

    (* Order of rules does not matter! *)
    assert (solver (p2, g3));
;;

run_test_cases solve_mrec;;
