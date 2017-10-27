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
    solve_mrec : program -> goal -> bool

    Return whether a goal can be proven from a program?

    This uses mutual recursion for backtracking
    (essentially using the Call-stack)
*)
let rec solve_mrec program goals =
    match goals with

    | [] -> true
    | g::rest ->

        (* Solve the first goal *)
        solve_one_mrec program g

        (* And solve the rest of the goals *)
        && solve_mrec program rest

and solve_one_mrec program goal =

    (* Define a new function so that original program value remains *)
    let rec resolve p g =

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
                resolve rest g

        | Rule (h, b) ->
            if g = h then
                (* See if the body of this rule can be solved *)
                solve_mrec program b

                (* Otherwise keep trying! *)
                || resolve rest g
            else
                resolve rest g
    )

    in resolve program goal
;;

(*
    solve_one : program -> goal -> bool * goals

    Return bodies of all rules that match
*)
let rec solve_one program goal new_goals =

    match program with

    | [] -> (new_goals <> [], new_goals)

    | c::rest ->
    (
        match c with
        | Fact h ->
            if goal = h then
                (true, [[]])
            else
                solve_one rest goal new_goals

        | Rule (h, b) ->
            if goal = h then
                solve_one rest goal (b::new_goals)
            else
                solve_one rest goal new_goals
    )


(*
    solve : joiner -> program -> clause -> bool

    Solve while explicitly maintaining a stack of current goals

    joiner function decides where to append newly found sub goals

        DFS : before the original goals (stack)
        BFS : after the original goals (queue)

*)
let rec solve joiner program goals =
    match goals with

    | [] -> true

    | g::rest ->

        (* Try to solve the first goal and find new goals *)
        let ok, subgoals_list = solve_one program g [] in

        if ok then

            let new_goals_list = List.map (fun x -> joiner x rest) subgoals_list in

            (* This is where the backtracking happens! *)
            List.fold_left (fun acc x -> acc || solve joiner program x) false new_goals_list
        else
            false

(*
    Test Cases
*)

let f1 = Fact (P "s");;
let f2 = Fact (P "t");;
let f3 = Fact (P "u");;
let f4 = Fact (P "q");;

let r1 = Rule (P "q", [P "s"; P "t"]);;
let r2 = Rule (P "q", [P "t";]);;
let r3 = Rule (P "q", [P "z"]);;

let p1 = [f1; f2; f3; r1];;
let p2 = [r3; r2; f2];;
let p3 = [r3; f4]

let g1 = [P "s"; P "q"];;
let g2 = [P "z"];;
let g3 = [P "q"];;

(* Run test cases on a solver *)
let run_test_cases solver =

    (* Emoty goal can always be proved *)
    assert (solver [] []);
    assert (solver p1 []);

    (* Can't prove a goal from an empty program *)
    assert (not (solver [] g1));

    (* s & q can be proven *)
    assert (solver p1 g1);

    (* No way to prove z *)
    assert (not (solver p1 g2));

    (* Order of rules shouldn't matter *)
    assert (solver p2 g3);

    (* Order of clauses shouldn't matter *)
    assert (solver p3 g3);
;;

(* Mutual Recursion is also a DFS *)
run_test_cases solve_mrec;;

(* DFS: Append newly found sub goals before the original goals *)
let solve_dfs = solve (fun naye purane -> naye @ purane) in
run_test_cases solve_dfs;;

(* BFS: Append newly found sub goals after the original goals *)
let solve_bfs = solve (fun naye purane -> purane @ naye) in
run_test_cases solve_bfs;;

