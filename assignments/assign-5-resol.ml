(*
    Data Types
*)

type atom = P of string;;

type head = atom;;
type body = atom list;;

type clause = Fact of head | Rule of head * body;;

type program = clause list;;
type goal = atom list;;

(*
    Others
*)

(* Solve using a stack for goals *)
let rec solve_dfs program goal_stack  =

    match program with

    | [] ->
        Stack.length goal_stack = 0

    | hd::tl ->
        solve_dfs tl goal_stack;;

(*
    solve : program * goal -> bool
*)
let solve program goal =

    (* Use a standard stack and add all starting goals to subgoals *)
    let subgoals = Stack.create () in
        List.iter (fun p -> Stack.push p subgoals) goal;

    solve_dfs program subgoals;;


(*
    Test Cases
*)

let c1 = Fact (P "S");;
let c2 = Fact (P "T");;
let c3 = Rule (P "Q", [P "S"; P "T"]);;

let p1 = [c1; c2; c3];;

let g1 = [P "S"; P "Q"];;

solve p1 g1;;
