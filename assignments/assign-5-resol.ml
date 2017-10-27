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
let rec solve_dfs program goals_stack  =

    match program with

    | [] ->
        Stack.length goals_stack = 0

    | cls::rest ->

        match cls with

        | Fact h -> true
        | Rule (h, b) -> false;;

        (* solve_dfs rest goals_stack;; *)

(*
    solve : program -> goal -> bool
*)
let solve program goals =

    (* Use a standard stack and add all starting goals to subgoals *)
    let goals_stack = Stack.create () in
        List.iter (fun p -> Stack.push p goals_stack) goals;

    solve_dfs program goals_stack;;

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

let r3 = Rule (P "r", [P "z"]);;

let p1 = [f1; f2; f3; r1; r2];;

let g1 = [P "s"; P "q"];;

let g2 = [P "z"];;

(* Should return true *)
solve p1 g1;;

(* Should return false *)
solve p1 g2;;

(* Should return true *)
solve [] [];;
solve p1 [];;

(* Should return false *)
solve [] g1;;
