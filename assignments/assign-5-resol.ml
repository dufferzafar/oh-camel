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
    Main Code!
*)

(*
    solve : program -> goal -> bool
*)
let rec solve (program, goals) =
    match goals with

    | [] -> true
    | g::rest -> solve_one (program, g, program) && solve (program, rest);

and solve_one (cprog, g, program) =
    match cprog with

    (* Can't prove a goal from an empty program *)
    | [] -> false

    | c::rest ->
    (
        match c with
        | Fact h ->
            if g = h then
                true
            else
                solve_one (rest, g, program)

        | Rule (h, b) ->
            if g = h then
                solve (program, b)
            else
                solve_one (rest, g, program)
    )
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

let r3 = Rule (P "r", [P "z"]);;

let p1 = [f1; f2; f3; r1];;

let g1 = [P "s"; P "q"];;

let g2 = [P "z"];;

(* Should return true *)
solve (p1, g1);;

(* Should return false *)
solve (p1, g2);;

(* Should return true *)
solve ([], []);;
solve (p1, []);;

(* Should return false *)
solve ([], g1);;
