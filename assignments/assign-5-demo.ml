let dfs = solve (fun naye purane -> naye @ purane);;
let bfs = solve (fun naye purane -> purane @ naye);;

let program1 = [Fact (P "a"); Fact (P "b")];;
let goal1 = [P "a"];;

let program2 = [Fact (P "x"); Fact (P "y"); Rule (P "z", [P "x"; P "y"])];;
let goal2 = [P "z"; P "x"];;

let program3 = [Fact (P "m"); Fact (P "n"); Rule (P "p", [P "m"]); Rule (P "q", [P "m"; P "n"])];;
let goal3 = [P "q"; P "p"];;

let program4 = [Rule (P "k", [P "l"]); Rule (P "l", [P "k"])];;
let goal4 = [P "l"];;

let test1 = dfs program1 goal1;;
let test2 = bfs program1 goal1;;

let test3 = dfs program2 goal2;;
let test4 = bfs program2 goal2;;

let test5 = dfs program3 goal3;;
let test6 = bfs program3 goal3;;

let test7 = dfs program4 goal4;;
let test8 = bfs program4 goal4;;
