type prop = | P of string | T | F | Not of prop | And of prop * prop | Or of prop * prop | Implies of prop * prop ;;
type sequent = prop list * prop;;
type prooftree = | Ass of sequent | TI of sequent | FE of sequent | ImpI of prooftree * sequent | ImpE of prooftree * prooftree * sequent | AndI of prooftree * prooftree * sequent | AndEleft of prooftree * sequent | AndEright of prooftree * sequent | OrIleft of prooftree * sequent | OrIright of prooftree * sequent | OrE of prooftree * prooftree * prooftree * sequent | NotClass of  prooftree * sequent | NotIntu of prooftree * sequent ;;

let gamma = [P "p2";P "p1";P "p3"];;
let gamma2 = [And (P "p",P "q");Or (P "p2",P "q2")];;

let test_tree1 = Ass (gamma ,P "p1") ;;
let test_tree2 = Ass (gamma ,P "p2") ;;
let test_tree3 = AndI (test_tree1,test_tree2,(gamma,(And (P "p1",P
"p2"))));;
let test_tree4 = AndEleft (test_tree3,(gamma,P "p2"));;

let test_fE    = FE (F::gamma,P "p1");;


let gamma2 = [And (P "p",P "q");Or (P "p2",P "q2")];;
let test_tree5 = Ass (gamma2,And (P "p",P "q")) ;;
let test_tree6 = ImpI (test_tree5,([Or (P "p2",P "q2")],Implies (And (P "p",P "q"),And (P "p",P "q"))));;

let gamma3 = [P "a"; P "b"];;
let tq1 = Ass (gamma3,P "a");;
let tq2 = Ass (gamma3,P "b");;
let q1 = AndI (tq1, tq2, (gamma3,(And (P "a",P "b"))));;
let q2 = OrIleft (tq2, (gamma3,(Or(P "b",P "c"))));;

let delta1 = [And (P "a",P "b")];;
let tp1 = Ass (delta1,(And (P "a",P "b")));;
let p = AndEleft (tp1,(delta1, (P "a")));;

let gamma3 = [P "a"; P "b"];;
let tq1 = Ass (gamma3,P "a");;
let tq2 = Ass (gamma3,P "b");;
let q1 = AndI (tq1, tq2, (gamma3,(And (P "a",P "b"))));;
let tree_nor = AndEleft (q1,(gamma,P "a"));;

# wfprooftree test_tree4;;
- : bool = false


# wfprooftree test_fE;;
- : bool = true


# pad test_tree4 [P "p3";P "p4"];;
- : prooftree =
AndEleft(AndI (Ass ([P "p3"; P "p4"; P "p2"; P "p1"], P "p1"),Ass ([P
"p3"; P "p4"; P "p2"; P "p1"], P "p2"), ([P "p3"; P "p4"; P "p2"; P
"p1"], And (P "p1", P "p2"))), ([P "p3"; P "p4"; P "p2"; P "p1"], P "p2"))


# pare test_tree6;;
- : prooftree = ImpI (Ass ([And (P "p", P "q")], And (P "p", P "q")), ([], Implies (And (P "p", P "q"), And (P "p", P "q"))))


# graft p [q1;q2];;
- : prooftree = AndEleft (AndI (Ass ([P "a"; P "b"], P "a"),Ass ([P "a";
P "b"], P "b"),([P "a"; P "b"], And (P "a", P "b"))),([P "a"; P "b"], P
"a"))


# normalize tree_nor;;
- : prooftree = Ass ([P "a"; P "b"], P "a")



(* TA's official test cases *)

let gamma2 = [And (P "p",P "q");Or (P "p2",P "q2")];;
let test_tree5 = Ass (gamma2,And (P "p",P "q")) ;;
let test_tree6 = ImpI (test_tree5,([Or (P "p2",P "q2")],Implies (And (P "p",P "q"),And (P "p",P "q"))));;

let gamma = [P "p2";P "p1";P "p3"];;
let gamma3 = [P "a"; P "b"];;
let tq1 = Ass (gamma3,P "a");;
let tq2 = Ass (gamma3,P "b");;
let q1 = AndI (tq1, tq2, (gamma3,(And (P "a",P "b"))));;
let tree_nor = AndEleft (q1,(gamma,P "a"));;

(* pare test_tree6;; *)

(* Harish's test cases *)

let p1 =  P"p";;
let p2 = Implies(P"p",P"s");;
let p3 = Implies(P"p",Implies(P"s",P"r"));;

let gamma = [p1; p2; p3; And(p1,p2); Or(p1,And(p2,p3))];;

let ass1 = Ass (gamma, p3);;
let ass2 = Ass (gamma, p1);;
let ass3 = Ass (gamma, p2);;

let imple1 = ImpE (ass1, ass2, (gamma, Implies(P"s",P"r")));;
let imple2 = ImpE (ass3, ass2, (gamma, P"s"));;

let imple3 = ImpE (imple1, imple2, (gamma,P"r"));;

let impli1 = ImpI (imple3,(gamma,Implies(P"p",P"r")));;

let impli2 = ImpI (impli1,(gamma,Implies(p2,Implies(P"p",P"r"))));;

let impli3 = ImpI (impli2, (gamma, Implies(p3, Implies(p2, Implies(P"p",P"r")))));;

(* pare impli3;; *)
