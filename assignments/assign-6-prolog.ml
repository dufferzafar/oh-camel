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

type atomic_formula = predicate * (term list);;

(* H of ? *)
type head = atomic_formula;;
(* B of ? *)
type body = atomic_formula list;;

type clause =
    | Fact of head
    | Rule of head * body
;;

type goal = atomic_formula list;;
type program = clause list;;

(*
    Test Cases
*)

(* Facts *)

(* let t1 = ("wizard", [Node("harry", [])]) *)
let wz1 = Fact("wizard", [C "harry"]);;
let wz2 = Fact("wizard", [C "ron"]);;
let wz3 = Fact("wizard", [C "arthur"]);;
let wz4 = Fact("wizard", [C "bill"]);;
let wz5 = Fact("wizard", [C "james"]);;
let wz6 = Fact("wizard", [C "snape"]);;

let wt1 = Fact("witch", [C "hermione"]);;
let wt2 = Fact("witch", [C "ginny"]);;
let wt3 = Fact("witch", [C "molly"]);;
let wt4 = Fact("witch", [C "fleur"]);;
let wt5 = Fact("witch", [C "lilly"]);;

let mr1 = Fact("married", [C "harry"; C "ginny"]);;
let mr2 = Fact("married", [C "ron"; C "hermione"]);;
let mr3 = Fact("married", [C "arthur"; C "molly"]);;
let mr3 = Fact("married", [C "james"; C "lilly"]);;
let mr3 = Fact("married", [C "bill"; C "fleur"]);;

let lv1 = Fact("loves", [C "james"; C "lilly"]);;
let lv2 = Fact("loves", [C "snape"; C "lilly"]);;
let lv3 = Fact("loves", [C "harry"; C "ginny"]);;
let lv4 = Fact("loves", [C "ron"; C "hermione"]);;

let pt1 = Fact("parent", [C "arthur"; C "ron"]);;
let pt2 = Fact("parent", [C "arthur"; C "ginny"]);;
let pt3 = Fact("parent", [C "arthur"; C "bill"]);;
let pt4 = Fact("parent", [C "molly"; C "ron"]);;
let pt5 = Fact("parent", [C "molly"; C "ginny"]);;
let pt6 = Fact("parent", [C "molly"; C "bill"]);;
let pt7 = Fact("parent", [C "james"; C "harry"]);;
let pt8 = Fact("parent", [C "lilly"; C "harry"]);;

(* Rules: head :- body *)
let r1 = Rule(
    ("father", [V "F"; V "C"]),
    [
        ("wizard", [V "F"]);
        ("parent", [V "F"; V "C"])
    ]
);;

let r2 = Rule(
    ("mother", [V "M"; V "C"]),
    [
        ("witch", [V "M"]);
        ("parent", [V "M"; V "C"])
    ]
);;

let r3 = Rule(
    ("son", [V "S"; V "P"]),
    [
        ("wizard", [V "S"]);
        ("parent", [V "P"; V "S"])
    ]
);;

let r4 = Rule(
    ("daughter", [V "D"; V "P"]),
    [
        ("witch", [V "D"]);
        ("parent", [V "P"; V "D"])
    ]
);;

(* Another definition of loves *)
let r5 = Rule(
    ("loves", [V "X"; V "Y"]),
    [
        ("married", [V "X"; V "Y"])
    ]
)

(* Programs *)

let p1 = [
    (* facts *)
    wz1; wz2; wz3; wz4; wz5; wz6;
    wt1; wt2; wt3; wt4; wt5;
    mr1; mr2; mr3; mr3; mr3;
    lv1; lv2; lv3; lv4;
    pt1; pt2; pt3; pt4; pt5; pt6; pt7; pt8;

    (* rules *)
    r1; r2; r3; r4; r5
];;

(* Goals *)

(* True / False ? *)
let g0 = ("loves", [C "snape"; C "lilly"])
let g1 = ("loves", [C "bill"; C "fleur"])

(* Who is father of harry? *)
let g1 = ("father", [V "X", C "harry"]);;

(* Who loved lilly? *)
let g2 = ("loves", [V "Z", C "lilly"]);;
