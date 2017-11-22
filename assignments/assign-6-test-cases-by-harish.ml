
let head_cls c = match c with Fact h | Rule (h, _) -> h;;

(* constant terms *)
let burger = C "burger";;
let sandwich = C "sandwich";;
let pizza = C "pizza";;

(* Variable *)
let x = V "X";;

(* Atomic Formulas*)
let f1 = Node ("food", [burger]);;
let f2 = Node ("food", [sandwich]);;
let f3 = Node ("food", [pizza]);;
let f4 = Node ("lunch", [sandwich]);;
let f5 = Node ("dinner", [pizza]);;


(* Atomic Formulas with body *)
let r1 = (Node ("meal", [x]), [ Node("food", [x])] );;

(* Program *)
let prog = [Fact f1; Fact f2; Fact f3; Fact f4; Fact f5; Rule (Node ("meal", [x]), [ Node("food", [x])])];;

(* Queries *)

(* Is pizza a food *)
let g1 = [Node ("food",[pizza])];;
(* solver prog g1;;  *)

(* Which food is meal and lunch *)
let g2 = [Node ("meal",[x]); Node ("lunch",[x])];;
(* solver prog g2;; *)

let g3 = [Node ("dinner",[sandwich])];;
(* solver prog g3;; *)
