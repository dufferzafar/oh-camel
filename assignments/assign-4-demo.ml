(* let sub1 = R/P ;; *)
let sub1 = [("P", V "R")];;

(* let sub2 = g(Z,X)/Y ;; *)
let sub2 = [("Y", Node("g", [V "Z"; V "X"]))];;

(* let sub3 = 2/Y o Y/X ;; *)
let sub3 = [("Y", Node("2", [])); ("X", V "Y"); ("X", V "2")];;

(* let sub4 = *(Y,Y)/X o Z/Y ;; *)
let sub4 = [("X", Node("*", [V "Z"; V "Z"])); ("Y", V "Z")];;

(* let sub5 = M/X o N/Y o g(X,*(Y,*(X,Y)))/Z ;; *)
let sub5 = [("X", V "M"); ("Y", V "N"); ("Z", Node("g", [V "X"; Node("*", [V "Y"; Node("*", [V "X"; V "Y"])])])) ];;

(* let sub6 = g(X,*(Y,*(X,Y)))/Z o M/X o N/Y ;; *)
let sub6 = [("Z", Node("g", [V "M"; Node("*", [V "N"; Node("*", [V "M"; V "N"])])])); ("X", V "M"); ("Y", V "N") ];;

(* let sub7 = I/X ;; *)
let sub7 = [("X", V "I")] ;;

(* let sub8 = J/Y ;; *)
let sub8 = [("Y", V "J")] ;;

let mgu_to_list m = m;;
