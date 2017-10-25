let sig1 = [("X",0);("Y",0);("f",1);("g",2);("h",3);("*",2)];;
let sig2 = [("X",0);("Y",0);("Z",0);("f",1);("g",2);("f",3);("*",2)];;



let term1 = (Node ("f",[V "X"]));;
let term2 = (Node ("g",[V "X";Node("h",[Node("f",[V "X"]);V "Y"])]));;
let term3 = (Node ("g",[V "X";Node("*",[V "Y";Node ("*",[V "X";V "Y"])])]));;

let term4 = (Node("g",[V "X";Node("*",[V "Y";V "X"])]));;
let term5 = (Node("g",[V "Z";Node("*",[V "X";V "Z"])]));;
let term6 = (Node("g",[V "Z";Node("g",[V "X";V "Z"])]));;



let sub1 = <{Node("*",[V "Y",V "Y"])/V "X"} {V "Z"/V "Y"}>;;        ({V "Z"/V "Y"} is read as ' V "Z" for  V  "Y" ' which means you need to substitute V "Z" in place of V "Y". This is a standard

Note: for substituitons given as <s1 s2>, the composition is to be worked out in left to right order only ie (s1 o s2). Remember a composition of substitutions is a monoid.

So, for sub1:

say a = {Node("*",[V "Y",V "Y"])/V "X"} and b = {V "Z"/V "Y"} for easy readability say a' = *(y,y)/x  and b' = z/y

a' o b' = { *(y,y) b'/x , z/y }

          = { *(z,z)/x , z/y }              ie { Node("*",[V "Z";V "Z"])/V "X"  ,  V "Z" / V "Y" }



when we say -- #subst some_term sub1;;  -- we are refering to the composition.



--------------------------------------------------------------------------------------------------



# check_sig sig1;;
- : bool = true
# check_sig sig2;;
- : bool = false





# wfterm term1 sig1;;
- : bool = true
# wfterm term2 sig1;;
- : bool = false





# ht term1;;
- : int = 1
# ht term3;;
- : int = 3



# size term1;;
- : int = 2
# size term3;;
- : int = 7



# vars term1;;
- : variable list = ["X"]
# vars term3;;
- : variable list = ["X"; "Y"]





# subst term3 sub1;;
- : term = Node ("g",[Node ("*", [V "Z"; V "Z"]);Node ("*", [V "Z"; Node ("*", [Node ("*", [V "Z"; V "Z"]); V "Z"])])])





# mgu term4 term5;;
one of the possible solutions: <{V "Z"/V "X"} {V "X"/V "Y"}>
# mgu term4 term6;;
Exception: NOT_UNIFIABLE
