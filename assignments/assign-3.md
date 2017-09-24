
---

1. All submissions would be in .ml format. If you are providing additional files for documentation or any other purpose, then zip it along with the .ml file.

2. Name your .ml file and zip file with your entry no

3. Any submissions with syntactical errors will STRICTLY not be considered for evaluation.

---

<!-- 1. Note the syntactical correction in the prooftree type. -->

<!-- 2. Strictly follow the prooftree type provided after correction (given below the instructions). -->

<!-- 3. Only accepted type of "prop set" is "prop list" based on the type prop provided in Assignment2. Do not assume any other type. 
    `type sequent = prop list * prop`
-->

4. Comply to the following types for your function
<!--     
    3.1 ht : prooftree -> int = <fun>
    
    Define the function ht, which returns the height of a prooftree.

    3.2 size : prooftree -> int = <fun>
    
    Define the function size, which returns the number of nodes (rules used) in a prooftree
-->
    3.3 wfprooftree : prooftree -> bool = <fun>
    
    Define the function wfprooftree that check tat a given candidate proof tree is indeed a well-formed proof tree (i.e., the main formula is of the form expected by the rule, the side formulas are consistent with the main formula, and the extra formulas agree as specified in each rule).

    3.4 pad : prooftree -> prop list -> prooftree = <fun>
    
    Write a function pad, that given a well-formed proof tree and a set of additional assumptions, creates a new well-formed proof tree with the set of additional assumptions added at each node. (F1)

    3.5 pare : prooftree -> prooftree = <fun>
    
    Write a function pare that given a well-formed proof tree, returns a well-formed proof tree with minimal assumptions in each sequent. (F2)

    3.6 graft : prooftree -> prooftree list -> prooftree = <fun>
    
    Write a function graft that given a proof tree pi_0 of D |- p and a list of i proof trees pi_i for G |- q_i for each q_i in D, returns a proof tree for G |- p, where the trees for G |- q_i have replaced the leaves of the form D' |- q_i in a proof tree similar in shape to pi_0. (F3)

    3.7 normalise : prooftree -> prooftree = <fun>
    
    Write a program normalise which removes all occurrences of r-pairs in a given well-formed proof tree (i.e., where an introduction rule is followed only by an elimination rule of the main connective).

5. Only the definition of functions given in the Assignment description on moodle would be accepted. Any other interpretation would not be considered. Please read the requirement properly.

6. Stick to the function names given.

7. There is no requirement of any user defined types except for prop, sequent and prooftree. Do no create any other types.

Correction for the type prooftree.

```ocaml

type prooftree  = Ass of sequent | TI of sequent | FE of sequent
|  ImpI of prooftree * sequent | ImpE of prooftree * prooftree * sequent
| AndI of prooftree * prooftree * sequent | AndEleft of prooftree * sequent | AndEright of prooftree * sequent
| OrIleft of prooftree * sequent | OrIright of prooftree * sequent | OrE of prooftree * prooftree * prooftree * sequent
| NotClass of  prooftree * sequent | NotIntu of prooftree * sequent ;;

```
---

