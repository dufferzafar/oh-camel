

In this assignment, you will write a simplified version of a baby Propositional Prolog interpreter in OCaml.

You will first define an ML data type to represent the structure of a legitimate Prolog program.

* A program is a set (list) of clauses. 
* A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body.  
* The head is a single atomic formula.  A body is a sequence of atomic formulas.
* An atomic formula is a propositional letter.
* A goal is a set (list) of atomic formulas.

You need to use resolution as the parameter-passing mechanism. 

You also need to develop a back-tracking strategy to explore the resolution search space. 

You need to be able to replace a goal by subgoals, as found by body of a program clause whose head resolved with the chosen subgoal.

Try different strategies:  Depth first exploration of subgoals (stack); then breadth first exploration of subgoals (queue). 
