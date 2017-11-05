
I

1. All submissions would be in .ml format. If you are providing additional files for documentation or any other purpose, then zip it along with the .ml file.

2. Name your .ml file and zip file with your entry no

3. Any submissions with syntactical errors will STRICTLY not be considered for evaluation.

II 

1. Provide definitions or the following types and base your program on these types defined:

```ocaml
type variable = string;;
type constant = string;;
type predicate = string;;

type term = V of variable | C of constant | Node of ???

type atomic_formula = ???

type clause = Fact of ??? | Rule of  ???

type goal = atomic_formula list;;

type program = clause list;;

```

Each of these types has a unique definition. You are required to understand what a type represents (understand theory properly), and provide a suitable definition. Correct definition is key to correct implementation of the solution.

2. Write function(s) to resolve as a Prolog interpreter would. (Consider cases of goals with constants and variables and cases of resolving for one solution or repeated resolving).

3. Understand the suitable use of unification and substitution and exploit your previous assignment submission on substitution to help you with this assignment.

---

In this assignment, you will write a simplified version of a Prolog interpreter in OCaml by combining the implementations of Assignment 4 (terms, substitutions and unification) and Assignment 5 (Resolution and Backtracking)

You will first (re)define an ML data type to represent the structure of a legitimate Prolog program.

* A program is a set (list) of clauses. 

* A clause can either be a fact or a rule. 
    - A fact has a head but no body.  
    - A rule has a head and a body.  
        + The head is a single atomic formula. 
        + A body is a sequence of atomic formulas.

* An atomic formula is a k-ary predicate symbol followed by k terms.

* A term is either a variable, a constant, or a k-ary function symbol with k subterms.

* A goal is a set (list) of atomic formulas.

---

You need to take your implementation of unification to use as the parameter-passing mechanism. (Note: by pretending the predicate symbol is a function symbol, you can perform resolution of goals and program clauses).

You also need to choose a back-tracking strategy to explore the resolution search space. 

You need to be able to replace a goal by subgoals, as found by applying a unifier to the body of a program clause whose head unified with the chosen subgoal.

Now provide examples of Prolog programs and queries, and test the execution of your interpreter.
