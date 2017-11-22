
# oh camel!

Programs written in OCaml / Prolog for the IIT D Course COL 765 - "Introduction to Logic & Functional Programming" taught by Sanjiva Prasad.

## List of Assignments

1. Sets using Lists (scored 10/10)
2. Propositions and Conversion to CNF etc. (scored 9/10)
3. Natural Deduction Proof Trees (scored 9/10)
4. Terms (on a signature) and Substitutions (scored 7.5/10)
5. Propositional Resolution via Backtracking
6. Prolog Interpreter in OCaml
7. Type Checker in Prolog

## Beware of MOSS

MOSS is a tool that can detect similarity in code and it works on OCaml. So If you're copying code from here, do remember to make sufficient changes and be prepared to get a fail grade if you are caught.

You can also register for the MOSS service so you can check your similarity score before submitting the assignment. You should aim at keeping the score below 20.

## Other repositories

This [repo](https://github.com/saurabhs92/logic-and-functional-programming-iit-delhi/) by a '2015 student was quite helpful (especially in the 2nd assignment). 

There's not much prior code for this course on the web, but Sanjiva Sir also teaches a course on "Programming Languages" in which he once gave a Prolog Interpreter assignment so GitHub has a ton of other examples for this. [This is the one](https://github.com/swapnil96/Toy-Prolog-Interpreter) I took help from.

## Bugs

I did not get full marks in all assignments, so the code is not fully correct. Some of the issues I remember are: 

* In assignment 2:
    - Tautology / Contradiction functions were failing in some cases.

* In assignment 3:
    - `normalise` has some issues, so one of the trees from the TA's test cases was not getting normalised correctly.

* In assignment 4: 
    - Some edge case in `check_sig` 
    - Some edge case in `mgu`
    - Height of a constant (`Node("a", [])`) should've been 0 not 1.
    - Didn't know what was meant by "efficient substitution"

## Support

If you're having trouble understanding some part of the code, feel free to open an issue. 
