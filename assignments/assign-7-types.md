
In this assignment, you will write a type-checker for a simple functional language based on the lambda-calculus for simply-typed Propositional Natural Deduction proofs.  You need to write a Prolog predicate hastype(Gamma, E, T), where 

Gamma is a list of variable-type pairs, representing type assumptions on variables

E is an object language expression, where E ranges over v(X) | \X.E | (E1 E2)  | <E1, E2> | proj^(2)_i E | inl(E) | inr(E) |  case E0 of inl(X) => E1 || inr(Y) => E2 

and 

T is a type ranging over TypeVar(A) | T1 -> T2 | T1 * T2 | T1 + T2

-


You need to provide enough test examples to show your type checker works correctly. 

--

Note that this checker can work as a type inference engine.  However it does not work for polymorphic type inference.  Show with counter-examples that this is the case.

--

For extra credit (50% extra) 

-- extend the pairing and projection operation for arbitrary tuples.  Hint: use lists and use check for projection operations being of the right kind.

-- generalise from just the two given constructors, to work with arbitrary sums that can have user-defined constructors and take tuples of arguments are arguments.

---

Please take care of the following 

I 
1. All submissions would be in .pl format. If you are providing additional files for documentation or any other purpose, then zip it along with the .pl file. 
2. Name your .pl file and zip file with your entry no 
3. Any submissions with syntactical errors will STRICTLY not be considered for evaluation. 


II 

1. All test cases would be based on predicate 'hastype(Gamma, E, T)', where Gamma, E and T are defined on Moodle. Make sure you exactly match the name and signature.

2. You can create any number of internal predicates.

3. Make sure you cover all cases of E and T defined on Moodle.

4. Teset case queries can be based on (i) only constants, (ii) only variables or (iii) combination of variables and constants.
