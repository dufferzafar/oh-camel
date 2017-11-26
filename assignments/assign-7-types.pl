
%% Test whether an element is a member of a list
member(_, []) :- fail.
member(X, [X|_]) :- !.
member(X, [_|Z]) :- member(X, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Internal predicates:

%% For Expressions:

%% v - denotes a variable
%% pair - denotes a pair
%% proj1, proj2

%% For Types:

%% and
%% arrow

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Assumption
hastype(Gamma, v(X), T) :-
    member((v(X), T), Gamma).

%% And Introduction
%% Pair Construction
hastype(Gamma, pair(E1, E2), prod(T1, T2)) :-
    hastype(Gamma, E1, T1),
    hastype(Gamma, E2, T2).

%% And Elimination (Left)
%% Getting first part of a pair
hastype(Gamma, proj1(E), T1) :-
    hastype(Gamma, E, prod(T1, _)).

%% And Elimination (Right)
%% Getting second part of a pair
hastype(Gamma, proj2(E), T2) :-
    hastype(Gamma, E, prod(_, T2)).

%% Implies Introduction
%% Function (Lambda) Abstraction
hastype(Gamma, abs(v(X), E), arrow(T1, T2)) :-
    hastype(Gamma, v(X), T1),
    hastype(Gamma,    E, T2).

%% Implies Introduction
%% Function Application
hastype(Gamma, app(E1, E2), T2) :-
    hastype(Gamma, E1, arrow(T1, T2)),
    hastype(Gamma, E2, T1).

%% Or Introduction (Left)
hastype(Gamma, inl(E), sum(T1, _)) :-
    hastype(Gamma, E, T1).

%% Or Introduction (Right)
hastype(Gamma, inr(E), sum(_, T2)) :-
    hastype(Gamma, E, T2).

%% Or Eliminiation
%% match with?
hastype(Gamma, case(E0, inl(X), inr(Y), E1, E2), T3) :-
    hastype(Gamma, X, T1),
    hastype(Gamma, Y, T2),
    hastype(Gamma, E0, sum(T1, T2)),
    hastype(Gamma, E1, T3),
    hastype(Gamma, E2, T3).


%% Function abstraction
query(hastype([(v(x), int)], abs(v(x), v(x)), _)).
query(hastype([(v(x), int), (v(y), char)], abs(v(x), v(y)), _)).

%% Function application
query(hastype([(v(x), int), (v(y), char)], app(abs(v(x), v(y)), v(x)), _)).
query(hastype([(v(x), int), (v(y), char)], app(abs(v(x), v(y)), v(y)), _)).

%% Tuples
query(hastype([(v(x), int), (v(y), str)], pair(v(x), v(y)), _)).
query(hastype(_, pair(v(x), v(y)), prod(int, str))).

writeln(T) :- write(T), nl.
main :-
    forall(query(Q), (Q -> writeln(true: Q) ; writeln(false: Q)))
    %% ,halt
    .
