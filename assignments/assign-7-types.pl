
%% Test whether an element is a member of a list
member(_, []) :- fail.
member(X, [X|Y]) :- !.
member(X, [Y|Z]) :- member(X, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Internal predicates:

%% For Expressions:

%% v - denotes a variable
%% pair - denotes a pair
%% projl, projr

%% For Types:

%% and
%% arrow

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Assumption
hastype(Gamma, v(X), T) :-
    member( (v(X), T), Gamma ).

%% And Introduction
%% Pair Construction
hastype(Gamma, pair(E1, E2), prod(T1, T2)) :-
    hastype(Gamma, E1, T1),
    hastype(Gamma, E2, T2).

%% And Elimination (Left)
%% Getting first part of a pair
hastype(Gamma, projl(E), T1) :-
    hastype(Gamma, E, prod(T1, _)).

%% And Elimination (Right)
%% Getting second part of a pair
hastype(Gamma, projr(E), T2) :-
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
hastype(Gamma, inl(E), sum(_, T2)) :-
    hastype(Gamma, E, T2).

%% Or Eliminiation
%% match with?
