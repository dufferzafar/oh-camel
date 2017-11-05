%% Load it into prolog via prolog -s /path/to/file

%% The Harry Potter Lexicon

%% Facts

wizard(harry).
wizard(ron).
wizard(arthur).
wizard(bill).
wizard(james).
wizard(snape).

witch(hermione).
witch(ginny).
witch(molly).
witch(fleur).
witch(lilly).

married(harry, ginny).
married(ron, hermione).
married(arthur, molly).
married(james, lilly).
married(bill, fleur).

loves(james, lilly).
loves(snape, lilly).
loves(harry, ginny).
loves(ron, hermione).

%% Another definition of loves
loves(X, Y) :- married(X, Y).

parent(arthur, ron).
parent(arthur, ginny).
parent(arthur, bill).
parent(molly, ron).
parent(molly, ginny).
parent(molly, bill).
parent(james, harry).
parent(lilly, harry).

%% Rules

father(F,C) :- wizard(F), parent(F,C).

mother(M,C) :- witch(M), parent(M,C).

son(S,P) :- wizard(S), parent(P,S).

daughter(D,P) :- witch(D), parent(P,D).

%% Queries

%% true / false

%% loves(snape, lilly).
%% loves(bill, fleur).

%% Substitutions

%% father(X, harry).
%% loves(Z, lilly).
%% loves(Z, T).
