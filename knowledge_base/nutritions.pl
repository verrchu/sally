:- use_module(library(record)).
:- use_module(library(clpfd)).

:- record nutr(cals:integer=0, prots:integer=0, fats:integer=0, carbs:integer=0).

default(N) :-
    default_nutr(N).

new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), N) :-
    make_nutr([cals(Cals), prots(Prots), fats(Fats), carbs(Carbs)], N).

cals(N, Val) :- nutr_cals(N, Val).
prots(N, Val) :- nutr_prots(N, Val).
fats(N, Val) :- nutr_fats(N, Val).
carbs(N, Val) :- nutr_carbs(N, Val).

combine(NS, N) :-
    nutritions:default(Acc), apply:foldl(nutritions:combine, NS, Acc, N).

combine(NA, NB, NC) :-
    cals(NA, ACals), prots(NA, AProts), fats(NA, AFats), carbs(NA, ACarbs), 
    cals(NB, BCals), prots(NB, BProts), fats(NB, BFats), carbs(NB, BCarbs),

    Cals #= ACals + BCals,
    Prots #= AProts + BProts,
    Fats #= AFats + BFats,
    Carbs #= ACarbs + BCarbs,

    new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), NC).
