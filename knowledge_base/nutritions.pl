:- module(nutritions, [
    default/1, new/5, combine/2,
    cals/2, prots/2, fats/2, carbs/2
]).

:- use_module(library(record)).

:- record nutritions(cals=0, prots=0, fats=0, carbs=0).

default(N) :-
    default_nutritions(N).

new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), N) :-
    make_nutritions([cals(Cals), prots(Prots), fats(Fats), carbs(Carbs)], N).

cals(N, Val) :- nutritions_cals(N, Val).
prots(N, Val) :- nutritions_prots(N, Val).
fats(N, Val) :- nutritions_fats(N, Val).
carbs(N, Val) :- nutritions_carbs(N, Val).

combine(NS, N) :-
    nutritions:default(Acc), apply:foldl(nutritions:combine, NS, Acc, N).

combine(NA, NB, NC) :-
    cals(NA, ACals), prots(NA, AProts), fats(NA, AFats), carbs(NA, ACarbs), 
    cals(NB, BCals), prots(NB, BProts), fats(NB, BFats), carbs(NB, BCarbs),

    Cals is ACals + BCals,
    Prots is AProts + BProts,
    Fats is AFats + BFats,
    Carbs is ACarbs + BCarbs,

    new(cals(Cals), prots(Prots), fats(Fats), carbs(Carbs), NC).
