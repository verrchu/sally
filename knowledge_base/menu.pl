:- set_prolog_flag(verbose, silent).

:- initialization main.

:- use_module(recipe, [
    breakfast/1,
    characteristics_query/5
]).


main :-
    args(Cals, Prots, Fats, Carbs),
    recipe:breakfast(Breakfast),
    recipe:characteristics_query(
        Breakfast,
        BreakfastCals,
        BreakfastProts,
        BreakfastFats,
        BreakfastCarbs
    ),
    halt.


args(CalsNum, ProtsNum, FatsNum, CarbsNum) :-
    current_prolog_flag(argv, [
        _,Cals,Prots,Fats,Carbs
    ]),
    atom_number(Cals, CalsNum), positive(CalsNum),
    atom_number(Prots, ProtsNum), positive(ProtsNum),
    atom_number(Fats, FatsNum), positive(FatsNum),
    atom_number(Carbs, CarbsNum), positive(CarbsNum).

positive(X) :- X > 0.
