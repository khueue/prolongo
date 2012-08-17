:- module(mongo_util,
    [
        options_flags/3
    ]).

/** <module> Predicates for various helper routines.
 */

:- include(misc(common)).

/*
options_to_values(Pred, Options, Values) :-
    apply:maplist(Pred, Options, Values).

values_to_bitpattern([], 0).
values_to_bitpattern([V|Vs], Pattern) :-
    values_to_bitpattern(Vs, Pattern0),
    Pattern is Pattern0 \/ V.
*/

options_flags([], _Pred, 0).
options_flags([Option|Options], Pred, Value) :-
    options_flags(Options, Pred, Value0),
    call(Pred, Option, Value1),
    Value is Value0 \/ Value1,
    !.
options_flags([Option|_], _Pred, _Value) :-
    throw(mongo_error('unknown option', Option)).
