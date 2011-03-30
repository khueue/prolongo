:- module(_, []).

:- use_module(bson_bits).

encode(Term, Bson) :-
    Term = Bson.
