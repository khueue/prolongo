% BSON encoder.

:- module(_,
    [
        encode/2
    ]).

:- use_module(bson_bits, []).

:- include(misc(common)).

encode(Term, Bson) :-
    Term = Bson.
