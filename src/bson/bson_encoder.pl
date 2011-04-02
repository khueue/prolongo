% BSON encoder.

:- module(_,
[
    encode/2
]).

:- use_module(bson_bits).

:- encoding(utf8).

encode(Term, Bson) :-
    Term = Bson.
