/**
 * BSON encoder.
 */

:- module(_, [encode/2]).

:- use_module(bson_bits).

encode(Term, Bson) :-
    Term = Bson.
