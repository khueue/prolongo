% BSON encoder.

:- module(_,
    [
        encode/2
    ]).

:- use_module(bson_bits, []).

:- include(misc(common)).

:- begin_tests('bson_encoder:encode/2').

% Todo.

:- end_tests('bson_encoder:encode/2').

% Todo.
encode(Term, Bson) :-
    bson_bits:bytes_to_integer(0,0,0,0, _),
    Term = Bson.
