% BSON encoder.

:- module(_,
    [
        encode/2
    ]).

:- use_module(bson_bits, []).

:- include(misc(common)).

%%  encode(+Term, ?Bson:list) is semidet.
%
%   True if xxx.

:- begin_tests('bson_encoder:encode/2').

test('xxx', [true(Got == Expected)]) :-
    Expected = xxx,
    encode(xxx, Got).

:- end_tests('bson_encoder:encode/2').

% XXX Todo.
encode(Term, Bson) :-
    Term = Bson.
