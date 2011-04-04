:- module(_,
    [
        encode/2
    ]).

% <module> BSON encoder.

:- use_module(bson_bits, []).

:- include(misc(common)).

%%  encode(+Term, ?Bson:list) is semidet.
%
%   True if xxx.

% XXX Todo.
encode(Term, Bson) :-
    Term = Bson.
