%%% Implements BSON 1.0. <http://bsonspec.org/>

:- module(_,
    [
        term_bson/2
    ]).

:- use_module(bson_decoder, []).
:- use_module(bson_encoder, []).

:- include(misc(common)).

%%  term_bson(+Term:list, ?Bson:list(bytes)) is semidet.
%%  term_bson(?Term:list, +Bson:list(bytes)) is semidet.
%
%   True if XXX ...

term_bson(Term, Bson) :-
    nonvar(Term),
    nonvar(Bson),
    !,
    bson_decoder:decode(Bson, Term). % XXX Go with the fastest one.
term_bson(Term, Bson) :-
    nonvar(Bson),
    !,
    bson_decoder:decode(Bson, Term).
term_bson(Term, Bson) :-
    nonvar(Term),
    !,
    bson_encoder:encode(Term, Bson).
