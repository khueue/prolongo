:- module(_,
    [
        version/1,
        bson_version/1,
        term_bson/2
    ]).

% <module> BSON encoder/decoder.
%
% Specification: http://bsonspec.org/

:- use_module(bson_decoder, []).
:- use_module(bson_encoder, []).

:- include(misc(common)).

%%  version(?Version:list) is det.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([0,0,0]).

%%  bson_version(?Version:list) is det.
%
%   True if Version is a list representing the major and minor
%   version numbers of the implemented BSON format.

bson_version([1,0]).

%%  term_bson(+Term:list(pair), ?Bson:list(byte)) is semidet.
%%  term_bson(?Term:list(pair), +Bson:list(byte)) is semidet.
%
%   A pair is a structure atom:term. A byte is an integer in 0..255.
%
%   True if Bson is the BSON byte-encoding of Term.

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
