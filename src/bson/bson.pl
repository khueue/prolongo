:- module(bson,
    [
        pairs_bson/2,
        assoc_bson/2,
        version/1,
        bson_version/1
    ]).

% <module> BSON encoder/decoder.
%
% Specification: http://bsonspec.org/

:- include(misc(common)).

:- use_module(bson_decoder, []).
:- use_module(bson_encoder, []).

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

%%  pairs_bson(+Pairs, ?Bson) is semidet.
%%  pairs_bson(?Pairs, +Bson) is semidet.
%
%   True if Bson is the BSON byte-encoding of Pairs.
%
%   Pairs is a list of key-value pairs.
%   Bson is a list of bytes (0..255).
%
%   @throws bson_error(Reason)

pairs_bson(Pairs, Bson) :-
    core:nonvar(Pairs),
    !,
    bson_encoder:pairs_to_bson(Pairs, Bson).
pairs_bson(Pairs, Bson) :-
    core:nonvar(Bson),
    !,
    bson_decoder:bson_to_pairs(Bson, Pairs).

%%  assoc_bson(+Assoc, ?Bson) is semidet.
%%  assoc_bson(?Assoc, +Bson) is semidet.
%
%   True if Bson is the BSON byte-encoding of Assoc.
%
%   Assoc is an association list according to library(assoc).
%   Bson is a list of bytes (0..255).
%
%   XXX Should be handled natively by encoder/decoder.
%
%   @throws bson_error(Reason)

assoc_bson(Assoc, Bson) :-
    core:nonvar(Assoc),
    !,
    assoc:assoc_to_list(Assoc, Pairs),
    bson_encoder:pairs_to_bson(Pairs, Bson).
assoc_bson(Assoc, Bson) :-
    core:nonvar(Bson),
    !,
    bson_decoder:bson_to_pairs(Bson, Pairs),
    assoc:list_to_assoc(Pairs, Assoc).
