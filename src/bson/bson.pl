%%% Implements BSON 1.0. <http://bsonspec.org/>

:- module(_, []).

:- reexport(bson_decoder, [decode/2]).
:- reexport(bson_encoder, [encode/2]).

:- encoding(utf8).
