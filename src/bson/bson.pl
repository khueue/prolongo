:- module(bson,
    [
        doc_bytes/2,
        version/1,
        bson_version/1
    ]).

/** <module> BSON encoder/decoder.
 *
 *  Specification: http://bsonspec.org/
 */

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

%%  doc_bytes(+Doc, ?Bytes) is semidet.
%%  doc_bytes(?Doc, +Bytes) is semidet.
%
%   True if Bytes is the BSON byte-encoding of Doc.
%
%   Doc is a list of key-value pairs.
%   Bytes is a list of bytes (0..255).
%
%   @throws bson_error(Reason)

doc_bytes(Doc, Bytes) :-
    core:nonvar(Doc),
    !,
    bson_encoder:doc_to_bytes(Doc, Bytes).
doc_bytes(Doc, Bytes) :-
    core:nonvar(Bytes),
    !,
    bson_decoder:bytes_to_doc(Bytes, Doc).

%%  doc_get(+Doc, +Key, ?Value) is semidet.
%
%   True if Value is the value associated with Key in Doc,
%   or @(null) if the Key cannot be found. This means that there
%   is no way of knowing if Value actually was @(null) or not found.

doc_get([], _, @(null)).
doc_get([K-V|_], K, V) :- !.
doc_get([_|Pairs], K, V) :-
    doc_get(Pairs, K, V).

%%  doc_put(+Doc, +Key, +Value, ?NewDoc) is semidet.
%
%   True if NewDoc is Doc with the addition or update of the
%   association Key-Value.

doc_put([], K, V, [K-V]).
doc_put([K-_|Pairs], K, V, [K-V|Pairs]) :- !.
doc_put([Other|Pairs], K, V, [Other|Pairs1]) :-
    doc_put(Pairs, K, V, Pairs1).

%%  doc_delete(+Doc, +Key, ?NewDoc) is semidet.
%
%   True if NewDoc is Doc with the association removed that has
%   Key as key. At most one association is removed. No change if
%   Key is not found.

doc_delete([], _, []).
doc_delete([K-_|Pairs], K, Pairs) :- !.
doc_delete([Other|Pairs], K, [Other|Pairs1]) :-
    doc_delete(Pairs, K, Pairs1).
