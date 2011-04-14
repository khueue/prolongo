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
