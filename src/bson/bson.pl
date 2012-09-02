/** <module> BSON manipulation.
 *
 *  BSON document manipulation and conversion to-and-from bytes.
 *
 *  @see <http://bsonspec.org/>
 */

:- module(_,
    [
        doc_bytes/2,
        docs_bytes/2,
        version/1,
        spec_version/1
        % And see reexports below.
    ]).

:- reexport(
    [
        bson_doc
    ]).

:- include(misc(common)).

% Internal modules.
:- use_module(bson_decoder, []).
:- use_module(bson_encoder, []).
:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).
% :- use_module(bson(bson_format), []). % Useful during dev.

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([0,0,1]).

%%  spec_version(?Version) is semidet.
%
%   True if Version is a list representing the major and minor
%   version numbers of the implemented BSON specification.

spec_version([1,0]).

%%  docs_bytes(+Docs, ?Bytes) is semidet.
%%  docs_bytes(?Docs, +Bytes) is semidet.
%
%   True if Bytes is the flat-list BSON byte-encoding of all the
%   documents in the list Docs.
%
%   @param Docs is a list of key-value pair lists.
%   @param Bytes is a list of bytes (in 0..255).
%
%   @throws bson_error(Reason)

docs_bytes(Docs, Bytes) :-
    core:nonvar(Bytes),
    !,
    bson_decoder:bytes_to_docs(Bytes, Docs).
docs_bytes(Docs, Bytes) :-
    core:nonvar(Docs),
    !,
    bson_encoder:docs_to_bytes(Docs, Bytes).
docs_bytes(_Docs, _Bytes) :-
    throw(bson_error('at least one arg must be instantiated')).

%%  doc_bytes(+Doc, ?Bytes) is semidet.
%%  doc_bytes(?Doc, +Bytes) is semidet.
%
%   True if Bytes is the BSON byte-encoding of Doc.
%
%   @param Doc is a list of key-value pairs.
%   @param Bytes is a list of bytes (in 0..255).
%
%   @throws bson_error(Reason)

doc_bytes(Doc, Bytes) :-
    core:nonvar(Bytes),
    !,
    bson_decoder:bytes_to_doc(Bytes, Doc).
doc_bytes(Doc, Bytes) :-
    core:nonvar(Doc),
    !,
    bson_encoder:doc_to_bytes(Doc, Bytes).
doc_bytes(_Doc, _Bytes) :-
    throw(bson_error('at least one arg must be instantiated')).
