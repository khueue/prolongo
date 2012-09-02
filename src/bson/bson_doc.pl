/** <module> BSON document manipulation.
 *
 *  Most of these predicates run in O(n), but that may change.
 */

:- module(_,
    [
        doc_is_valid/1,
        doc_empty/1,
        doc_get/3,
        doc_get_strict/3,
        doc_put/4,
        doc_delete/3,
        doc_keys/2,
        doc_values/2,
        doc_keys_values/3
    ]).

:- include(misc(common)).

%%  doc_is_valid(+Doc) is semidet.
%
%   True if Doc is a valid BSON document.
%
%   Note: Right now, this is accomplished by converting it to bytes
%   and failing if an exception is thrown. This can probably be done
%   more efficiently.

doc_is_valid(Doc) :-
    catch(
        bson:doc_bytes(Doc, _Bytes),
        bson_error(_),
        fail).

%%  doc_empty(?Doc) is semidet.
%
%   True if Doc is an empty BSON document.

doc_empty([]).

%%  doc_get(+Doc, +Key, ?Value) is semidet.
%
%   True if Value is the value associated with Key in Doc
%   or +null if Key cannot be found. This means that there
%   is no way of knowing if Value actually was +null or not found.

doc_get([], _, +null).
doc_get([K-V|_], K, V) :- !.
doc_get([_|Pairs], K, V) :-
    doc_get(Pairs, K, V).

%%  doc_get_strict(+Doc, +Key, ?Value) is semidet.
%
%   True if Value is the value associated with Key in Doc,
%   or fails if Key is not found or does not match Value.

doc_get_strict([K-V|_], K, V) :- !.
doc_get_strict([_|Pairs], K, V) :-
    doc_get_strict(Pairs, K, V).

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

%%  doc_keys(+Doc, ?Keys) is semidet.
%
%   True if Keys is the keys for the associations in Doc.

doc_keys(Doc, Keys) :-
    doc_keys_values(Doc, Keys, _Values).

%%  doc_values(+Doc, ?Values) is semidet.
%
%   True if Values is the values for the associations in Doc.

doc_values(Doc, Values) :-
    doc_keys_values(Doc, _Keys, Values).

%%  doc_keys_values(+Doc, ?Keys, ?Values) is semidet.
%%  doc_keys_values(?Doc, +Keys, +Values) is semidet.
%
%   True if Doc is the list of successive associations of
%   Keys and Values.

doc_keys_values([], [], []).
doc_keys_values([K-V|Pairs], [K|Keys], [V|Values]) :-
    doc_keys_values(Pairs, Keys, Values).
