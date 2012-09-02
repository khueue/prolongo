:- include(misc(common)).

:- begin_tests('bson_doc:doc_is_valid/1').

test('valid') :-
    Doc =
    [
        a - +null
    ],
    bson_doc:doc_is_valid(Doc).

test('invalid', [fail]) :-
    Doc =
    [
        a - +nul % Unknown constant.
    ],
    bson_doc:doc_is_valid(Doc).

:- end_tests('bson_doc:doc_is_valid/1').

:- begin_tests('bson_doc:doc_empty/1').

test('empty') :-
    bson_doc:doc_empty(Doc),
    bson_doc:doc_empty(Doc).

test('non-empty', [fail]) :-
    bson_doc:doc_empty(Doc),
    bson_doc:doc_put(Doc, key, value, Doc1),
    bson_doc:doc_empty(Doc1).

test('fill and empty') :-
    bson_doc:doc_empty(Doc),
    bson_doc:doc_put(Doc, key, value, Doc1),
    bson_doc:doc_delete(Doc1, key, Doc2),
    bson_doc:doc_empty(Doc2).

:- end_tests('bson_doc:doc_empty/1').

:- begin_tests('bson_doc:doc_get/3').

test('not found') :-
    Doc =
    [
        key - value
    ],
    bson_doc:doc_get(Doc, notfoundkey, +null).

test('not found in empty doc') :-
    Doc = [],
    bson_doc:doc_get(Doc, notfoundkey, +null).

test('found') :-
    Doc =
    [
        key - value
    ],
    bson_doc:doc_get(Doc, key, value).

test('found null') :-
    Doc =
    [
        key - +null
    ],
    bson_doc:doc_get(Doc, key, +null).

:- end_tests('bson_doc:doc_get/3').

:- begin_tests('bson_doc:doc_get_strict/3').

test('not found') :-
    Doc =
    [
        key - value
    ],
    \+ bson_doc:doc_get_strict(Doc, notfoundkey, _).

test('found') :-
    Doc =
    [
        key - value
    ],
    bson_doc:doc_get_strict(Doc, key, value).

test('found null') :-
    Doc =
    [
        key - +null
    ],
    bson_doc:doc_get_strict(Doc, key, +null).

:- end_tests('bson_doc:doc_get_strict/3').

:- begin_tests('bson_doc:doc_put/4').

test('put in empty') :-
    Doc = [],
    bson_doc:doc_get(Doc, key, +null),
    bson_doc:doc_put(Doc, key, value, Doc1),
    bson_doc:doc_get(Doc1, key, value).

test('put in non-empty') :-
    Doc =
    [
        keyold - valueold
    ],
    bson_doc:doc_get(Doc, keyold, valueold),
    bson_doc:doc_get(Doc, keynew, +null),
    bson_doc:doc_put(Doc, keynew, valuenew, Doc1),
    bson_doc:doc_get(Doc1, keyold, valueold),
    bson_doc:doc_get(Doc1, keynew, +null).

:- end_tests('bson_doc:doc_put/4').

:- begin_tests('bson_doc:doc_delete/3').

test('delete from empty') :-
    Doc = [],
    bson_doc:doc_delete(Doc, notfoundkey, Doc).

test('delete not found') :-
    Doc =
    [
        key - value
    ],
    bson_doc:doc_delete(Doc, notfoundkey, Doc).

test('delete only key') :-
    Doc =
    [
        key - value
    ],
    bson_doc:doc_delete(Doc, key, Doc1),
    bson_doc:doc_get(Doc1, key, +null).

test('delete one key') :-
    Doc =
    [
        key1 - value1,
        key2 - value2
    ],
    bson_doc:doc_delete(Doc, key2, Doc1),
    bson_doc:doc_get(Doc1, key1, value1),
    bson_doc:doc_get(Doc1, key2, +null).

:- end_tests('bson_doc:doc_delete/3').

:- begin_tests('bson_doc:doc_keys/2').

test('doc_keys 1', [true(Got == Expected)]) :-
    Doc =
    [
        a - 1,
        b - 2,
        c - 3
    ],
    Expected = [a,b,c],
    bson_doc:doc_keys(Doc, Got).

:- end_tests('bson_doc:doc_keys/2').

:- begin_tests('bson_doc:doc_values/2').

test('doc_values 1', [true(Got == Expected)]) :-
    Doc =
    [
        a - 1,
        b - 2,
        c - 3
    ],
    Expected = [1,2,3],
    bson_doc:doc_values(Doc, Got).

:- end_tests('bson_doc:doc_values/2').
