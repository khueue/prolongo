:- include(misc(common)).

:- begin_tests('bson:doc_bytes/2').

test('nonvar, nonvar') :-
    Doc =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:doc_bytes(Doc, Bytes).

test('nonvar, var', [true(Got == Expected)]) :-
    Doc =
    [
        hello - 256
    ],
    Expected =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:doc_bytes(Doc, Got).

test('var, nonvar', [true(Got == Expected)]) :-
    Expected =
    [
        hello - 256
    ],
    Bytes =
    [
        16,0,0,0, % Length of top doc.
        0x10, % Tag.
            104,101,108,108,111, 0, % Ename.
            0,1,0,0, % Int32 data.
        0 % End of top doc.
    ],
    bson:doc_bytes(Got, Bytes).

test('var, var', [fail]) :-
    bson:doc_bytes(_, _).

test('complex doc back-and-forth', [true(Got == Expected)]) :-
    Expected =
    [
        k01 - -5.05,
        k02 - åäö_string, % Atoms only.
        k03 - [],
        k04 - [k1-v1, k2-v2],
        k05 - [v1,v2,v3],
        k06 - binary(generic,[1,2,3]),
        k07 - binary(function,[1,2,3]),
        k08 - binary(old_generic,[1,2,3]),
        k09 - binary(uuid,[1,2,3]),
        k10 - binary(md5,[1,2,3]),
        k11 - binary(user_defined,[1,2,3]),
        k12 - @(undefined),
        k13 - object_id('47cc67093475061e3d95369d'),
        k14 - @(false),
        k15 - @(true),
        k16 - utc(1302354660284),
        k17 - @(null),
        k18 - regex('pattern','options'),
        k19 - db_pointer('string','47cc67093475061e3d95369d'),
        k20 - js('code'),
        k21 - symbol(åäö_string), % Just like atoms.
        k22 - js('code',[mappings-doc]),
        k23 - 32,
        k24 - mongostamp(0),
        k25 - 9223372036854775807,
        k26 - @(min),
        k27 - @(max)
    ],
    bson:doc_bytes(Expected, Bytes),
    bson:doc_bytes(Got, Bytes).

:- end_tests('bson:doc_bytes/2').

:- begin_tests('bson:doc_get/3').

test('not found') :-
    Doc =
    [
        key - value
    ],
    bson:doc_get(Doc, notfoundkey, @(null)).

test('not found in empty doc') :-
    Doc = [],
    bson:doc_get(Doc, notfoundkey, @(null)).

test('found') :-
    Doc =
    [
        key - value
    ],
    bson:doc_get(Doc, key, value).

test('found null') :-
    Doc =
    [
        key - @(null)
    ],
    bson:doc_get(Doc, key, @(null)).

:- end_tests('bson:doc_get/3').

:- begin_tests('bson:doc_put/4').

test('put in empty') :-
    Doc = [],
    bson:doc_get(Doc, key, @(null)),
    bson:doc_put(Doc, key, value, Doc1),
    bson:doc_get(Doc1, key, value).

test('put in non-empty') :-
    Doc =
    [
        keyold - valueold
    ],
    bson:doc_get(Doc, keyold, valueold),
    bson:doc_get(Doc, keynew, @(null)),
    bson:doc_put(Doc, keynew, valuenew, Doc1),
    bson:doc_get(Doc1, keyold, valueold),
    bson:doc_get(Doc1, keynew, @(null)).

:- end_tests('bson:doc_put/4').

:- begin_tests('bson:doc_delete/3').

test('delete from empty') :-
    Doc = [],
    bson:doc_delete(Doc, notfoundkey, Doc).

test('delete not found') :-
    Doc =
    [
        key - value
    ],
    bson:doc_delete(Doc, notfoundkey, Doc).

test('delete only key') :-
    Doc =
    [
        key - value
    ],
    bson:doc_delete(Doc, key, Doc1),
    bson:doc_get(Doc1, key, @(null)).

test('delete one key') :-
    Doc =
    [
        key1 - value1,
        key2 - value2
    ],
    bson:doc_delete(Doc, key2, Doc1),
    bson:doc_get(Doc1, key1, value1),
    bson:doc_get(Doc1, key2, @(null)).

:- end_tests('bson:doc_delete/3').
