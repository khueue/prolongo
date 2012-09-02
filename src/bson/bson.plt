:- include(misc(common)).

:- begin_tests('bson:docs_bytes/2').

test('no docs', [true(Got == Expected)]) :-
    Expected = [],
    Bytes = [],
    bson:docs_bytes(Expected, Bytes),
    bson:docs_bytes(Got, Bytes).

test('complex doc back-and-forth', [true(Got == Expected)]) :-
    Doc =
    [
        k01 - -5.05,
        k02 - åäö_string, % Atoms only (no code lists).
        k03 - [],
        k04 - [k1-v1, k2-v2],
        k05 - [v1,v2,v3],
        k06 - binary(generic,[1,2,3]),
        k07 - binary(function,[1,2,3]),
        k08 - binary(old_generic,[1,2,3]),
        k09 - binary(uuid_old,[1,2,3]),
        k10 - binary(uuid,[1,2,3]),
        k11 - binary(md5,[1,2,3]),
        k12 - binary(user_defined,[1,2,3]),
        k13 - +undefined,
        k14 - object_id('47cc67093475061e3d95369d'),
        k15 - +false,
        k16 - +true,
        k17 - utc(1302354660284),
        k18 - +null,
        k19 - regex('pattern','options'),
        k20 - db_pointer('string','47cc67093475061e3d95369d'),
        k21 - js('code'),
        k22 - symbol(åäö_string), % Just like atoms.
        k23 - js('code',[mappings-doc]),
        k24 - 32,
        k25 - mongostamp(0),
        k26 - 9223372036854775807,
        k27 - +min,
        k28 - +max
    ],
    Expected =
    [
        Doc,
        Doc
    ],
    bson:docs_bytes(Expected, Bytes),
    bson:docs_bytes(Got, Bytes).

:- end_tests('bson:docs_bytes/2').

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

test('var, var', [throws(bson_error(_))]) :-
    bson:doc_bytes(_, _).

test('complex doc back-and-forth', [true(Got == Expected)]) :-
    Expected =
    [
        k01 - -5.05,
        k02 - åäö_string, % Atoms only (no code lists).
        k03 - [],
        k04 - [k1-v1, k2-v2],
        k05 - [v1,v2,v3],
        k06 - binary(generic,[1,2,3]),
        k07 - binary(function,[1,2,3]),
        k08 - binary(old_generic,[1,2,3]),
        k09 - binary(uuid_old,[1,2,3]),
        k10 - binary(uuid,[1,2,3]),
        k11 - binary(md5,[1,2,3]),
        k12 - binary(user_defined,[1,2,3]),
        k13 - +undefined,
        k14 - object_id('47cc67093475061e3d95369d'),
        k15 - +false,
        k16 - +true,
        k17 - utc(1302354660284),
        k18 - +null,
        k19 - regex('pattern','options'),
        k20 - db_pointer('string','47cc67093475061e3d95369d'),
        k21 - js('code'),
        k22 - symbol(åäö_string), % Just like atoms.
        k23 - js('code',[mappings-doc]),
        k24 - 32,
        k25 - mongostamp(0),
        k26 - 9223372036854775807,
        k27 - +min,
        k28 - +max
    ],
    bson:doc_bytes(Expected, Bytes),
    bson:doc_bytes(Got, Bytes).

:- end_tests('bson:doc_bytes/2').
