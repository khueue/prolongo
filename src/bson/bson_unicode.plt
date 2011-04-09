:- include(misc(common)).

:- begin_tests('bson_unicode:utf8_bytes/2').

test('empty string', [true(Got == Expected)]) :-
    Utf8 = '',
    Expected = [],
    bson_unicode:utf8_bytes(Utf8, Got).

test('empty bytes', [true(Got == Expected)]) :-
    Bytes = [],
    Expected = atom(''),
    bson_unicode:utf8_bytes(Got, Bytes).

test('match') :-
    bson_unicode:utf8_bytes('', []).

test('naked atom to bytes', [true(Got == Expected)]) :-
    Utf8 = 'ä',
    Expected = [0xc3,0xa4],
    bson_unicode:utf8_bytes(Utf8, Got).

test('naked codes to bytes', [true(Got == Expected)]) :-
    Utf8 = "ä",
    Expected = [0xc3,0xa4],
    bson_unicode:utf8_bytes(Utf8, Got).

test('atom structure to bytes', [true(Got == Expected)]) :-
    Utf8 = atom('ä'),
    Expected = [0xc3,0xa4],
    bson_unicode:utf8_bytes(Utf8, Got).

test('codes structure to bytes', [true(Got == Expected)]) :-
    Utf8 = codes("ä"),
    Expected = [0xc3,0xa4],
    bson_unicode:utf8_bytes(Utf8, Got).

:- end_tests('bson_unicode:utf8_bytes/2').
