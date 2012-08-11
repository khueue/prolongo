:- include(misc(common)).

:- begin_tests('bson_unicode:utf8_bytes/2').

test('empty utf8', [true(Got == Expected)]) :-
    Utf8 = '',
    Expected = [],
    bson_unicode:utf8_bytes(Utf8, Got).

test('empty bytes', [true(Got == Expected)]) :-
    Bytes = [],
    Expected = '',
    bson_unicode:utf8_bytes(Got, Bytes).

test('both nonvar') :-
    bson_unicode:utf8_bytes('', []).

test('utf8 to bytes', [true(Got == Expected)]) :-
    Utf8 = 'ä\0ä',
    Expected = [0xc3,0xa4, 0, 0xc3,0xa4],
    bson_unicode:utf8_bytes(Utf8, Got).

test('bytes to utf8', [true(Got == Expected)]) :-
    Expected = 'ä\0ä',
    Bytes = [0xc3,0xa4, 0, 0xc3,0xa4],
    bson_unicode:utf8_bytes(Got, Bytes).

:- end_tests('bson_unicode:utf8_bytes/2').

:- begin_tests('bson_unicode:utf8_bytes_size/3').

test('utf8 to bytes, with size', [true(Got == Expected)]) :-
    Utf8 = 'ä\0ä',
    Expected = 5,
    bson_unicode:utf8_bytes_size(Utf8, _Bytes, Got).

:- end_tests('bson_unicode:utf8_bytes_size/3').
