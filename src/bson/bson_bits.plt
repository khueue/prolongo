:- include(misc(common)).

:- begin_tests('bson_bits:float_bytes/2').

test('float, zero', [true(Got == Expected)]) :-
    Expected = 0.0,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

test('float, -5.05', [true(Got == Expected)]) :-
    Expected = -5.05,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

test('float, 5.05', [true(Got == Expected)]) :-
    Expected = 5.05,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

test('float, pi', [true(Got == Expected)]) :-
    Expected is pi,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

test('float, e', [true(Got == Expected)]) :-
    Expected is e,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

:- end_tests('bson_bits:float_bytes/2').

:- begin_tests('bson_bits:integer_bytes/4').

test('32-bit big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, min', [true(Got == Expected)]) :-
    Expected = -2147483648,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, min-1', [true(Got \== Expected)]) :-
    Expected = -2147483649, % Too low.
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, max', [true(Got == Expected)]) :-
    Expected = 2147483647,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, max+1', [true(Got \== Expected)]) :-
    Expected = 2147483648, % Too high.
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

% ------------------------------------

test('32-bit little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, min', [true(Got == Expected)]) :-
    Expected = -2147483648,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, min-1', [true(Got \== Expected)]) :-
    Expected = -2147483649, % Too low.
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, max', [true(Got == Expected)]) :-
    Expected = 2147483647,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, max+1', [true(Got \== Expected)]) :-
    Expected = 2147483648, % Too high.
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

% ------------------------------------

test('64-bit big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, min', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, min-1', [true(Got \== Expected)]) :-
    Expected = -9223372036854775809, % Too low.
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, max', [true(Got == Expected)]) :-
    Expected = 9223372036854775807,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, max+1', [true(Got \== Expected)]) :-
    Expected = 9223372036854775808, % Too high.
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

% ------------------------------------

test('64-bit little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, min', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, min-1', [true(Got \== Expected)]) :-
    Expected = -9223372036854775809, % Too low.
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, max', [true(Got == Expected)]) :-
    Expected = 9223372036854775807,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, max+1', [true(Got \== Expected)]) :-
    Expected = 9223372036854775808, % Too high.
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

:- end_tests('bson_bits:integer_bytes/4').

:- begin_tests('bson_bits:unsigned_bytes/4').

test('unbounded unsigned little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:unsigned_bytes(Expected, 42, little, Bytes),
    bson_bits:unsigned_bytes(Got,      42, little, Bytes).

test('unbounded unsigned little-endian, negative', [true(Got \== Expected)]) :-
    Expected = -1, % Not unsigned.
    bson_bits:unsigned_bytes(Expected, 42, little, Bytes),
    bson_bits:unsigned_bytes(Got,      42, little, Bytes).

test('unbounded unsigned little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:unsigned_bytes(Expected, 42, little, Bytes),
    bson_bits:unsigned_bytes(Got,      42, little, Bytes).

test('unbounded unsigned little-endian, huge', [true(Got == Expected)]) :-
    Expected = 92233720368547758080000000000000000,
    bson_bits:unsigned_bytes(Expected, 42, little, Bytes),
    bson_bits:unsigned_bytes(Got,      42, little, Bytes).

% ------------------------------------

test('unbounded unsigned big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:unsigned_bytes(Expected, 42, big, Bytes),
    bson_bits:unsigned_bytes(Got,      42, big, Bytes).

test('unbounded unsigned big-endian, negative', [true(Got \== Expected)]) :-
    Expected = -1, % Not unsigned.
    bson_bits:unsigned_bytes(Expected, 42, big, Bytes),
    bson_bits:unsigned_bytes(Got,      42, big, Bytes).

test('unbounded unsigned big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    bson_bits:unsigned_bytes(Expected, 42, big, Bytes),
    bson_bits:unsigned_bytes(Got,      42, big, Bytes).

test('unbounded unsigned big-endian, huge', [true(Got == Expected)]) :-
    Expected = 92233720368547758080000000000000000,
    bson_bits:unsigned_bytes(Expected, 42, big, Bytes),
    bson_bits:unsigned_bytes(Got,      42, big, Bytes).

:- end_tests('bson_bits:unsigned_bytes/4').
