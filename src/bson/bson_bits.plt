:- include(misc(common)).

:- begin_tests('bson_bits:integer_bytes/4').

test('32-bit big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, min', [true(Got == Expected)]) :-
    Expected = -2147483648,
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, min-1', [true(Got =\= Expected)]) :-
    Expected = -2147483649, % Too low.
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, max', [true(Got == Expected)]) :-
    Expected = 2147483647,
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit big-endian, max+1', [true(Got =\= Expected)]) :-
    Expected = 2147483648, % Too high.
    NumBytes = 4,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

% --------------------------------------------------------------------------

test('32-bit little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, min', [true(Got == Expected)]) :-
    Expected = -2147483648,
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, min-1', [true(Got =\= Expected)]) :-
    Expected = -2147483649, % Too low.
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, max', [true(Got == Expected)]) :-
    Expected = 2147483647,
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('32-bit little-endian, max+1', [true(Got =\= Expected)]) :-
    Expected = 2147483648, % Too high.
    NumBytes = 4,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

% --------------------------------------------------------------------------

test('64-bit big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, min', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, min-1', [true(Got =\= Expected)]) :-
    Expected = -9223372036854775809, % Too low.
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, max', [true(Got == Expected)]) :-
    Expected = 9223372036854775807,
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit big-endian, max+1', [true(Got =\= Expected)]) :-
    Expected = 9223372036854775808, % Too high.
    NumBytes = 8,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

% --------------------------------------------------------------------------

test('64-bit little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, -1', [true(Got == Expected)]) :-
    Expected = -1,
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, min', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, min-1', [true(Got =\= Expected)]) :-
    Expected = -9223372036854775809, % Too low.
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, max', [true(Got == Expected)]) :-
    Expected = 9223372036854775807,
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('64-bit little-endian, max+1', [true(Got =\= Expected)]) :-
    Expected = 9223372036854775808, % Too high.
    NumBytes = 8,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

% --------------------------------------------------------------------------

test('unbounded unsigned little-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 42,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('unbounded unsigned little-endian, negative', [fail]) :-
    Expected = -1, % Negative is only allowed for 32/64-bit.
    NumBytes = 42,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(_Got,     NumBytes, Endian, Bytes).

test('unbounded unsigned little-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 42,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('unbounded unsigned little-endian, huge', [true(Got == Expected)]) :-
    Expected = 92233720368547758080000000000000000,
    NumBytes = 42,
    Endian   = little,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

% --------------------------------------------------------------------------

test('unbounded unsigned big-endian, zero', [true(Got == Expected)]) :-
    Expected = 0,
    NumBytes = 42,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('unbounded unsigned big-endian, negative', [fail]) :-
    Expected = -1, % Negative is only allowed for 32/64-bit.
    NumBytes = 42,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(_Got,     NumBytes, Endian, Bytes).

test('unbounded unsigned big-endian, 1', [true(Got == Expected)]) :-
    Expected = 1,
    NumBytes = 42,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

test('unbounded unsigned big-endian, huge', [true(Got == Expected)]) :-
    Expected = 92233720368547758080000000000000000,
    NumBytes = 42,
    Endian   = big,
    bson_bits:integer_bytes(Expected, NumBytes, Endian, Bytes),
    bson_bits:integer_bytes(Got,      NumBytes, Endian, Bytes).

:- end_tests('bson_bits:integer_bytes/4').
