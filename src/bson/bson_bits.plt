:- include(misc(common)).

:- begin_tests('bson_bits:float_bytes/2').

test('float, bad input, result undefined', [true(Got \== Expected)]) :-
    Expected = bad(input),
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

test('float, not a float', [true(Got \== Expected)]) :-
    Expected = 0,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

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
    Expected is 3.14159,
    bson_bits:float_bytes(Expected, Bytes),
    bson_bits:float_bytes(Got,      Bytes).

:- end_tests('bson_bits:float_bytes/2').

:- begin_tests('bson_bits:integer_bytes/4').

test('32-bit, bad input, result undefined', [true(Got \== Expected)]) :-
    Expected = bad(input),
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

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

test('32-bit big-endian, verify endian', [true(Got == Expected)]) :-
    Expected = [0,0,0,1],
    bson_bits:integer_bytes(1, 4, big, Got).

test('32-bit big-endian, min', [true(Got == Expected)]) :-
    Expected is -2**31,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, min-1', [true(Got \== Expected)]) :-
    Expected is -2**31 - 1, % Too low.
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, max', [true(Got == Expected)]) :-
    Expected is 2**31 - 1,
    bson_bits:integer_bytes(Expected, 4, big, Bytes),
    bson_bits:integer_bytes(Got,      4, big, Bytes).

test('32-bit big-endian, max+1', [true(Got \== Expected)]) :-
    Expected is 2**31, % Too high.
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

test('32-bit little-endian, verify endian', [true(Got == Expected)]) :-
    Expected = [1,0,0,0],
    bson_bits:integer_bytes(1, 4, little, Got).

test('32-bit little-endian, min', [true(Got == Expected)]) :-
    Expected is -2**31,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, min-1', [true(Got \== Expected)]) :-
    Expected is -2**31 - 1, % Too low.
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, max', [true(Got == Expected)]) :-
    Expected is 2**31 - 1,
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

test('32-bit little-endian, max+1', [true(Got \== Expected)]) :-
    Expected is 2**31, % Too high.
    bson_bits:integer_bytes(Expected, 4, little, Bytes),
    bson_bits:integer_bytes(Got,      4, little, Bytes).

% ------------------------------------

test('64-bit, bad input, result undefined', [true(Got \== Expected)]) :-
    Expected = bad(input),
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

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

test('64-bit big-endian, verify endian', [true(Got == Expected)]) :-
    Expected = [0,0,0,0,0,0,0,1],
    bson_bits:integer_bytes(1, 8, big, Got).

test('64-bit big-endian, min', [true(Got == Expected)]) :-
    Expected is -2**63,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, min-1', [true(Got \== Expected)]) :-
    Expected is -2**63 - 1, % Too low.
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, max', [true(Got == Expected)]) :-
    Expected is 2**63 - 1,
    bson_bits:integer_bytes(Expected, 8, big, Bytes),
    bson_bits:integer_bytes(Got,      8, big, Bytes).

test('64-bit big-endian, max+1', [true(Got \== Expected)]) :-
    Expected is 2**63, % Too high.
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

test('64-bit little-endian, verify endian', [true(Got == Expected)]) :-
    Expected = [1,0,0,0,0,0,0,0],
    bson_bits:integer_bytes(1, 8, little, Got).

test('64-bit little-endian, min', [true(Got == Expected)]) :-
    Expected is -2**63,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, min-1', [true(Got \== Expected)]) :-
    Expected is -2**63 - 1, % Too low.
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, max', [true(Got == Expected)]) :-
    Expected is 2**63 - 1,
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

test('64-bit little-endian, max+1', [true(Got \== Expected)]) :-
    Expected is 2**63, % Too high.
    bson_bits:integer_bytes(Expected, 8, little, Bytes),
    bson_bits:integer_bytes(Got,      8, little, Bytes).

:- end_tests('bson_bits:integer_bytes/4').

:- begin_tests('bson_bits:hex_bytes/2').

test('hex to bytes, bad input should throw', [throws(_)]) :-
    Expected = bad(input),
    bson_bits:hex_bytes(Expected, _Bytes).

test('hex <-> bytes', [true(Got == Expected)]) :-
    Expected = '47cc67093475061e3d95369d',
    bson_bits:hex_bytes(Expected, Bytes),
    bson_bits:hex_bytes(Got,      Bytes).

test('hex to bytes, verify big endian', [true(Got == Expected)]) :-
    Hex = '47cc6709',
    Expected = [0x47,0xcc,0x67,0x09],
    bson_bits:hex_bytes(Hex, Got).

test('hex to bytes, odd length should fail', [fail]) :-
    HexOddLength = '47c',
    bson_bits:hex_bytes(HexOddLength, _Bytes).

:- end_tests('bson_bits:hex_bytes/2').
