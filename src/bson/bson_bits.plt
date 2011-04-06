:- include(misc(common)).

%%  bytes_to_float(
%       +B0:byte, +B1:byte, +B2:byte, +B3:byte,
%       +B4:byte, +B5:byte, +B6:byte, +B7:byte,
%       ?Float:float) is det.
%
%   True if Float is the floating point number represented by the
%   consecutive bytes (0..255) B0..B7 interpreted as a 64-bit
%   IEEE 754 double.
%
%   Implemented in foreign library.

:- begin_tests('bson_bits:bytes_to_float/9').

test('zero', [true(Got == Expected)]) :-
    Expected = 0.0,
    bson_bits:bytes_to_float(0,0,0,0, 0,0,0,0, Got).

test('neg', [true(Got == Expected)]) :-
    Expected = -5.05,
    bson_bits:bytes_to_float(51,51,51,51, 51,51,20,192, Got).

test('pos', [true(Got == Expected)]) :-
    Expected = 5.05,
    bson_bits:bytes_to_float(51,51,51,51, 51,51,20,64, Got).

:- end_tests('bson_bits:bytes_to_float/9').

:- begin_tests('bson_bits:bytes_to_integer/5').

test('zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:bytes_to_integer(0,0,0,0, Got).

test('int32 min', [true(Got == Expected)]) :-
    Expected = -2147483648,
    bson_bits:bytes_to_integer(0,0,0,0x80, Got).

test('int32 max', [true(Got == Expected)]) :-
    Expected = 2147483647,
    bson_bits:bytes_to_integer(0xFF,0xFF,0xFF,0x7F, Got).

:- end_tests('bson_bits:bytes_to_integer/5').

:- begin_tests('bson_bits:bytes_to_integer/9').

test('zero', [true(Got == Expected)]) :-
    Expected = 0,
    bson_bits:bytes_to_integer(0,0,0,0, 0,0,0,0, Got).

test('int64 min', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    bson_bits:bytes_to_integer(0,0,0,0, 0,0,0,0x80, Got).

test('int64 max', [true(Got == Expected)]) :-
    Expected = 9223372036854775807,
    bson_bits:bytes_to_integer(0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0x7F, Got).

:- end_tests('bson_bits:bytes_to_integer/9').

:- begin_tests('bson_bits:float_to_bytes/9').

test('zero', [true(Got == Expected)]) :-
    Expected = [0,0,0,0, 0,0,0,0],
    Got      = [B0,B1,B2,B3, B4,B5,B6,B7],
    bson_bits:float_to_bytes(0.0, B0,B1,B2,B3, B4,B5,B6,B7).

test('neg', [true(Got == Expected)]) :-
    Expected = [51,51,51,51, 51,51,20,192],
    Got      = [B0,B1,B2,B3, B4,B5,B6,B7],
    bson_bits:float_to_bytes(-5.05, B0,B1,B2,B3, B4,B5,B6,B7).

test('pos', [true(Got == Expected)]) :-
    Expected = [51,51,51,51, 51,51,20,64],
    Got      = [B0,B1,B2,B3, B4,B5,B6,B7],
    bson_bits:float_to_bytes(5.05, B0,B1,B2,B3, B4,B5,B6,B7).

:- end_tests('bson_bits:float_to_bytes/9').
