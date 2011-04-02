% Low-level bytes-to-number conversions.

:- module(_,
    [
        bytes_to_float/9,
        bytes_to_integer/5,
        bytes_to_integer/9
    ]).

:- use_foreign_library(foreign(bson_bits)).

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

test('bytes to float, neg', [true(Got == Expected)]) :-
    Expected = -5.05,
    bytes_to_float(51,51,51,51, 51,51,20,192, Got).

test('bytes to float, pos', [true(Got == Expected)]) :-
    Expected = 5.05,
    bytes_to_float(51,51,51,51, 51,51,20,64, Got).

:- end_tests('bson_bits:bytes_to_float/9').

%%  bytes_to_integer(
%       +B0:byte, +B1:byte, +B2:byte, +B3:byte,
%       ?Integer:int) is det.
%
%   True if Integer is the integer represented by the consecutive
%   bytes (0..255) B0..B3 interpreted as a 32-bit little-endian integer.
%
%   Implemented in foreign library.

:- begin_tests('bson_bits:bytes_to_integer/5').

test('bytes to int32, neg', [true(Got == Expected)]) :-
    Expected = -2147483648,
    bytes_to_integer(0,0,0,128, Got).

test('bytes to int32, pos', [true(Got == Expected)]) :-
    Expected = 2130706432,
    bytes_to_integer(0,0,0,127, Got).

:- end_tests('bson_bits:bytes_to_integer/5').

%%  bytes_to_integer(
%       +B0:byte, +B1:byte, +B2:byte, +B3:byte,
%       +B4:byte, +B5:byte, +B6:byte, +B7:byte,
%       ?Integer:int) is det.
%
%   True if Integer is the integer represented by the consecutive
%   bytes (0..255) B0..B7 interpreted as a 64-bit little-endian integer.
%
%   Implemented in foreign library.

:- begin_tests('bson_bits:bytes_to_integer/9').

test('bytes to int64, neg', [true(Got == Expected)]) :-
    Expected = -9223372036854775808,
    bytes_to_integer(0,0,0,0, 0,0,0,128, Got).

test('bytes to int64, pos', [true(Got == Expected)]) :-
    Expected = 9151314442816847872,
    bytes_to_integer(0,0,0,0, 0,0,0,127, Got).

:- end_tests('bson_bits:bytes_to_integer/9').
