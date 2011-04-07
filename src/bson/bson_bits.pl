:- module(bson_bits,
    [
        bytes_to_float/9,
        bytes_to_integer/5,
        bytes_to_integer/9,
        float_to_bytes/9
    ]).

% <module> Low-level bytes-to-number conversions.
%
% Some predicates are implemented in C.

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

%%  bytes_to_integer(
%       +B0:byte, +B1:byte, +B2:byte, +B3:byte,
%       ?Integer:int) is det.
%
%   True if Integer is the integer represented by the consecutive
%   bytes (0..255) B0..B3 interpreted as a 32-bit little-endian integer.
%
%   Implemented in foreign library.

%%  bytes_to_integer(
%       +B0:byte, +B1:byte, +B2:byte, +B3:byte,
%       +B4:byte, +B5:byte, +B6:byte, +B7:byte,
%       ?Integer:int) is det.
%
%   True if Integer is the integer represented by the consecutive
%   bytes (0..255) B0..B7 interpreted as a 64-bit little-endian integer.
%
%   Implemented in foreign library.

%%  fits_in_32_bits
%
%   XXX

fits_in_32_bits(Int) :-
    -(2**(32-1)) =< Int, Int =< (2**(32-1))-1.

%%  fits_in_64_bits
%
%   XXX

fits_in_64_bits(Int) :-
    -(2**(64-1)) =< Int, Int =< (2**(64-1))-1.

%%  int32_to_bytes
%
%   XXX

% Todo: What happens when given a too large (unbounded) integer?
int32_to_bytes(Int32, B0, B1, B2, B3) :-
    B0 is (Int32 >> (0*8)) /\ 0xFF,
    B1 is (Int32 >> (1*8)) /\ 0xFF,
    B2 is (Int32 >> (2*8)) /\ 0xFF,
    B3 is (Int32 >> (3*8)) /\ 0xFF.

%%  int64_to_bytes
%
%   XXX

% Todo: What happens when given a too large (unbounded) integer?
int64_to_bytes(Int64, B0, B1, B2, B3, B4, B5, B6, B7) :-
    B0 is (Int64 >> (0*8)) /\ 0xFF,
    B1 is (Int64 >> (1*8)) /\ 0xFF,
    B2 is (Int64 >> (2*8)) /\ 0xFF,
    B3 is (Int64 >> (3*8)) /\ 0xFF,
    B4 is (Int64 >> (4*8)) /\ 0xFF,
    B5 is (Int64 >> (5*8)) /\ 0xFF,
    B6 is (Int64 >> (6*8)) /\ 0xFF,
    B7 is (Int64 >> (7*8)) /\ 0xFF.
