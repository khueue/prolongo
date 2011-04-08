:- module(bson_bits,
    [
        bytes_float/2,
        littlebytes_n_integer/3,
        fits_in_32_bits/1,
        fits_in_64_bits/1
    ]).

% <module> Low-level bytes-to-number conversions.
%
% Some predicates are implemented in C.

:- use_foreign_library(foreign(bson_bits)).

:- include(misc(common)).

%%  bytes_float
%
%   XXX

bytes_float(Bytes, Float) :-
    inbuilt:ground(Bytes),
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7],
    !,
    bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Float).
bytes_float(Bytes, Float) :-
    inbuilt:float(Float),
    !,
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7],
    float_to_bytes(Float, B0, B1, B2, B3, B4, B5, B6, B7).

%%  littlebytes_n_integer
%
%   XXX

littlebytes_n_integer(Bytes, 4, Integer) :-
    inbuilt:ground(Bytes),
    Bytes = [B0,B1,B2,B3],
    !,
    bytes_to_integer(B0, B1, B2, B3, Integer).
littlebytes_n_integer(Bytes, 8, Integer) :-
    inbuilt:ground(Bytes),
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7],
    !,
    bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer).
littlebytes_n_integer(Bytes, N, Integer) :-
    inbuilt:integer(Integer),
    inbuilt:integer(N),
    !,
    integer_to_n_bytes(Integer, N, Bytes).

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

%%  fits_in_32_bits(+Integer) is semidet.
%
%   True if Integer can be stored as a 32-bit signed int.

fits_in_32_bits(Int) :-
    -(2**(32-1)) =< Int, Int =< (2**(32-1))-1.

%%  fits_in_64_bits(+Integer) is semidet.
%
%   True if Integer can be stored as a 64-bit signed int.

fits_in_64_bits(Int) :-
    -(2**(64-1)) =< Int, Int =< (2**(64-1))-1.

%%  integer_to_n_bytes
%
%   XXX

integer_to_n_bytes(Int, N, Bytes) :-
    N > 0,
    integer_to_n_bytes(Int, 0, N, Bytes).

integer_to_n_bytes(_Int, N, N, []) :- !.
integer_to_n_bytes(Int, N0, N, [Byte|Bytes]) :-
    Byte is (Int >> (N0*8)) /\ 0xFF,
    N1 is N0 + 1,
    integer_to_n_bytes(Int, N1, N, Bytes).
