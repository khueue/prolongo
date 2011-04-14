:- module(bson_bits,
    [
        float_bytes/2,
        integer_bytes/4,
        unsigned_bytes/4,
        fits_in_32_bits/1,
        fits_in_64_bits/1
    ]).

/** <module> Low-level bytes-to-number conversions.
 *
 *  Note: This module favors little-endian conversions in such a way
 *  that big-endian conversions are little-endian with a reverse.
 *  This should probably be fixed future versions.
 */

:- include(misc(common)).

:- use_foreign_library(foreign(bson_bits)).

%%  float_bytes(+Float, ?Bytes) is semidet.
%%  float_bytes(?Float, +Bytes) is semidet.
%
%   True if Float is the floating point number represented by
%   the consecutive bytes in Bytes interpreted as a 64-bit
%   IEEE 754 double.
%
%   Uses C library for actual conversions.

float_bytes(Float, Bytes) :-
    core:nonvar(Bytes),
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7],
    !,
    foreign_bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Float).
float_bytes(Float, Bytes) :-
    core:nonvar(Float),
    !,
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7],
    foreign_float_to_bytes(Float, B0, B1, B2, B3, B4, B5, B6, B7).

%%  integer_bytes(+Integer, +NumBytes, +Endian, ?Bytes) is semidet.
%%  integer_bytes(?Integer, +NumBytes, +Endian, +Bytes) is semidet.
%
%   True if Integer is the signed integer represented by the bytes
%   in Bytes, given NumBytes 4 or 8 and Endian little or big.
%   Results are undefined if Integer cannot fit in NumBytes bytes.
%
%   Uses C library for actual conversions.

integer_bytes(Integer, NumBytes, Endian, Bytes) :-
    core:nonvar(Integer),
    !,
    integer_to_bytes(Integer, NumBytes, Endian, Bytes).
integer_bytes(Integer, _NumBytes, Endian, Bytes) :-
    core:nonvar(Bytes),
    !,
    bytes_to_integer(Endian, Bytes, Integer).

integer_to_bytes(Integer, 4, little, [B0,B1,B2,B3]) :- !,
    foreign_integer_to_bytes(Integer, B0, B1, B2, B3).

integer_to_bytes(Integer, 8, little, [B0,B1,B2,B3,B4,B5,B6,B7]) :- !,
    foreign_integer_to_bytes(Integer, B0, B1, B2, B3, B4, B5, B6, B7).

integer_to_bytes(Integer, N, big, Bytes) :- !,
    integer_to_bytes(Integer, N, little, BytesLittle),
    lists:reverse(BytesLittle, Bytes).

bytes_to_integer(little, [B0,B1,B2,B3], Integer) :- !,
    foreign_bytes_to_integer(B0, B1, B2, B3, Integer).

bytes_to_integer(little, [B0,B1,B2,B3,B4,B5,B6,B7], Integer) :- !,
    foreign_bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer).

bytes_to_integer(big, Bytes, Integer) :- !,
    lists:reverse(Bytes, BytesLittle),
    bytes_to_integer(little, BytesLittle, Integer).

%%  unsigned_bytes(+Unsigned, +NumBytes, +Endian, ?Bytes) is semidet.
%%  unsigned_bytes(?Unsigned, +NumBytes, +Endian, +Bytes) is semidet.
%
%   True if Unsigned is the unsigned (possibly unbounded) integer
%   represented by the bytes in Bytes, given NumBytes > 0 and
%   Endian little or big.

unsigned_bytes(Unsigned, NumBytes, Endian, Bytes) :-
    core:nonvar(Unsigned),
    !,
    unsigned_to_bytes(Unsigned, NumBytes, Endian, Bytes).
unsigned_bytes(Unsigned, _NumBytes, Endian, Bytes) :-
    core:nonvar(Bytes),
    !,
    bytes_to_unsigned(Endian, Bytes, Unsigned).

unsigned_to_bytes(Unsigned, N, little, Bytes) :- !,
    unsigned_to_bytes_aux(Unsigned, 0, N, Bytes).

unsigned_to_bytes(Unsigned, N, big, Bytes) :- !,
    unsigned_to_bytes(Unsigned, N, little, BytesLittle),
    lists:reverse(BytesLittle, Bytes).

unsigned_to_bytes_aux(_Unsigned, N, N, []) :- !.
unsigned_to_bytes_aux(Unsigned, N0, N, [Byte|Bytes]) :-
    Byte is (Unsigned >> (N0*8)) /\ 0xFF,
    N1 is N0 + 1,
    unsigned_to_bytes_aux(Unsigned, N1, N, Bytes).

bytes_to_unsigned(little, Bytes, Unsigned) :- !,
    bytes_to_unsigned_aux(Bytes, 0, 0, Unsigned).

bytes_to_unsigned(big, Bytes, Unsigned) :- !,
    lists:reverse(Bytes, BytesLittle),
    bytes_to_unsigned(little, BytesLittle, Unsigned).

bytes_to_unsigned_aux([], _N, Unsigned, Unsigned).
bytes_to_unsigned_aux([Byte|Bytes], N, Unsigned0, Unsigned) :-
    Unsigned1 is (Byte << (N*8)) \/ Unsigned0,
    N1 is N + 1,
    bytes_to_unsigned_aux(Bytes, N1, Unsigned1, Unsigned).

%%  fits_in_32_bits(+Integer) is semidet.
%
%   True if Integer can be stored as a signed 32-bit int.

fits_in_32_bits(Integer) :-
    -(2**(32-1)) =< Integer, Integer =< (2**(32-1))-1.

%%  fits_in_64_bits(+Integer) is semidet.
%
%   True if Integer can be stored as a signed 64-bit int.

fits_in_64_bits(Integer) :-
    -(2**(64-1)) =< Integer, Integer =< (2**(64-1))-1.
