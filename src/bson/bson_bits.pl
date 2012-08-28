/** <module> Low-level bytes-to-number conversions.
 *
 *  Note: This module favors little-endian conversions, meaning
 *  that big-endian conversions are little-endian with a reverse.
 *  This should probably be fixed in future versions.
 */

:- module(bson_bits,
    [
        float_bytes/2,
        integer_bytes/4,
        hex_bytes/2,
        fits_in_32_bits/1,
        fits_in_64_bits/1
    ]).

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

%%  hex_bytes(+Hex, ?Bytes) is semidet.
%%  hex_bytes(?Hex, +Bytes) is semidet.
%
%   True if Hex is the hexadecimal atom (without leading '0x')
%   represented by the big-endian bytes in Bytes. Hex must have even
%   number of chars (each pair is a byte).

hex_bytes(Hex, Bytes) :-
    core:nonvar(Hex),
    !,
    core:atom_chars(Hex, HexChars),
    hexchars_to_bytes(HexChars, Bytes).
hex_bytes(Hex, Bytes) :-
    core:nonvar(Bytes),
    !,
    bytes_to_hexchars(Bytes, HexAtoms),
    core:atomic_list_concat(HexAtoms, Hex).

hexchars_to_bytes([], []).
hexchars_to_bytes([C1,C2|Chars], [Byte|Bytes]) :-
    core:atomic_list_concat(['0x',C1,C2], ByteAsHexAtom),
    core:atom_number(ByteAsHexAtom, Byte),
    hexchars_to_bytes(Chars, Bytes).

bytes_to_hexchars([], []).
bytes_to_hexchars([Byte|Bytes], [HexPadded|Chars]) :-
    number_to_hex(Byte, Hex),
    left_pad_with_zero(Byte, Hex, HexPadded),
    bytes_to_hexchars(Bytes, Chars).

number_to_hex(Number, HexAtom) :-
    core:format(atom(HexAtom), '~16r', [Number]).

left_pad_with_zero(Number, Hex, Hex) :-
    Number >= 0x10,
    !.
left_pad_with_zero(_Number, Hex, Padded) :-
    core:atom_concat('0', Hex, Padded).
