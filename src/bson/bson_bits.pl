:- module(bson_bits,
    [
        bytes_to_float/9,
        bytes_to_integer/5,
        bytes_to_integer/9
    ]).

% <module> Low-level bytes-to-number conversions.
%
% Implemented in C.

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
