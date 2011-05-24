:- module(mongo_bytes,
    [
        int32/3,
        int64/3,
        c_string/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).

% Can parse or construct.
int32(Int) -->
    [L0,L1,L2,L3],
    { bson_bits:integer_bytes(Int, 4, little, [L0,L1,L2,L3]) }.

% Can parse or construct.
int64(Int) -->
    [L0,L1,L2,L3,L4,L5,L6,L7],
    { bson_bits:integer_bytes(Int, 8, little, [L0,L1,L2,L3,L4,L5,L6,L7]) }.

c_string(Atom) -->
    { bson_unicode:utf8_bytes(Atom, Bytes) },
    Bytes,
    [0].
