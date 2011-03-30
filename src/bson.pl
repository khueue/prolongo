%%% Implements BSON 1.0.

:- module(bson, []).
:- reexport(bson_decoder, [decode/2]).
:- reexport(bson_encoder, [encode/2]).

test_decode :-
    % \x16\x00\x00\x00\x02hello\x00\x06\x00\x00\x00world\x00\x00
    BsonHelloWorld =
    [
        0x16,0x00,0x00,0x00,0x02,
        104, 101, 108, 108, 111,
        0x00,0x06,0x00,0x00,0x00,
        119, 111, 114, 108, 100,
        0x00,0x00
    ],
    _BsonHello32 =
    [
        0xFF,0x00,0x00,0x00,
        0x10, 104, 101, 108, 108, 111, 0x00,
        0x20,0x00,0x00,0x00,
        0x00
    ],
    _BsonAwesome =
    [
        49,0,0,0, % length
        0x04, % array tag
            66,83,79,78,0, % element name, "BSON\0"
            38,0,0,0, % length of embedded doc (array)
            0x02, % string tag
                48,0, % index 0 ("0\0")
                8,0,0,0, % length of string, incl. nul
                97,119,101,115,111,109,101, 0, % string, "awesome\0"
            0x01, % double tag
                49,0, % ename, index 1 ("1\0")
                51,51,51,51,51,51,20,64, % double 8-byte
            0x10, % int32 tag
                50,0, % ename, index 2 ("2\0")
                194,7,0,0, % int32 data (1986)
            0, % end of array doc
        0 % end of doc
    ],
    Bson = BsonHelloWorld,
    decode(Bson, Term),
    io:format('BSON: ~w~n', [Bson]),
    io:format('Term: ~w~n', [Term]),
    bson_bits:bytes_to_float(51,51,51,51, 51,51,20,64, F),
    io:format('Float: ~w~n', [F]),
    bson_bits:bytes_to_integer(0,0,0,128, Int),
    io:format('Int32: ~w~n', [Int]),
    bson_bits:bytes_to_integer(0,0,0,0,0,0,0,128, Int64),
    io:format('Int64: ~w~n', [Int64]).
