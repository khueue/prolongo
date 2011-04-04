:- module(_,
    [
        decode/2
    ]).

% <module> BSON decoder.

:- use_module(bson_bits, []).

:- include(misc(common)).

%%  decode(+Bson:list, -Term) is semidet.
%
%   True if Term is the BSON document represented by the list
%   of bytes (0..255) in Bson.

decode(Bson, Term) :-
    phrase(decode(Term), Bson),
    !.
decode(_Bson, _Term) :-
    throw(bson_error(invalid)).

decode(Term) -->
    document(Term).

document(Elements) -->
    length(_Length), % XXX Ignored for now. Validate how much?
    element_list(Elements),
    end.

element_list([Name:Value|Elements]) -->
    element(Name, Value), !,
    element_list(Elements).
element_list([]) --> [].

element(Name, Value) -->
    [0x01], !,
    key_name(Name),
    value_double(Value).
element(Name, Value) -->
    [0x02], !,
    key_name(Name),
    value_string(Value).
element(Name, Value) -->
    [0x03], !,
    key_name(Name),
    value_document(Value).
element(Name, Value) -->
    [0x04], !,
    key_name(Name),
    value_document(Value).
element(Name, Value) -->
    [0x05], !,
    key_name(Name),
    value_binary(Value).
element(Name, Value) -->
    [0x06], !, % Deprecated in BSON 1.0.
    key_name(Name),
    value_undefined(Value).
element(Name, Value) -->
    [0x07], !,
    key_name(Name),
    value_object_id(Value).
element(Name, Value) -->
    [0x08], !,
    key_name(Name),
    value_boolean(Value).
element(Name, Value) -->
    [0x09], !,
    key_name(Name),
    value_utc(Value).
element(Name, Value) -->
    [0x0A], !,
    key_name(Name),
    value_null(Value).
element(Name, Value) -->
    [0x0B], !,
    key_name(Name),
    value_regex(Value).
element(Name, Value) -->
    [0x0C], !, % Deprecated in BSON 1.0.
    key_name(Name),
    value_db_pointer(Value).
element(Name, Value) -->
    [0x0D], !,
    key_name(Name),
    value_js(Value).
element(Name, Value) -->
    [0x0E], !,
    key_name(Name),
    value_symbol(Value).
element(Name, Value) -->
    [0x0F], !,
    key_name(Name),
    value_js_with_scope(Value).
element(Name, Value) -->
    [0x10], !,
    key_name(Name),
    value_int32(Value).
element(Name, Value) -->
    [0x11], !,
    key_name(Name),
    value_timestamp(Value).
element(Name, Value) -->
    [0x12], !,
    key_name(Name),
    value_int64(Value).
element(Name, Value) -->
    [0xFF], !,
    key_name(Name),
    value_min(Value).
element(Name, Value) -->
    [0x7F], !,
    key_name(Name),
    value_max(Value).

key_name(Ename) -->
    cstring(CharList),
    { bytes_to_utf8_atom(CharList, Ename) }.

value_document(Doc) -->
    document(Doc).

value_string(Atom) -->
    length(Length),
    utf8_bytes(ByteList, Length),
    { bytes_to_utf8_codes(ByteList, Atom) }.

value_regex(regex(Pattern,Options)) -->
    cstring(PatternBytes),
    { bytes_to_utf8_codes(PatternBytes, Pattern) },
    cstring(OptionsBytes),
    { bytes_to_utf8_codes(OptionsBytes, Options) }.

value_db_pointer(db_pointer(String,ObjectID)) -->
    value_string(String),
    value_object_id_aux(IntegerObjectID, 0, 12),
    { number_hexatom(IntegerObjectID, ObjectID) }.

value_utc(utc(Timestamp)) -->
    int64(Timestamp).

value_timestamp(timestamp(Timestamp)) -->
    int64(Timestamp).

value_null(nil) -->
    [].

value_min(min) -->
    [].

value_max(max) -->
    [].

value_binary(binary(Subtype,ByteList)) -->
    length(Length),
    subtype(Subtype),
    bytes(ByteList, Length).

value_js(js(JsCode)) -->
    value_string(JsCode).

value_js_with_scope(js(JsCode,MappingsDoc)) -->
    length(_LengthEntireJsWithScope), % XXX Unused for now.
    value_string(JsCode),
    value_document(MappingsDoc).

value_symbol(symbol(Atom)) -->
    value_string(Codes),
    { builtin:atom_codes(Atom, Codes) }.

value_undefined(undefined) -->
    [].

value_object_id(object_id(ObjectID)) -->
    value_object_id_aux(IntegerObjectID, 0, 12),
    { number_hexatom(IntegerObjectID, ObjectID) }.

value_object_id_aux(Num, Num, 0) -->
    [], !.
value_object_id_aux(Num, Num0, Length0) -->
    [Byte],
    { Num1 is (Num0 << 8) \/ Byte },
    { Length1 is Length0 - 1 },
    value_object_id_aux(Num, Num1, Length1).

value_boolean(false) --> [0], !.
value_boolean(true)  --> [1], !.
value_boolean(_)     --> { throw(bson_error(invalid_boolean)) }.

value_double(Double) -->
    double(Double).

value_int32(Integer) -->
    int32(Integer).

value_int64(Integer) -->
    int64(Integer).

subtype(generic)      --> [0x00], !.
subtype(function)     --> [0x01], !.
subtype(old_generic)  --> [0x02], !.
subtype(uuid)         --> [0x03], !.
subtype(md5)          --> [0x05], !.
subtype(user_defined) --> [0x80], !.

cstring([]) -->
    [0x00], !.
cstring([Char|Cs]) -->
    [Char], % May not be nul (caught by base case).
    cstring(Cs).

utf8_bytes(ByteList, Length) -->
    { LengthMinusNul is Length - 1 },
    bytes(ByteList, LengthMinusNul),
    [0x00].

bytes([], 0) -->
    [], !.
bytes([Byte|Bs], Length0) -->
    [Byte],
    { Length1 is Length0 - 1 },
    bytes(Bs, Length1).

length(Length) -->
    int32(Length).

double(Double) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Double) }.

int32(Integer) -->
    [B0,B1,B2,B3],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, Integer) }.

int64(Integer) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer) }.

end --> [0x00].

% XXX Is there something more appropriate than format/3?
number_hexatom(Number, Atom) :-
    builtin:format(atom(Atom), '~16r', [Number]).

% A bit of a hack, but in order to interpret raw bytes as UTF-8
% we use a memory file as a temporary buffer, fill it with the
% bytes and then read them back, treating them as UTF-8.
% See: http://www.swi-prolog.org/pldoc/doc_for?object=memory_file_to_atom/3

bytes_to_utf8_atom(Bytes, Atom) :-
    builtin:atom_chars(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file:memory_file_to_atom(MemFile, Atom, utf8),
        memory_file:free_memory_file(MemFile)).

bytes_to_utf8_codes(Bytes, Codes) :-
    builtin:atom_chars(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file:memory_file_to_codes(MemFile, Codes, utf8),
        memory_file:free_memory_file(MemFile)).
