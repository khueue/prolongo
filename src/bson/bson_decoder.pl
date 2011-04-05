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
    int32(_Length), % XXX Ignored for now. Validate how much?
    element_list(Elements),
    [0].

element_list([Name:Value|Elements]) -->
    element(Name, Value), !,
    element_list(Elements).
element_list([]) --> [].

element(Name, Value) -->
    [0x01], !,
    key(Name),
    value_double(Value).
element(Name, Value) -->
    [0x02], !,
    key(Name),
    value_string(Value).
element(Name, Value) -->
    [0x03], !,
    key(Name),
    value_document(Value).
element(Name, Value) -->
    [0x04], !,
    key(Name),
    value_array(Value).
element(Name, Value) -->
    [0x05], !,
    key(Name),
    value_binary(Value).
element(Name, Value) -->
    [0x06], !, % Deprecated in BSON 1.0.
    key(Name),
    value_undefined(Value).
element(Name, Value) -->
    [0x07], !,
    key(Name),
    value_object_id(Value).
element(Name, Value) -->
    [0x08], !,
    key(Name),
    value_boolean(Value).
element(Name, Value) -->
    [0x09], !,
    key(Name),
    value_utc(Value).
element(Name, Value) -->
    [0x0A], !,
    key(Name),
    value_null(Value).
element(Name, Value) -->
    [0x0B], !,
    key(Name),
    value_regex(Value).
element(Name, Value) -->
    [0x0C], !, % Deprecated in BSON 1.0.
    key(Name),
    value_db_pointer(Value).
element(Name, Value) -->
    [0x0D], !,
    key(Name),
    value_js(Value).
element(Name, Value) -->
    [0x0E], !,
    key(Name),
    value_symbol(Value).
element(Name, Value) -->
    [0x0F], !,
    key(Name),
    value_js_with_scope(Value).
element(Name, Value) -->
    [0x10], !,
    key(Name),
    value_int32(Value).
element(Name, Value) -->
    [0x11], !,
    key(Name),
    value_timestamp(Value).
element(Name, Value) -->
    [0x12], !,
    key(Name),
    value_int64(Value).
element(Name, Value) -->
    [0xFF], !,
    key(Name),
    value_min(Value).
element(Name, Value) -->
    [0x7F], !,
    key(Name),
    value_max(Value).

key(Name) -->
    c_string_atom(Name).

value_array(Doc) -->
    document(Doc).

value_document(Doc) -->
    document(Doc).

value_string(Text) -->
    string(Text).

value_regex(regex(Pattern,Options)) -->
    c_string_atom(Pattern),
    c_string_atom(Options).

value_db_pointer(db_pointer(Text,ObjectID)) -->
    string(Text),
    value_object_id_aux(IntegerObjectID, 0, 12),
    { number_hexatom(IntegerObjectID, ObjectID) }.

value_utc(utc(Timestamp)) -->
    int64(Timestamp).

value_timestamp(timestamp(Timestamp)) -->
    int64(Timestamp).

value_null(null) --> [].

value_min(min) --> [].

value_max(max) --> [].

value_binary(binary(Subtype,Bytes)) -->
    int32(Length),
    subtype(Subtype),
    n_bytes(Bytes, Length).

value_js(js(JsCode)) -->
    string(JsCode).

value_js_with_scope(js(JsCode,MappingsDoc)) -->
    int32(_LengthEntireJsWithScope), % XXX Unused for now.
    string(JsCode),
    document(MappingsDoc).

value_symbol(symbol(Atom)) -->
    string(Atom).

value_undefined(undefined) --> [].

value_object_id(object_id(ObjectID)) -->
    value_object_id_aux(IntegerObjectID, 0, 12),
    { number_hexatom(IntegerObjectID, ObjectID) }.

value_object_id_aux(Num, Num, 0) --> [], !.
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

string(Text) -->
    int32(Length),
    string_atom(Text, Length).

c_string_atom(Atom) -->
    bytes_stop_with_nul(Bytes),
    { bytes_to_utf8_atom(Bytes, Atom) }.

% Unused.
c_string_codes(Codes) -->
    bytes_stop_with_nul(Bytes),
    { bytes_to_utf8_codes(Bytes, Codes) }.

string_atom(Atom, Length) -->
    n_bytes_including_nul(Bytes, Length),
    { bytes_to_utf8_atom(Bytes, Atom) }.

% Unused.
string_codes(Codes, Length) -->
    n_bytes_including_nul(Bytes, Length),
    { bytes_to_utf8_codes(Bytes, Codes) }.

bytes_stop_with_nul([]) --> [0], !.
bytes_stop_with_nul([Byte|Bytes]) -->
    [Byte], % May NOT be nul (caught by base case).
    bytes_stop_with_nul(Bytes).

n_bytes_including_nul(Bytes, Length) -->
    { LengthMinusNul is Length - 1 },
    n_bytes(Bytes, LengthMinusNul),
    [0].

n_bytes([], 0) --> [], !.
n_bytes([Byte|Bytes], Length0) -->
    [Byte], % May be anything.
    { Length1 is Length0 - 1 },
    n_bytes(Bytes, Length1).

double(Double) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Double) }.

int32(Integer) -->
    [B0,B1,B2,B3],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, Integer) }.

int64(Integer) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer) }.

% XXX Is there something more appropriate than format/3?
number_hexatom(Number, Atom) :-
    HexInLowercase = '~16r',
    builtin:format(atom(Atom), HexInLowercase, [Number]).

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

% Unused.
bytes_to_utf8_codes(Bytes, Codes) :-
    builtin:atom_chars(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file:memory_file_to_codes(MemFile, Codes, utf8),
        memory_file:free_memory_file(MemFile)).
