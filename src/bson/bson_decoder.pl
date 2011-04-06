:- module(bson_decoder,
    [
        bson_to_term/2
    ]).

% <module> BSON decoder.

:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).

:- include(misc(common)).

%%  bson_to_term(+Bson:list, -Term) is semidet.
%
%   True if Term is the BSON document represented by the list
%   of bytes (0..255) in Bson.

bson_to_term(Bson, Term) :-
    phrase(document(Term), Bson),
    !.
bson_to_term(_Bson, _Term) :-
    throw(bson_error(invalid)).

document(Elements) -->
    int32(_Length), % XXX Ignored for now. Validate how much?
    elements(Elements),
    end.

elements([]) --> [].
elements([Name:Value|Elements]) -->
    element(Name, Value),
    elements(Elements).

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
    value_mongostamp(Value).
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
    c_string(atom(Name)).

value_array(Doc) -->
    document(Doc).

value_document(Doc) -->
    document(Doc).

value_string(Text) -->
    string(atom(Text)).

value_regex(regex(Pattern,Options)) -->
    c_string(atom(Pattern)),
    c_string(atom(Options)).

value_db_pointer(db_pointer(Text,ObjectID)) -->
    string(atom(Text)),
    object_id(atom(ObjectID)).

value_utc(utc(Timestamp)) -->
    int64(Timestamp).

value_mongostamp(mongostamp(Mongostamp)) -->
    int64(Mongostamp).

value_null(null) --> [].

value_min(min) --> [].

value_max(max) --> [].

value_binary(binary(Subtype,Bytes)) -->
    int32(Length),
    subtype(Subtype),
    n_bytes(Bytes, Length).

value_js(js(JsCode)) -->
    string(atom(JsCode)).

value_js_with_scope(js(JsCode,MappingsDoc)) -->
    int32(_LengthEntireJsWithScope), % XXX Unused for now.
    string(atom(JsCode)),
    document(MappingsDoc).

value_symbol(symbol(Symbol)) -->
    string(atom(Symbol)).

value_undefined(undefined) --> [].

value_object_id(object_id(ObjectID)) -->
    object_id(atom(ObjectID)).

value_boolean(false) --> [0], !.
value_boolean(true)  --> [1], !.
value_boolean(_)     --> { throw(bson_error(invalid_boolean)) }.

value_double(Double) -->
    double(Double).

value_int32(Integer) -->
    int32(Integer).

value_int64(Integer) -->
    int64(Integer).

object_id(AtomOrCodes) -->
    n_bytes_as_unsigned_integer(Integer, 12),
    { number_to_hex(Integer, AtomOrCodes) }.

end --> [0].

subtype(generic)      --> [0x00], !.
subtype(function)     --> [0x01], !.
subtype(old_generic)  --> [0x02], !.
subtype(uuid)         --> [0x03], !.
subtype(md5)          --> [0x05], !.
subtype(user_defined) --> [0x80], !.

string(AtomOrCodes) -->
    int32(Length),
    string(AtomOrCodes, Length).

c_string(AtomOrCodes) -->
    bytes_stop_with_nul(Bytes),
    { bson_unicode:utf8_bytes(AtomOrCodes, Bytes) }.

string(AtomOrCodes, Length) -->
    n_bytes_including_nul(Bytes, Length),
    { bson_unicode:utf8_bytes(AtomOrCodes, Bytes) }.

bytes_stop_with_nul([]) --> [0], !.
bytes_stop_with_nul([NotNul|Bytes]) -->
    [NotNul],
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

n_bytes_as_unsigned_integer(Integer, Length) -->
    n_bytes_as_unsigned_integer(Integer, 0, Length).

n_bytes_as_unsigned_integer(Integer, Integer, 0) --> [], !.
n_bytes_as_unsigned_integer(Integer, Integer0, Length0) -->
    [Byte],
    { Integer1 is (Integer0 << 8) \/ Byte },
    { Length1 is Length0 - 1 },
    n_bytes_as_unsigned_integer(Integer, Integer1, Length1).

double(Double) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_float(B0, B1, B2, B3, B4, B5, B6, B7, Double) }.

int32(Integer) -->
    [B0,B1,B2,B3],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, Integer) }.

int64(Integer) -->
    [B0,B1,B2,B3,B4,B5,B6,B7],
    { bson_bits:bytes_to_integer(B0, B1, B2, B3, B4, B5, B6, B7, Integer) }.

number_to_hex(Number, AtomOrCodes) :-
    builtin:format(AtomOrCodes, '~16r', [Number]).
