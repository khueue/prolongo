:- module(bson_decoder,
    [
        bson_to_pairs/2
    ]).

% <module> BSON decoder.

:- include(misc(common)).

:- use_module(bson_bits, []).
:- use_module(bson_unicode, []).

%%  bson_to_pairs(+Bson:list(byte), ?Term:list(pair)) is semidet.
%
%   True if Bson is the BSON byte-encoding of Term.
%
%   @throws bson_error(Reason)

bson_to_pairs(Bson, Term) :-
    phrase(document(Term), Bson),
    !.
bson_to_pairs(_Bson, _Term) :-
    throw(bson_error(invalid)).

document(Elements) -->
    int32(_Length), % XXX Ignored for now. Validate how much?
    elements(Elements),
    [0].

elements([]) --> [].
elements([Key-Value|Elements]) -->
    element(Key, Value),
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
    [0x06], !, % Deprecated.
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
    [0x0C], !, % Deprecated.
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
    c_string(Name).

value_array(Values) -->
    document(Document),
    { pairs_keys_values(Document, _Keys, Values) }.

value_document(Document) -->
    document(Document).

value_string(Text) -->
    string(Text).

value_regex(regex(Pattern,Options)) -->
    c_string(Pattern),
    c_string(Options).

value_db_pointer(db_pointer(Text,ObjectId)) -->
    string(Text),
    object_id(ObjectId).

value_utc(utc(Timestamp)) -->
    int64(Timestamp).

value_mongostamp(mongostamp(Mongostamp)) -->
    int64(Mongostamp).

value_null(@null) --> [].

value_min(@min) --> [].

value_max(@max) --> [].

value_undefined(@undefined) --> [].

value_boolean(@false) --> [0], !.
value_boolean(@true)  --> [1], !.
value_boolean(_)      --> { throw(bson_error(invalid_boolean)) }.

value_binary(binary(Subtype,Bytes)) -->
    int32(Length),
    subtype(Subtype),
    bytes_n(Bytes, Length).

value_js(js(JsCode)) -->
    string(JsCode).

value_js_with_scope(js(JsCode,MappingsDoc)) -->
    int32(_LengthEntireJsWithScope), % XXX Unused for now.
    string(JsCode),
    document(MappingsDoc).

value_symbol(symbol(Symbol)) -->
    string(Symbol).

value_object_id(object_id(ObjectId)) -->
    object_id(ObjectId).

value_double(Double) -->
    double(Double).

value_int32(Integer) -->
    int32(Integer).

value_int64(Integer) -->
    int64(Integer).

pairs_keys_values([], [], []).
pairs_keys_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
    pairs_keys_values(Pairs, Keys, Values).

object_id(AtomOrCodes) -->
    n_bytes_as_unsigned_integer(Integer, 12),
    { number_to_hex(Integer, AtomOrCodes) }.

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
    bytes_stop_on_nul(Bytes),
    { bson_unicode:utf8_bytes(AtomOrCodes, Bytes) }.

string(AtomOrCodes, Length) -->
    n_bytes_including_nul(Bytes, Length),
    { bson_unicode:utf8_bytes(AtomOrCodes, Bytes) }.

bytes_stop_on_nul([]) --> [0], !.
bytes_stop_on_nul([NotNul|Bytes]) -->
    [NotNul],
    bytes_stop_on_nul(Bytes).

n_bytes_including_nul(Bytes, Length) -->
    { LengthMinusNul is Length - 1 },
    bytes_n(Bytes, LengthMinusNul),
    [0].

bytes_n([], 0) --> [], !.
bytes_n([Byte|Bytes], Length0) -->
    [Byte], % May be anything.
    { Length1 is Length0 - 1 },
    bytes_n(Bytes, Length1).

n_bytes_as_unsigned_integer(Integer, Length) -->
    n_bytes_as_unsigned_integer(Integer, 0, Length).

n_bytes_as_unsigned_integer(Integer, Integer, 0) --> [], !.
n_bytes_as_unsigned_integer(Integer, Integer0, Length0) -->
    [Byte],
    { Integer1 is (Integer0 << 8) \/ Byte },
    { Length1 is Length0 - 1 },
    n_bytes_as_unsigned_integer(Integer, Integer1, Length1).

int32(Integer) -->
    int_size(Integer, 4).

int64(Integer) -->
    int_size(Integer, 8).

int_size(Integer, N) -->
    bytes_n(Bytes, N),
    { bson_bits:integer_bytes(Integer, N, little, Bytes) }.

double(Double) -->
    bytes_n(Bytes, 8),
    { bson_bits:float_bytes(Double, Bytes) }.

number_to_hex(Number, Atom) :-
    core:format(atom(Atom), '~16r', [Number]).
