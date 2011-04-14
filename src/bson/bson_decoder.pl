:- module(bson_decoder,
    [
        bson_to_pairs/2
    ]).

/** <module> BSON decoder.
 *
 *  This module is not meant to be used directly, but instead
 *  used through bson.
 */

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
    [Tag],
    key(Key),
    value(Tag, Value),
    elements(Elements).

value(0x01, Value) --> value_double(Value).
value(0x02, Value) --> value_string(Value).
value(0x03, Value) --> value_document(Value).
value(0x04, Value) --> value_array(Value).
value(0x05, Value) --> value_binary(Value).
value(0x06, Value) --> value_undefined(Value). % Deprecated.
value(0x07, Value) --> value_object_id(Value).
value(0x08, Value) --> value_boolean(Value).
value(0x09, Value) --> value_utc(Value).
value(0x0A, Value) --> value_null(Value).
value(0x0B, Value) --> value_regex(Value).
value(0x0C, Value) --> value_db_pointer(Value). % Deprecated.
value(0x0D, Value) --> value_js(Value).
value(0x0E, Value) --> value_symbol(Value).
value(0x0F, Value) --> value_js_with_scope(Value).
value(0x10, Value) --> value_int32(Value).
value(0x11, Value) --> value_mongostamp(Value).
value(0x12, Value) --> value_int64(Value).
value(0xFF, Value) --> value_min(Value).
value(0x7F, Value) --> value_max(Value).

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

object_id(ObjectId) -->
    bytes_n(Bytes, 12),
    { bson_bits:unsigned_bytes(Unsigned, 12, big, Bytes) },
    { number_to_hex(Unsigned, ObjectId) }.

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
