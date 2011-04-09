:- module(bson_unicode,
    [
        utf8_bytes/2,
        utf8_bytes/3
    ]).

:- include(misc(common)).

%%  utf8_bytes(+Utf8,        +Bytes:list) is semidet.
%%  utf8_bytes(+Utf8,        ?Bytes:list) is semidet.
%%  utf8_bytes(?atom(Utf8),  +Bytes:list) is semidet.
%%  utf8_bytes(?codes(Utf8), +Bytes:list) is semidet.
%
%   XXX

utf8_bytes(Utf8, Bytes) :-
    inbuilt:nonvar(Utf8),
    seems_like_input_utf8(Utf8),
    !,
    utf8_to_bytes(Utf8, Bytes).
utf8_bytes(Utf8, Bytes) :-
    inbuilt:nonvar(Bytes),
    seems_like_input_bytes(Bytes),
    !,
    bytes_to_utf8(Bytes, Utf8).

seems_like_input_utf8(atom(Atom))   :- inbuilt:atom(Atom).
seems_like_input_utf8(codes(Codes)) :- seems_like_list(Codes).
seems_like_input_utf8(Atom)         :- inbuilt:atom(Atom).
seems_like_input_utf8(Codes)        :- seems_like_list(Codes).

seems_like_input_bytes(Bytes) :-
    seems_like_list(Bytes).

seems_like_list([]).
seems_like_list([_|_]).

%%  utf8_bytes
%
%   XXX

utf8_bytes(Utf8, Bytes, NumBytes) :-
    utf8_bytes(Utf8, Bytes),
    lists:length(Bytes, NumBytes).

%%  utf8_to_bytes
%
%   XXX

utf8_to_bytes(atom(Atom), Bytes) :-
    !,
    utf8_atom_to_bytes(Atom, Bytes).
utf8_to_bytes(codes(Codes), Bytes) :-
    !,
    utf8_codes_to_bytes(Codes, Bytes).
utf8_to_bytes(Atom, Bytes) :-
    inbuilt:atom(Atom),
    !,
    utf8_atom_to_bytes(Atom, Bytes).
utf8_to_bytes(Codes, Bytes) :-
    utf8_codes_to_bytes(Codes, Bytes).

utf8_atom_to_bytes(Atom, Bytes) :-
    inbuilt:atom_codes(Atom, Codes),
    utf8_codes_to_bytes(Codes, Bytes).

utf8_codes_to_bytes(Codes, Bytes) :-
    setup_call_cleanup(
        charsio:open_chars_stream(Codes, ReadStream),
        stream_to_bytes(ReadStream, Bytes),
        inbuilt:close(ReadStream)).

stream_to_bytes(ReadStream, Bytes) :-
    inbuilt:set_stream(ReadStream, encoding(octet)),
    readutil:read_stream_to_codes(ReadStream, Bytes).

%%  bytes_to_utf8(+Bytes, ?AtomOrCodes) is semidet.
%
%   True if AtomOrCodes is the structure atom(Atom) where Atom is the
%   UTF-8 atom represented by Bytes, or if AtomOrCodes is the structure
%   codes(Codes) where Codes is the list of UTF-8 code points
%   represented by Bytes.
%
%   A bit of a hack, but in order to interpret raw bytes as UTF-8
%   we use a memory file as a temporary buffer, fill it with the
%   bytes and then read them back, treating them as UTF-8.

bytes_to_utf8(Bytes, AtomOrCodes) :-
    inbuilt:atom_chars(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file_to_atom_or_codes(MemFile, AtomOrCodes, utf8),
        memory_file:free_memory_file(MemFile)).

memory_file_to_atom_or_codes(MemFile, atom(Text), Encoding) :-
    !,
    memory_file:memory_file_to_atom(MemFile, Text, Encoding).
memory_file_to_atom_or_codes(MemFile, codes(Text), Encoding) :-
    !,
    memory_file:memory_file_to_codes(MemFile, Text, Encoding).
