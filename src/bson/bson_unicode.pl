:- module(bson_unicode,
    [
        utf8_bytes/2,
        utf8_bytes_size/3
    ]).

/** <module> Unicode conversions between atom and bytes.
 *
 *  A bit of a hack, but in order to interpret raw bytes as UTF-8
 *  we use a memory file as a temporary buffer, fill it with the
 *  bytes and then read them back, treating them as UTF-8.
 */

:- include(misc(common)).

/*
:- use_module(library(utf8), []).

% Investigate this: using the library predicate utf8:utf8_codes/2
% seems to be faster when the length is approx < 30 bytes.
% (I guess the overhead of memory files etc. is too high when the
% string is so short anyway.)
% It might be interesting to choose this predicate if the string
% is short enough. This might be worthwhile (if we already know
% the length beforehand!), since most keys and
% probably many text values are shorter than 30 bytes.

utf8_bytes(Utf8, Bytes) :-
    core:nonvar(Utf8),
    !,
    atom_codes(Utf8, Codes),
    phrase(utf8:utf8_codes(Codes), Bytes).
utf8_bytes(Utf8, Bytes) :-
    core:nonvar(Bytes),
    !,
    phrase(utf8:utf8_codes(Codes), Bytes),
    atom_codes(Utf8, Codes).
*/

%%  utf8_bytes(+Utf8, ?Bytes) is semidet.
%%  utf8_bytes(?Utf8, +Bytes) is semidet.
%
%   True if Utf8 is the atom represented by the UTF-8 encoded Bytes.

utf8_bytes(Utf8, Bytes) :-
    core:nonvar(Utf8),
    !,
    utf8_to_bytes(Utf8, Bytes).
utf8_bytes(Utf8, Bytes) :-
    core:nonvar(Bytes),
    !,
    bytes_to_utf8(Bytes, Utf8).

%%  utf8_bytes_size(+Utf8, ?Bytes, ?Size) is semidet.
%
%   True if Utf8 is the atom represented by the UTF-8 encoded Bytes,
%   and Size is the number of Bytes (not the number of code points).
%
%   Right now, this predicate simply counts the number of bytes in
%   Bytes, but maybe this can be done on-the-fly in future versions.

utf8_bytes_size(Utf8, Bytes, NumBytes) :-
    utf8_bytes(Utf8, Bytes),
    lists:length(Bytes, NumBytes).

%%  utf8_to_bytes(+Utf8, ?Bytes) is semidet.
%
%   True if Utf8 is the atom represented by the UTF-8 encoded Bytes.

utf8_to_bytes(Utf8, Bytes) :-
    setup_call_cleanup(
        memory_file:new_memory_file(MemFile),
        utf8_to_memory_file_to_bytes(Utf8, MemFile, Bytes),
        memory_file:free_memory_file(MemFile)).

utf8_to_memory_file_to_bytes(Utf8, MemFile, Bytes) :-
    utf8_to_memory_file(Utf8, MemFile),
    memory_file_to_bytes(MemFile, Bytes).

utf8_to_memory_file(Utf8, MemFile) :-
    setup_call_cleanup(
        memory_file:open_memory_file(MemFile, write, Write, [encoding(utf8)]),
        core:format(Write, '~w', [Utf8]),
        core:close(Write)).

memory_file_to_bytes(MemFile, Bytes) :-
    setup_call_cleanup(
        memory_file:open_memory_file(MemFile, read, Read, [encoding(octet)]),
        readutil:read_stream_to_codes(Read, Bytes),
        core:close(Read)).

/*
% Previous version, seems a bit slower (probably due to atom_codes).
utf8_to_bytes(Utf8, Bytes) :-
    core:atom_codes(Utf8, Codes),
    setup_call_cleanup(
        charsio:open_chars_stream(Codes, ReadStream),
        stream_to_bytes(ReadStream, Bytes),
        core:close(ReadStream)).

stream_to_bytes(ReadStream, Bytes) :-
    core:set_stream(ReadStream, encoding(octet)),
    readutil:read_stream_to_codes(ReadStream, Bytes).
*/

%%  bytes_to_utf8(+Bytes, ?Utf8) is semidet.
%
%   True if Utf8 is the atom represented by the UTF-8 encoded Bytes.

bytes_to_utf8(Bytes, Utf8) :-
    core:atom_codes(RawAtom, Bytes),
    setup_call_cleanup(
        memory_file:atom_to_memory_file(RawAtom, MemFile),
        memory_file:memory_file_to_atom(MemFile, Utf8, utf8),
        memory_file:free_memory_file(MemFile)).
