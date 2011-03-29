%:- set_prolog_flag(iso, true).
:- encoding(utf8).

:- include(includes).

run :-
    nl,
    bson:test_decode,
    nl.
