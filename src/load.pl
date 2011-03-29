%:- set_prolog_flag(iso, true).
:- encoding(utf8).

:- include(includes).

run :-
    nl,
    ignore(bson:test_decode),
    nl.
