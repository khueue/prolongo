% :- set_prolog_flag(iso, true).

% UTF-8 is awesome.
:- encoding(utf8).

% We need this, since the default is to skip tests when compiled with -O.
:- set_test_options([load(always)]).

:- use_module(bson).

run :-
    nl,
    ignore(bson:test_decode),
    nl,
    run_tests,
    nl.
