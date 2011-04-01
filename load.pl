% This makes listing/1 problematic. Avoid for now.
% :- set_prolog_flag(iso, true).

% We need this, since the default is to skip tests when compiling with -O.
:- set_test_options([load(always)]).

:- encoding(utf8).
:- set_prolog_flag(encoding, utf8).

% Enable this to see how modules and such are located.
% :- set_prolog_flag(verbose_file_search, true).

% Set up load paths.
:-
    prolog_load_context(directory, RootDir), % Dir of this file.
    atom_concat(RootDir, '/src/bson', Bson),
    asserta(user:file_search_path(bson, Bson)),
    atom_concat(RootDir, '/src/mongo', Mongo),
    asserta(user:file_search_path(mongo, Mongo)),
    atom_concat(RootDir, '/lib', Lib),
    asserta(user:file_search_path(foreign, Lib)).

do_term(Term, In, Tail) :-
        with_output_to(codes(In, Tail), write(Term)).
% ?- phrase(do_term(hello), X).
test :-
    load_all_modules,
    run_test_suite.

load_all_modules :-
    use_module(['src/**/*.pl']).

run_test_suite :-
    io:format('~n% Running tests~n'),
    run_tests.
