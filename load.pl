% This makes listing/1 problematic. Avoid for now.
% :- set_prolog_flag(iso, true).

% We need this, since the default is to skip tests when compiling with -O.
:- set_test_options([load(always)]).

% Enable this to see how modules and such are located.
% :- set_prolog_flag(verbose_file_search, true).

:-
    prolog_load_context(directory, Dir), % Dir of this file.
    atom_concat(Dir, '/src/bson', DirBson),
    asserta(user:file_search_path(bson, DirBson)),
    atom_concat(Dir, '/src/mongo', DirMongo),
    asserta(user:file_search_path(mongo, DirMongo)),
    atom_concat(Dir, '/lib', DirLib),
    asserta(user:file_search_path(foreign, DirLib)).

run :-
    use_module(bson(bson)),
    io:format('~n% Running tests ...~n'),
    run_tests.
