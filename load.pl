% This makes listing/1 problematic. Avoid for now.
% :- set_prolog_flag(iso, true).

% We need this, since the default is to skip tests when compiling with -O.
:- set_test_options([load(always)]).

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

test :-
    load_all_modules,
    io:format('~n% Running tests~n'),
    run_tests.

load_all_modules :-
    use_module(['src/bson/*.pl', 'src/mongo/*.pl']).
