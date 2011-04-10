% Acts as an interface to the system. Sets up load paths and provides
% a predicate for running the test suite.

setup_globals :-
    % For optimized compiles, tests are by default ignored.
    set_test_options([load(always)]).
    % Try to make everything as UTF-8 as possible.
    % set_prolog_flag(encoding, utf8). % When using streams, global setting.
    % Hunting implicit dependencies is easier without autoload.
    % set_prolog_flag(autoload, false),
    % Displays how modules and such are located.
    % set_prolog_flag(verbose_file_search, true).

setup_load_paths :-
    prolog_load_context(directory, Root), % Available only during compilation.
    setup_path(Root, '/lib', foreign),
    setup_path(Root, '/src', misc),
    setup_path(Root, '/src/bson', bson),
    setup_path(Root, '/src/mongo', mongo).

setup_path(PathPrefix, PathSuffix, Name) :-
    atom_concat(PathPrefix, PathSuffix, Path),
    asserta(user:file_search_path(Name, Path)).

:- setup_globals.
:- setup_load_paths.

% Simply loading this speeds up phrase, maplist, etc. Can be safely removed.
% Or does it?
% :- use_module(library(apply_macros)).

:- include(misc(common)).

test :-
    load_all_modules,
    load_all_tests,
    run_test_suite.

cov :-
    load_all_modules,
    load_all_tests,
    run_test_suite_with_coverage.

gen_doc :-
    load_all_modules,
    use_module(library(doc_latex)),
    Modules =
    [
        bson(bson),
        bson(bson_bits),
        bson(bson_decoder),
        bson(bson_encoder),
        bson(bson_unicode)
    ],
    doc_latex:doc_latex(Modules, 'doc/prolongo.tex', [public_only(false)]).

load_all_modules :-
    use_module(library(pldoc), []), % Load first to process all doc comments.
    use_module(bson(bson), []).

load_all_tests :-
    plunit:load_test_files([]).

run_test_suite :-
    inbuilt:format('~n% Run tests ...~n'),
    plunit:run_tests.

run_test_suite_with_coverage :-
    inbuilt:format('~n% Run tests ...~n'),
    show_coverage(plunit:run_tests).
