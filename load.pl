% Acts as an interface to the system. Configures load paths, loads external
% libraries and provides predicates for initiating the system.

%   prolongo_external_library_loader(-RelativePathToLoader) is multi.
%
%   Facts with paths to external dependencies relative to this project's root.
%   These facts should point to other loader scripts (usually 'load.pl' files)
%   found in the root of the respective project.

prolongo_external_library_loader('../prolog-bson/load.pl').

%   prolongo_configure_globals is det.
%
%   Configures useful globals used throughout the session.

prolongo_configure_globals :-
    % For optimized compiles, tests are by default ignored.
    set_test_options([load(always)]).
    % Try to make everything as UTF-8 as possible.
    % set_prolog_flag(encoding, utf8). % When using streams, global setting.
    % Hunting implicit dependencies is easier without autoload.
    % set_prolog_flag(autoload, false),
    % Displays how modules and such are located.
    % set_prolog_flag(verbose_file_search, true).

%   prolongo_load_external_libraries is det.
%
%   Loads external load.pl scripts required by this library.

prolongo_load_external_libraries :-
    prolog_load_context(directory, Root), % Available only during compilation.
    prolongo_external_library_loader(Loader),
    prolongo_load_external_library(Root, Loader),
    fail. % Loop through all prolongo_external_library_loader/1.
prolongo_load_external_libraries.

prolongo_load_external_library(Root, RelativePathToLoader) :-
    atomic_list_concat([Root,RelativePathToLoader], '/', Loader),
    [Loader].

%   prolongo_configure_load_paths is det.
%
%   Configures internal load paths in preparation of use_module calls.

prolongo_configure_load_paths :-
    prolog_load_context(directory, Root), % Available only during compilation.
    prolongo_configure_path(Root, 'src/misc', misc),
    prolongo_configure_path(Root, 'src', mongo).

prolongo_configure_path(PathPrefix, PathSuffix, Name) :-
    atomic_list_concat([PathPrefix,PathSuffix], '/', Path),
    asserta(user:file_search_path(Name, Path)).

% Set everything up.
:- prolongo_configure_globals.
:- prolongo_load_external_libraries.
:- prolongo_configure_load_paths.

% Simply loading this module claims to speed up phrase, maplist, etc.,
% but I haven't noticed much difference.
% :- use_module(library(apply_macros)).

:- include(misc(common)).

prolongo_load_project_modules :-
    use_module(library(pldoc), []), % Load first to enable comment processing.
    use_module(mongo(mongo), []).

prolongo_load_project_tests :-
    plunit:load_test_files([]).

%%  prolongo_test is det.
%
%   Loads everything and runs the test suite.

prolongo_test :-
    prolongo_load_project_modules,
    prolongo_load_project_tests,
    prolongo_run_test_suite.

prolongo_run_test_suite :-
    core:format('~n% Run tests ...~n'),
    call_cleanup(
        plunit:run_tests,
        mongo_test_helper:drop_all_test_databases).

%%  prolongo_cov is det.
%
%   Loads everything and runs the test suite with coverage analysis.

prolongo_cov :-
    prolongo_load_project_modules,
    prolongo_load_project_tests,
    prolongo_run_test_suite_with_coverage.

prolongo_run_test_suite_with_coverage :-
    core:format('~n% Run tests ...~n'),
    call_cleanup(
        plunit:show_coverage(plunit:run_tests),
        mongo_test_helper:drop_all_test_databases).

%%  prolongo_repl is det.
%
%   Loads everything and enters interactive mode.

prolongo_repl :-
    prolongo_load_project_modules,
    prolongo_load_project_tests.
