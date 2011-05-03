:- module(util,
    [
        list_shaped/1,
        ms_since_epoch/1,
        get_arg/3,
        set_arg/4,
        replace_nth1/4
    ]).

/** <module> Various utility predicates.
 */

:- include(misc(common)).

%%  list_shaped(+Term) is semidet.
%
%   True if Term looks like a list (no recursive checks).

list_shaped([]).
list_shaped([_|_]).

%%  ms_since_epoch(-Millis) is det.
%
%   True if Millis is the number of milliseconds elapsed
%   since the Unix epoch.

ms_since_epoch(Millis) :-
    core:get_time(FloatSeconds),
    Millis is floor(FloatSeconds * 1000).

%%  get_arg(+Struct, +Index, ?Arg) is semidet.
%
%   True if Arg is the argument at Index in Struct. Indexing
%   starts at 1.

get_arg(Struct, Index, Arg) :-
    core:arg(Index, Struct, Arg).

%%  set_arg(+Struct, +Index, +Arg, ?NewStruct) is semidet.
%
%   True if NewStruct is Struct with the argument at Index
%   replaced with Arg. Indexing starts at 1.
%
%   @tbd There must be a better way to do this?

set_arg(Struct, Index, Arg, Struct1) :-
    Struct =.. [Name|Args],
    replace_nth1(Args, Index, Arg, Args1),
    Struct1 =.. [Name|Args1].

%%  replace_nth1(+List, +Index, +Value, ?NewList) is semidet.
%
%   True if NewList is List with the element at Index
%   replaced by Value. Indexing starts at 1.

replace_nth1([_|Xs], 1, Value, [Value|Xs]) :- !.
replace_nth1([X|Xs], N, Value, [X|Xs1]) :-
    N1 is N - 1,
    replace_nth1(Xs, N1, Value, Xs1).
