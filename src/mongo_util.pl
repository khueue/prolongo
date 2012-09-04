/** <module> Helpers.
 */

:- module(_,
    [
        options_to_bitmask/3,
        ms_since_epoch/1,
        get_nth1_arg/3,
        atom_contains/2
    ]).

:- include(include/common).

%%  options_to_bitmask(+Options, +PredOptionToBitmask, ?Bitmask) is semidet.
%
%   True if Bitmask is an unsigned integer with bits set according to
%   which Options are given, where each option gets its value from
%   calling PredOptionToBitmask(Option, Bitmask).

options_to_bitmask(Options, Pred, Bitmask) :-
    options_to_bitmask(Options, Pred, 0, Bitmask).

options_to_bitmask([], _Pred, Bitmask, Bitmask).
options_to_bitmask([Option|Options], Pred, Bitmask0, Bitmask) :-
    call(Pred, Option, Value),
    !,
    Bitmask1 is Bitmask0 \/ Value,
    options_to_bitmask(Options, Pred, Bitmask1, Bitmask).
options_to_bitmask([Option|_], _, _, _) :-
    throw(mongo_error('unknown option', [Option])).

%%  ms_since_epoch(-Millis) is det.
%
%   True if Millis is the number of milliseconds elapsed
%   since the Unix epoch.

ms_since_epoch(Millis) :-
    core:get_time(FloatSeconds),
    Millis is floor(FloatSeconds * 1000).

%%  get_nth1_arg(+Struct, +Index, ?Arg) is semidet.
%
%   True if Arg is the argument at Index in Struct. Indexing
%   starts at 1.

get_nth1_arg(Struct, Index, Arg) :-
    core:arg(Index, Struct, Arg).

%%  atom_contains(+Atom, +SubAtom) is semidet.
%
%   True if SubAtom is a subatom of Atom.

atom_contains(Atom, SubAtom) :-
    core:sub_atom(Atom, _, _, _, SubAtom),
    !.
