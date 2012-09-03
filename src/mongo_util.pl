/** <module> Helpers.
 */

:- module(_,
    [
        options_to_bitmask/3
    ]).

:- include(misc(common)).

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
