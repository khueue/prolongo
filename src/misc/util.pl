:- module(util,
    [
        ms_since_epoch/1
    ]).

/** <module> Various utility predicates.
 */

:- include(misc(common)).

%%  ms_since_epoch(-Millis) is det.
%
%   True if Millis is the number of milliseconds elapsed
%   since the Unix epoch.

ms_since_epoch(Millis) :-
    core:get_time(FloatSeconds),
    Millis is floor(FloatSeconds * 1000).
