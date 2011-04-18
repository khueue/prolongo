:- module(util,
    [
        ms_since_epoch/1
    ]).

/** <module> Various utility predicates.
 */

:- include(misc(common)).

%%  ms_since_epoch(-MilliSeconds) is det.
%
%   True if MilliSeconds is the number of milliseconds elapsed
%   since the Unix epoch.

ms_since_epoch(MilliSeconds) :-
    core:get_time(FloatSeconds),
    FloatMilliSeconds is FloatSeconds * 1000,
    MilliSeconds is floor(FloatMilliSeconds).
