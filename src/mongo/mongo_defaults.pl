:- module(mongo_defaults,
    [
        host/1,
        port/1
    ]).

/** <module> MongoDB default settings.
 */

:- include(misc(common)).

%%  host(?Host) is semidet.
%
%   True if Host is the default hostname used by MongoDB.

host(localhost).

%%  port(?Port) is semidet.
%
%   True if Port is the default port used by MongoDB.

port(27017).
