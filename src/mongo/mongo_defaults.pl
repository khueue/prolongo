:- module(mongo_defaults,
    [
        host/1,
        port/1
    ]).

/** <module> MongoDB default settings.
 */

:- include(misc(common)).

host(localhost).
port(27017).
