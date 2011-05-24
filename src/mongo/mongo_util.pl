:- module(mongo_util,
    [
        full_coll_name/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

full_coll_name(Database, Collection, FullCollName) :-
    core:atomic_list_concat([Database,Collection], '.', FullCollName).
