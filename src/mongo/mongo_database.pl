:- module(mongo_database,
    [
        get_collection/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_util), []).

%%  get_collection.
%
%   xxxxxxxx

get_collection(Db, CollName, Coll) :-
    Db = database(_Conn,DbName),
    Coll = collection(Db,FullCollName),
    full_coll_name(DbName, CollName, FullCollName).

full_coll_name(Database, Collection, FullCollName) :-
    core:atomic_list_concat([Database,Collection], '.', FullCollName).
