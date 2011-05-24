:- module(mongo_database,
    [
        get_collection/3
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_defaults), []).
:- use_module(mongo(mongo_util), []).

%%  get_collection.
%
%   xxxxxxxx

get_collection(Db, CollName, Coll) :-
    Db = db(_Conn,DbName),
    Coll = coll(Db,FullCollName),
    mongo_util:full_coll_name(DbName, CollName, FullCollName).
