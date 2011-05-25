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

%%  get_collection(+Database, +CollectionName, -Collection) is det.
%
%   Collection is a handle to the collection CollectionName in Database.

get_collection(Database, CollectionName, Collection) :-
    Database = database(_Connection,DatabaseName),
    namespace(DatabaseName, CollectionName, Namespace),
    Collection = collection(Database,Namespace).

namespace(Database, CollectionName, Namespace) :-
    core:atomic_list_concat([Database,CollectionName], '.', Namespace).
