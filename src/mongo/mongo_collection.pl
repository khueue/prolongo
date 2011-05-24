:- module(mongo_collection,
    [
        get_namespace/2,
        get_connection/2
    ]).

/** <module> xxxxxxxxx
 */

:- include(misc(common)).

:- use_module(bson(bson), []).
:- use_module(misc(util), []).
:- use_module(mongo(mongo_defaults), []).

%%  get_namespace.
%
%   xxxxxxxx

get_namespace(coll(_Db,FullCollName), FullCollName).

%%  get_connection.
%
%   xxxxxxxx

get_connection(coll(db(Conn,_DbName),_FullCollName), Conn).
