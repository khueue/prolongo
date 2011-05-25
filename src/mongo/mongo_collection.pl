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

%%  get_namespace.
%
%   xxxxxxxx

get_namespace(collection(_Db,Namespace), Namespace).

%%  get_connection.
%
%   xxxxxxxx

get_connection(collection(database(Conn,_DbName),_Namespace), Conn).
