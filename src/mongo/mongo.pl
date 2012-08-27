:- module(mongo,
    [
        version/1
        % And see reexports below.
    ]).

/** <module> MongoDB driver.
 *
 *  Provides connection management and wraps the MongoDB API.
 *
 *  @see <http://www.mongodb.org/>
 */

:- reexport(
    [
        mongo(mongo_connection),
        mongo(mongo_database),
        mongo(mongo_defaults),
        mongo(mongo_collection),
        mongo(mongo_find),
        mongo(mongo_cursor),
        mongo(mongo_insert),
        mongo(mongo_update),
        mongo(mongo_delete),
        mongo(mongo_command)
    ]).

:- include(misc(common)).

% Internal modules.
:- use_module(mongo(mongo_socket), []).
:- use_module(mongo(mongo_util), []).
:- use_module(mongo(mongo_bytes), []).
:- use_module(mongo(mongo_test_helper), []).
:- use_module(misc(util), []).
:- use_module(bson(bson), []).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([1,0,0]).
