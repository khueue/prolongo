/** <module> MongoDB driver.
 *
 *  Provides connection management and wraps the MongoDB API.
 *
 *  @see <http://www.mongodb.org/>
 */

:- module(mongo,
    [
        version/1
        % And see reexports below.
    ]).

:- reexport(
    [
        mongo_connection,
        mongo_database,
        mongo_defaults,
        mongo_collection,
        mongo_find,
        mongo_cursor,
        mongo_insert,
        mongo_update,
        mongo_delete,
        mongo_command
    ]).

:- include(misc(common)).

% Internal modules.
:- use_module(mongo_socket, []).
:- use_module(mongo_util, []).
:- use_module(mongo_bytes, []).
:- use_module(mongo_test_helper, []).
:- use_module(misc(util), []).
:- use_module(bson(bson), []).

%%  version(?Version) is semidet.
%
%   True if Version is a list representing the major, minor
%   and patch version numbers of this library.

version([1,0,0]).
