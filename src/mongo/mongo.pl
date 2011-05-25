:- module(mongo,
    [
        % See reexports below.
    ]).

/** <module> MongoDB driver.
 *
 *  Provides connection management and wraps the MongoDB API.
 *
 *  @see <http://www.mongodb.org/>
 */

:- reexport(mongo(mongo_connection),
    [
        new/1,
        new/3,
        free/1,
        get_database/3
    ]).
:- reexport(mongo(mongo_collection),
    [
        get_namespace/2,
        get_connection/2
    ]).
:- reexport(mongo(mongo_database),
    [
        get_collection/3
    ]).
:- reexport(mongo(mongo_find),
    [
        find_one/3,
        find_one/4,
        find_all/4,
        find/7
    ]).
:- reexport(mongo(mongo_cursor),
    [
        kill/1,
        get_more/4,
        has_more/1,
        exhaust/2
    ]).
:- reexport(mongo(mongo_insert),
    [
        insert/2,
        insert_batch/3
    ]).
:- reexport(mongo(mongo_update),
    [
        upsert/3,
        update/3,
        update_all/3
    ]).
:- reexport(mongo(mongo_delete),
    [
        delete/2
    ]).

:- include(misc(common)).
