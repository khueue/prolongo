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
        new_connection/1,
        new_connection/3,
        free_connection/1,
        get_database/3
    ]).
:- reexport(mongo(mongo_database),
    [
        get_collection/3
    ]).
:- reexport(mongo(mongo_collection),
    [
        collection_database/2
    ]).
:- reexport(mongo(mongo_find),
    [
        find_one/3,
        find_one/4,
        find_all/4,
        find/7,
        find/8
    ]).
:- reexport(mongo(mongo_cursor),
    [
        kill/1,
        kill_batch/1,
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
        delete/2,
        delete/3
    ]).
:- reexport(mongo(mongo_command),
    [
        command/3,
        list_commands/2,
        list_collection_names/2,
        list_database_infos/2,
        list_database_names/2,
        drop_collection/1,
        drop_database/1,
        get_last_error/2
    ]).

:- include(misc(common)).
