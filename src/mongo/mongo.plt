:- include(misc(common)).

database('prolongo_test').
collection('testcoll').
database_dot_collection('prolongo_test.testcoll').

ms_since_epoch(MilliSeconds) :-
    core:get_time(FloatSeconds),
    FloatMilliSeconds is FloatSeconds * 1000,
    MilliSeconds is floor(FloatMilliSeconds).

%%  doc_get(+Doc, +Key, ?Value) is semidet.
%
%   True if Value is the value associated with Key in Doc,
%   or @(null) if the Key cannot be found. This means that there
%   is no way of knowing if it actually was @(null) or not found.

doc_get([], _, @(null)).
doc_get([K-V|_], K, V) :- !.
doc_get([_|Pairs], K, V) :-
    doc_get(Pairs, K, V).

%%  doc_put(+Doc, +Key, +Value, ?NewDoc) is semidet.
%
%   True if NewDoc is Doc with the addition or update of the pair
%   Key-Value.

doc_put([], K, V, [K-V]).
doc_put([K-_|Pairs], K, V, [K-V|Pairs]) :- !.
doc_put([Other|Pairs], K, V, [Other|Pairs1]) :-
    doc_put(Pairs, K, V, Pairs1).

%%  doc_delete(+Doc, +Key, ?NewDoc) is semidet.
%
%   True if NewDoc is Doc with the first pair with Key as
%   key removed. No change if Key cannot be found.

doc_delete([], _, []).
doc_delete([K-_|Pairs], K, Pairs) :- !.
doc_delete([Other|Pairs], K, [Other|Pairs1]) :-
    doc_delete(Pairs, K, Pairs1).

:- begin_tests('mongo:insert/3').

test('insert',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
    ms_since_epoch(MilliSeconds),
    Document =
    [
        hello - [åäö,5.05],
        now   - utc(MilliSeconds)
    ],
    database_dot_collection(DbDotColl),
    mongo:insert(Mongo, Document, DbDotColl).

:- end_tests('mongo:insert/3').

:- begin_tests('mongo:command/3').

test('drop collection',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
    Command =
    [
        drop - Coll
    ],
    collection(Coll),
    database(Db),
    mongo:command(Mongo, Command, Db, Result),
    doc_get(Result, ok, 1.0).

/*
% Takes a bit too long when MongoDB reallocates the collection later.
test('drop database',
[
    setup(mongo:new_mongo(Mongo)),
    cleanup(mongo:free_mongo(Mongo))
]) :-
    Command =
    [
        dropDatabase - 1
    ],
    database(Db),
    mongo:command(Mongo, Command, Db).
*/

:- end_tests('mongo:command/3').
