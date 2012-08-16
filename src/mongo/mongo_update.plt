:- include(misc(common)).

:- use_module(mongo_test_helper, []).
:- use_module(bson(bson), []).
:- use_module(misc(util), []).

:- begin_tests('mongo:upsert/3').

test('upsert', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:upsert(Coll, [hello-world], [hello-me]),
    mongo:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:upsert/3').

:- begin_tests('mongo:update/3').

test('update normal', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:insert(Coll, [hello-world]),
    mongo:update(Coll, [hello-world], [hello-me]),
    mongo:find_one(Coll, [hello-me], Doc),
    bson:doc_get(Doc, hello, me).

:- end_tests('mongo:update/3').

:- begin_tests('mongo:update_all/3').

test('update multi', [
        setup(mongo_test_helper:up(Conn,Coll)),
        cleanup(mongo_test_helper:down(Conn))
    ]) :-
    mongo:delete(Coll, [hello-world]),
    mongo:delete(Coll, [hello-me]),
    mongo:insert(Coll, [hello-world,num-1]),
    mongo:insert(Coll, [hello-world,num-2]),
    mongo:insert(Coll, [hello-world,num-3]),
    mongo:update_all(Coll, [hello-world], ['$inc'-[num-100]]),
    mongo:find(Coll, [hello-world], [], 0, 3, _Cursor, Docs),
    lists:length(Docs, 3),
    lists:member(Doc1, Docs),
    bson:doc_get(Doc1, num, 101),
    lists:member(Doc2, Docs),
    bson:doc_get(Doc2, num, 102),
    lists:member(Doc3, Docs),
    bson:doc_get(Doc3, num, 103),
    !. % Not interested in member choices.

:- end_tests('mongo:update_all/3').

