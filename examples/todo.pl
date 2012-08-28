#!/usr/bin/env swipl --quiet -O -t todo -f

% Usage: Run `examples/todo.pl` from the project root.

:- [load].
:- use_module(mongo(mongo), []). % Empty import forces use of namespace.

todo :-
    print_welcome,
    setup_call_cleanup(
        mongo:new_connection(Connection),
        todo_run(Connection),
        mongo:free_connection(Connection)).

print_welcome :-
    format('--- Simple Todo ---~n'),
    format('Terminate input with a period.~n~n').

todo_run(Connection) :-
    mongo:get_database(Connection, 'prolongo_example_todo', Database),
    mongo:get_collection(Database, 'items', Collection),
    action(list, Collection).

action(list, Collection) :- !,
    list_items(Collection),
    new_action(Collection).
action(add, Collection) :- !,
    add_item(Collection),
    new_action(Collection).
action(delete, Collection) :- !,
    delete_item(Collection),
    new_action(Collection).
action(quit, _Collection) :- !,
    format('Bye!~n').
action(_Unknown, Collection) :-
    format('Unknown alternative.~n'),
    new_action(Collection).

new_action(Collection) :-
    format('~nEnter list/add/delete/quit: '),
    read(Action),
    action(Action, Collection).

list_items(Collection) :-
    mongo:find_all(Collection, [], [], Docs),
    print_items(Docs).

print_items(Docs) :-
    format('Id~26|Label~45|Priority~n'),
    print_items_aux(Docs).

print_items_aux([]).
print_items_aux([Doc|Docs]) :-
    bson:doc_get(Doc, '_id', object_id(Id)),
    bson:doc_get(Doc, label, Label),
    bson:doc_get(Doc, priority, Priority),
    format('~w~26|~w~45|~w~n', [Id,Label,Priority]),
    print_items_aux(Docs).

add_item(Collection) :-
    format('Label: '),
    read(Label),
    format('Priority: '),
    read(Priority),
    Doc = [label-Label,priority-Priority],
    mongo:insert(Collection, Doc).

delete_item(Collection) :-
    format('Id: '),
    read(Id),
    mongo:delete(Collection, ['_id'-object_id(Id)]).
