# MongoDB Driver for Prolog

## Todo

 * Move BSON to separate repository (figure out how to load it properly).
 * Reduce amount of arguments to CRUD predicates (start with update?).
 * Make BSON exceptions more idiomatic: bson_error(Desc, EnvList).

## Release History

### Version 2.0.0 (not yet released)

 * Updated BSON parser to handle new binary 'uuid' subtype.
 * Rewritten exception handling (not done yet XXX).
 * Fix buggy hex_bytes/2 (used when handling object_id).
 * Hopefully improved compilation on Windows (now includes stdint.h).
 * Test suite can be run in parallel by separate Prolog sessions.
 * Better API documentation (in the code).
 * Minor performance improvements and code cleanup.
 * Internal module usage rewritten.
 * Now tested on MongoDB 2.2.0.
 * Prepared for SWI-Prolog 6.2.0.

### Version 1.0.0 (2012-08-11)

 * First real release.
 * Fix issue #1 ("Two tests failing on Swi-Prolog 6.0.2").
 * Add runnable example program (simple todo).

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite. Part of the test suite requires a MongoDB instance
running on localhost on the default port. See the example app below, and
the tests (*.plt) in the src folder for usage examples.

## Usage Example

A small to-do application (see the examples folder):

```prolog
#!/usr/bin/env swipl --quiet -O -t todo -f

% Usage: Run `examples/todo.pl` from the project root.

% Setup prolongo load paths and load the library.
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
```

Consult the file and make sure you have a MongoDB instance running on
localhost, then go:

    ?- todo.
    --- Simple Todo ---
    Id                        Label              Priority

    Enter list/add/delete/quit: add.
    Label: 'Make tea'.
    Priority: 1.

    Enter list/add/delete/quit: add.
    Label: 'Go for a walk'.
    Priority: 2.

    Enter list/add/delete/quit: list.
    Id                        Label              Priority
    4dff66bd4c594ffa3e17cb70  Make tea           1
    4dff66eb4c594ffa3e17cb71  Go for a walk      2

    Enter list/add/delete/quit: delete.
    Id: '4dff66eb4c594ffa3e17cb71'.

    Enter list/add/delete/quit: list.
    Id                        Label              Priority
    4dff66bd4c594ffa3e17cb70  Make tea           1

    Enter list/add/delete/quit: quit.
    Bye!

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 6.0.2)
    * Autoloading must be turned on (default).
 * ANSI C compiler (modify Makefile if not GCC) (tested on Mac OS X
   using GCC and Clang)
 * MongoDB (tested on Mac OS X using MongoDB 2.2.0)

## License

Licensed under the MIT license which can be found in the file
`LICENSE` in the project root.

## Coding Guidelines

 * Use empty imports (use_module(mymodule, [])) in order to not
   pollute the namespace.
 * Always use module prefixes (mymodule:predicate(...)) in order to
   clarify where things are coming from.
 * Always use the "made-up" module prefix "core:" when calling
   built-in predicates. This is completely unnecessary, and doesn't even
   work in all cases, but I think it is a good idea as long as it doesn't
   cause any problems. This decision may need to be revised when
   compatibility between different Prologs is investigated.
 * Avoid the if-then-else construct. It just looks ugly.
 * Avoid disjunctions. They are ugly, and can be replaced by properly
   written helpers. Think: premises are "and", clauses are "or".
 * Use cuts where appropriate, and try to keep each cut on a line by
   itself unless its placement is obvious and consistent in each clause.
   PlUnit is excellent at pointing out when tests succeed but leave
   choice points.
 * Try to avoid spaces within lists and structures, but always use
   spaces between arguments.
 * Predicates, atoms, etc. should use "this_naming_style" while variables
   should use "ThisNamingStyle".
 * Try to stick to the PlDoc structure.
 * If in doubt, consult: <http://www.ai.uga.edu/mc/plcoding.pdf>
