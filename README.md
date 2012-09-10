# MongoDB Driver for Prolog

A MongoDB driver compatible with SWI-Prolog that implements basic CRUD
functionality. Several things have yet to be implemented (authentication,
GridFS, etc.), but the driver can be used for simple use-cases.

## Release History

### Version 1.1.0 (2012-09-10)

 * BSON parser is now an external dependency (separate repository).
 * Updated BSON parser to handle new binary 'uuid' subtype.
 * Rewritten exception handling, exceptions now follow:
`mongo_error(DescriptionAtom, ListOfRelatedVars)`.
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

## License

Licensed under the MIT license which can be found in the file
`LICENSE` in the project root.

## Todo

 * Reduce amount of arguments to CRUD predicates.

## Usage

 1. Clone the BSON parser found at <https://github.com/khueue/prolog-bson>
    and compile it.
 2. Clone prolongo.
 3. Edit the path to the BSON loader script found in `load.pl`. The path
    should be relative to prolongo's root (so you don't have to edit
    anything if you clone both BSON and prolongo into the same parent
    folder).
 4. Run `make` to run the tests (will also run the BSON tests). Some
    of the tests require a MongoDB instance running on localhost on the
    default port.
 5. See the example below and the tests (*.plt) in the `src` folder for
    more usage information.

## Usage Example

A small to-do application (found in the examples folder):

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
 * MongoDB (tested on Mac OS X using MongoDB 2.2.0)
 * BSON parser found at <https://github.com/khueue/prolog-bson>

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
