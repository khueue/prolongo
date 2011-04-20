# Design and Implementation of a MongoDB Driver for Prolog

## Todo

### Report

 * Think about structure and start to write something.

### Development

 * Investigate and wrap the MongoDB API.
 * (Decide on amount of (and implement) validation in encoder/decoder.)

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite. Part of the test suite requires a MongoDB instance
running on localhost on the default port.

``` prolog
    doc_delete([], _, []).
    doc_delete([K-_|Pairs], K, Pairs) :- !.
    doc_delete([Other|Pairs], K, [Other|Pairs1]) :-
        doc_delete(Pairs, K, Pairs1).
```

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * Autoloading must be turned on (default).
 * ANSI C compiler (modify the Makefile if other than GCC)
 * MongoDB (tested on Mac OS X using MongoDB 1.8.1)

## Coding Guidelines (That Might Surprise)

 * Use empty imports (use_module(mymodule, [])) in order to not
   pollute the namespace.
 * Always use module prefixes (mymodule:predicate(...)) in order to
   clarify where things are coming from.
 * Always use the "made-up" module prefix "core:" when calling
   built-in predicates. This is completely unnecessary, and maybe
   a bit weird, but I think it is a good idea as long as it doesn't
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

## Future

 * Move BSON to separate repository (BSON is not inherent to MongoDB).
 * Test and improve compatibility between Prologs.
 * Make sure exception handling is more idiomatic.
 * (Implement all the stuff I won't have time to.)

## Links

### SWI-Prolog

 * Manual: <http://www.swi-prolog.org/pldoc/refman/>
 * PlUnit: <http://www.swi-prolog.org/pldoc/package/plunit.html>
 * PlDoc: <http://www.swi-prolog.org/pldoc/package/pldoc.html>
 * C API & Sockets: <http://www.swi-prolog.org/pldoc/package/clib.html>

### MongoDB

 * Manual: <http://www.mongodb.org/display/DOCS/Manual>
 * BSON Spec: <http://bsonspec.org/>
 * Writing Drivers: <http://www.mongodb.org/display/DOCS/Writing+Drivers+and+Tools>
