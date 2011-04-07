# Design and Implementation of a MongoDB Driver for Prolog

## Todo

 * Finish encoder.
 * Properly document predicates. Some are only stubs.
 * More validation in decoder?

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * foreign(memfile) (should be autoloaded)
    * foreign(apply_macros) (loaded by load.pl, can be safely removed)
    * library(readutil) (uses foreign library if it exists)
 * ANSI C compiler (modify the Makefile if you aren't using GCC)
 * _Not yet._ MongoDB (tested on Mac OS X using MongoDB 1.8.0)

## Thoughts

 * SWI Association Lists? Probably not. Should stick with stuff that is
   common to most Prologs.

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
