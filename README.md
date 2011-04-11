# Design and Implementation of a MongoDB Driver for Prolog

## Todo

 * Decide on (and implement) amount of validation in encode/decode.

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
 * ANSI C compiler (modify the Makefile if other than GCC)
 * _Not yet._ MongoDB (tested on Mac OS X using MongoDB 1.8.1)

## Thoughts

 * Caveat: Depends on library autoloading.

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
