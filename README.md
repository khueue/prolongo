# Design and Implementation of a MongoDB Driver for Prolog

## Todo

 * Start to write proper socket communication.
 * Start thinking about how to write the report.
 * (Decide on amount of (and implement) validation in encoder/decoder.)

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * Autoloading must be turned on (default).
 * ANSI C compiler (modify the Makefile if other than GCC)
 * _Not yet._ MongoDB (tested on Mac OS X using MongoDB 1.8.1)

## Thoughts

 * Nothing.

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
