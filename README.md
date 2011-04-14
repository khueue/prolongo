# Design and Implementation of a MongoDB Driver for Prolog

## Todo

### Report

 * Think about structure and start to write something.

### Development

 * Investigate and wrap the MongoDB API.
 * Add BSON tests that convert back-and-forth between pairs/assoc and bytes.
 * Properly implement bson:assoc_bson/2. (Now it just wraps pairs.)
 * (Decide on amount of (and implement) validation in encoder/decoder.)

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * Autoloading must be turned on (default).
 * ANSI C compiler (modify the Makefile if other than GCC)
 * MongoDB (tested on Mac OS X using MongoDB 1.8.1)

## Thoughts

 * Nothing.

## Future

 * Move BSON to separate repository (it is not inherent to MongoDB).

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
