# Design and Implementation of a MongoDB Driver for Prolog

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * foreign(memfile) (should be autoloaded)
    * foreign(apply_macros) (loaded by load.pl, can be safely removed)
 * C compiler (modify the Makefile if you aren't using GCC)

## Todo

 * Finish the decoder.
 * Write more decoder tests.
 * Research exception handling in Prolog.

## Thoughts

 * SWI Association Lists?
 * Implement BSON in Prolog or use existing C lib? Prolog is more fun.
   C would be better for efficiency, and probably also easier since the lib
   already exists... **Solution:** Mostly Prolog, some bit-hacking in C.
 * Interleave tests with code or keep separate test files? I like
   interleaving... **Solution:** Looks like no interleaving, but tests
   go at the top of each module.

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
