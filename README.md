# Design and Implementation of a MongoDB Driver for Prolog

## Todo

 * Clean up the decoder. What is and what isn't a value_xxx? Extract preds?
 * More validation in decoder?
 * Think about and start to implement encoder.
 * Research exception handling in Prolog.

## Usage

Clone the repository and run `make` to compile the necessary C libraries
and run the test suite.

## Dependencies

 * SWI-Prolog (tested on Mac OS X using SWI 5.10.2)
    * foreign(memfile) (should be autoloaded)
    * foreign(apply_macros) (loaded by load.pl, can be safely removed)
 * C compiler (modify the Makefile if you aren't using GCC)

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
