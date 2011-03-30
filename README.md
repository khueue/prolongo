# Design and Implementation of a MongoDB Driver for Prolog

## Food for Thought

 * Implement BSON in Prolog or use existing C lib? Prolog is more fun.
   C would be better for efficiency, and probably also easier since the lib
   already exists... *Resolution:* Mostly Prolog, some bithacking in C.
 * Interleave tests with code or keep separate test files? I like
   interleaving...

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
