/**
 * Bit-packing routines used by the Prolog BSON parser.
 *
 * These functions do no error-checking whatsoever and assume
 * that an 'int' is 32 bits.
 */

#include <SWI-Prolog.h>

/**
 * Get the integer from a term.
 */
static int
get_int(term_t t)
{
    int val;
    int rc;
    rc = PL_get_integer(t, &val);
    return val;
}

/**
 * Fill a term with a byte.
 */
static void
set_int(term_t t, unsigned char byte)
{
    int rc;
    rc = PL_unify_integer(t, byte);
}

/**
 *  foreign_float_to_bytes(
 *      +Float,
 *      ?Byte0, ?Byte1, ?Byte2, ?Byte3,
 *      ?Byte4, ?Byte5, ?Byte6, ?Byte7) is semidet.
 *
 *  True if Float is the floating point number represented by the
 *  consecutive bytes Byte0..Byte7 interpreted as a 64-bit
 *  IEEE 754 double.
 */
static foreign_t
double_to_bytes8(
    term_t float_in,
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7)
{
    double val;
    unsigned char *byte = (unsigned char *)&val;
    int rc;

    rc = PL_get_float(float_in, &val);

    set_int(b0, byte[0]);
    set_int(b1, byte[1]);
    set_int(b2, byte[2]);
    set_int(b3, byte[3]);
    set_int(b4, byte[4]);
    set_int(b5, byte[5]);
    set_int(b6, byte[6]);
    set_int(b7, byte[7]);

    PL_succeed;
}

/**
 *  foreign_bytes_to_float(
 *      +Byte0, +Byte1, +Byte2, +Byte3,
 *      +Byte4, +Byte5, +Byte6, +Byte7,
 *      ?Float) is semidet.
 *
 *  True if Float is the floating point number represented by the
 *  consecutive bytes Byte0..Byte7 interpreted as a 64-bit
 *  IEEE 754 double.
 */
static foreign_t
bytes8_to_double(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7,
    term_t float_out)
{
    double val;
    unsigned char *byte = (unsigned char *)&val;

    byte[0] = get_int(b0);
    byte[1] = get_int(b1);
    byte[2] = get_int(b2);
    byte[3] = get_int(b3);
    byte[4] = get_int(b4);
    byte[5] = get_int(b5);
    byte[6] = get_int(b6);
    byte[7] = get_int(b7);

    return PL_unify_float(float_out, val);
}

/**
 *  foreign_integer_to_bytes(
 *      +Integer,
 *      ?Byte0, ?Byte1, ?Byte2, ?Byte3) is semidet.
 *
 *  True if Integer is the integer represented by the consecutive
 *  bytes Byte0..Byte3 interpreted as a signed 32-bit
 *  little-endian integer.
 */
static foreign_t
int32_to_bytes4(
    term_t int32_in,
    term_t b0, term_t b1, term_t b2, term_t b3)
{
    int val; /* NOTE: Assuming 32 bits! */
    unsigned char *byte = (unsigned char *)&val;
    int rc;

    rc = PL_get_integer(int32_in, &val);

    set_int(b0, byte[0]);
    set_int(b1, byte[1]);
    set_int(b2, byte[2]);
    set_int(b3, byte[3]);

    PL_succeed;
}

/**
 *  foreign_bytes_to_integer(
 *      +Byte0, +Byte1, +Byte2, +Byte3,
 *      ?Integer) is semidet.
 *
 *  True if Integer is the integer represented by the consecutive
 *  bytes Byte0..Byte3 interpreted as a signed 32-bit
 *  little-endian integer.
 */
static foreign_t
bytes4_to_int32(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t int32_out)
{
    int val; /* NOTE: Assuming 32 bits! */
    unsigned char *byte = (unsigned char *)&val;

    byte[0] = get_int(b0);
    byte[1] = get_int(b1);
    byte[2] = get_int(b2);
    byte[3] = get_int(b3);

    return PL_unify_integer(int32_out, val);
}

/**
 *  foreign_integer_to_bytes(
 *      +Integer,
 *      ?Byte0, ?Byte1, ?Byte2, ?Byte3,
 *      ?Byte4, ?Byte5, ?Byte6, ?Byte7) is semidet.
 *
 *  True if Integer is the integer represented by the consecutive
 *  bytes Byte0..Byte7 interpreted as a signed 64-bit
 *  little-endian integer.
 */
static foreign_t
int64_to_bytes8(
    term_t int64_in,
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7)
{
    int64_t val;
    unsigned char *byte = (unsigned char *)&val;
    int rc;

    rc = PL_get_int64(int64_in, &val);

    set_int(b0, byte[0]);
    set_int(b1, byte[1]);
    set_int(b2, byte[2]);
    set_int(b3, byte[3]);
    set_int(b4, byte[4]);
    set_int(b5, byte[5]);
    set_int(b6, byte[6]);
    set_int(b7, byte[7]);

    PL_succeed;
}

/**
 *  foreign_bytes_to_integer(
 *      +Byte0, +Byte1, +Byte2, +Byte3,
 *      +Byte4, +Byte5, +Byte6, +Byte7,
 *      ?Integer) is semidet.
 *
 *  True if Integer is the integer represented by the consecutive
 *  bytes Byte0..Byte7 interpreted as a signed 64-bit
 *  little-endian integer.
 */
static foreign_t
bytes8_to_int64(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7,
    term_t int64_out)
{
    int64_t val;
    unsigned char *byte = (unsigned char *)&val;

    byte[0] = get_int(b0);
    byte[1] = get_int(b1);
    byte[2] = get_int(b2);
    byte[3] = get_int(b3);
    byte[4] = get_int(b4);
    byte[5] = get_int(b5);
    byte[6] = get_int(b6);
    byte[7] = get_int(b7);

    return PL_unify_int64(int64_out, val);
}

/**
 * Export functions to Prolog. Called implicitly by Prolog's
 * use_foreign_library/1.
 */
install_t
install_bson_bits(void)
{
    PL_register_foreign("foreign_integer_to_bytes", 5, int32_to_bytes4,  0);
    PL_register_foreign("foreign_bytes_to_integer", 5, bytes4_to_int32,  0);

    PL_register_foreign("foreign_integer_to_bytes", 9, int64_to_bytes8,  0);
    PL_register_foreign("foreign_bytes_to_integer", 9, bytes8_to_int64,  0);

    PL_register_foreign("foreign_float_to_bytes",   9, double_to_bytes8, 0);
    PL_register_foreign("foreign_bytes_to_float",   9, bytes8_to_double, 0);
}
