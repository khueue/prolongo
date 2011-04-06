/**
 * Bit-packing routines used by the Prolog BSON parser.
 * These functions do no error-checking whatsoever.
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
 * Convert 8 bytes into a double (64-bit IEEE 754 floating point).
 */
static foreign_t
pl_bytes8_to_double(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7,
    term_t result)
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

    return PL_unify_float(result, val);
}

/**
 * Convert 4 bytes into a long (32-bit). Byte-order is little-endian.
 */
static foreign_t
pl_bytes4_to_int32(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t result)
{
    int32_t val;
    unsigned char *byte = (unsigned char *)&val;

    byte[0] = get_int(b0);
    byte[1] = get_int(b1);
    byte[2] = get_int(b2);
    byte[3] = get_int(b3);

    return PL_unify_integer(result, val);
}

/**
 * Convert 8 bytes into an int64_t (64-bit). Byte-order is little-endian.
 */
static foreign_t
pl_bytes8_to_int64(
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7,
    term_t result)
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

    return PL_unify_int64(result, val);
}

/**
 * XXX
 */
static foreign_t
pl_double_to_bytes8(
    term_t float_in,
    term_t b0, term_t b1, term_t b2, term_t b3,
    term_t b4, term_t b5, term_t b6, term_t b7)
{
    double val;
    unsigned char *byte = (unsigned char *)&val;
    int rc;

    rc = PL_get_float(float_in, &val);

    rc = PL_unify_integer(b0, byte[0] & 0xFF);
    rc = PL_unify_integer(b1, byte[1] & 0xFF);
    rc = PL_unify_integer(b2, byte[2] & 0xFF);
    rc = PL_unify_integer(b3, byte[3] & 0xFF);
    rc = PL_unify_integer(b4, byte[4] & 0xFF);
    rc = PL_unify_integer(b5, byte[5] & 0xFF);
    rc = PL_unify_integer(b6, byte[6] & 0xFF);
    rc = PL_unify_integer(b7, byte[7] & 0xFF);

    PL_succeed;
}

/**
 * Export functions to Prolog. Called implicitly by Prolog's
 * use_foreign_library/1.
 */
install_t
install_bson_bits(void)
{
    PL_register_foreign("bytes_to_float",   9, pl_bytes8_to_double, 0);
    PL_register_foreign("bytes_to_integer", 5, pl_bytes4_to_int32,  0);
    PL_register_foreign("bytes_to_integer", 9, pl_bytes8_to_int64,  0);
    PL_register_foreign("float_to_bytes",   9, pl_double_to_bytes8, 0);
}
