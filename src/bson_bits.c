#include <SWI-Prolog.h>

static foreign_t
pl_bytes8_to_double(
    term_t byte0, term_t byte1, term_t byte2, term_t byte3,
    term_t byte4, term_t byte5, term_t byte6, term_t byte7,
    term_t result)
{
    double d;
    unsigned char *pd = (unsigned char *)&d;
    int temp;
    int rc;

    rc = PL_get_integer(byte0, &temp); pd[0] = temp;
    rc = PL_get_integer(byte1, &temp); pd[1] = temp;
    rc = PL_get_integer(byte2, &temp); pd[2] = temp;
    rc = PL_get_integer(byte3, &temp); pd[3] = temp;
    rc = PL_get_integer(byte4, &temp); pd[4] = temp;
    rc = PL_get_integer(byte5, &temp); pd[5] = temp;
    rc = PL_get_integer(byte6, &temp); pd[6] = temp;
    rc = PL_get_integer(byte7, &temp); pd[7] = temp;

    return PL_unify_float(result, d);
}

install_t
install_bson_bits(void)
{
    PL_register_foreign("bytes_as_float", 9, pl_bytes8_to_double, 0);
}
