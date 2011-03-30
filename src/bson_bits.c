#include <stdlib.h>
#include <SWI-Prolog.h>

static foreign_t
pl_bytes8_to_double(
    term_t byte0, term_t byte1, term_t byte2, term_t byte3,
    term_t byte4, term_t byte5, term_t byte6, term_t byte7)
{
    term_t d = PL_new_term_ref();
    return d;
}

install_t
install_bson_bits(void)
{
    PL_register_foreign("bytes_as_float", 8, pl_bytes8_to_double, 0);
}
