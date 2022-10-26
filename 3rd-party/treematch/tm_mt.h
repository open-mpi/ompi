#include "ompi_config.h"

OMPI_HIDDEN void tm_init_genrand(unsigned long s);

/* generates a random number on the interval [0,0x7fffffff] */
OMPI_HIDDEN unsigned long tm_genrand_int32(void);
