/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#undef OMPI_BUILDING
#include "ompi_config.h"

#undef OMPI_GCC_INLINE_ASSEMBLY
#define OMPI_GCC_INLINE_ASSEMBLY 0

#undef OMPI_DEC_INLINE_ASSEMBLY
#define OMPI_DEC_INLINE_ASSEMBLY 0

#undef OMPI_XLC_INLINE_ASSEMBLY
#define OMPI_XLC_INLINE_ASSEMBLY 0

#include <assert.h>

#include "include/sys/atomic.h"

#if OMPI_HAVE_ATOMIC_MATH_64
volatile int64_t vol64;
int64_t val64;
int64_t old64;
int64_t new64;
#endif

int
main(int argc, char *argv[])
{
#if OMPI_HAVE_ATOMIC_MATH_64
    vol64 = 42, old64 = 42, new64 = 50;
    assert(1 == ompi_atomic_cmpset_64(&vol64, old64, new64));
    assert(new64 == vol64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(ompi_atomic_cmpset_64(&vol64, old64, new64) == 0);
    assert(vol64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 1);
    assert(vol64 == new64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(ompi_atomic_cmpset_acq_64(&vol64, old64, new64) == 0);
    assert(vol64 == 42);

    vol64 = 42, old64 = 42, new64 = 50;
    assert(ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 1);
    assert(vol64 == new64);

    vol64 = 42, old64 = 420, new64 = 50;
    assert(ompi_atomic_cmpset_rel_64(&vol64, old64, new64) == 0);
    assert(vol64 == 42);

    return 0;
#else
    return 77;
#endif
}
