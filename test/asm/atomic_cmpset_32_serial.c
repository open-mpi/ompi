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

#include <assert.h>

#include "include/sys/atomic.h"

volatile int32_t vol32;
int32_t val32;
int32_t old32;
int32_t new32;

int
main(int argc, char *argv[])
{
#if ! OMPI_HAVE_ATOMIC_CMPSET_32
    return 77;
#else

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_32(&vol32, old32, new32) == 1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_32(&vol32, old32, new32) ==  0);
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_acq_32(&vol32, old32, new32) == 0);
    assert(vol32 == 42);

    vol32 = 42, old32 = 42, new32 = 50;
    assert(ompi_atomic_cmpset_rel_32(&vol32, old32, new32) ==  1);
    assert(vol32 == new32);

    vol32 = 42, old32 = 420, new32 = 50;
    assert(ompi_atomic_cmpset_rel_32(&vol32, old32, new32) == 0);
    assert(vol32 == 42);

    return 0;
#endif
}
