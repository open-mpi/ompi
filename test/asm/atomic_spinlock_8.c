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
#include "atomic_test.h"

int
main(int argc, char *argv[])
{
    int ret = 77;
    ompi_lock_t lock;

    ompi_atomic_init(&lock, OMPI_ATOMIC_UNLOCKED);
    ret = atomic_spinlock_test_th(&lock, TEST_REPS, 0, 8);

    return ret;
}
