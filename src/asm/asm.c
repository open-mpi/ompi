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

#include "ompi_config.h"

#include "include/sys/atomic.h"
#include "include/sys/architecture.h"

#if OMPI_ASSEMBLY_ARCH == OMPI_SPARC32

#if OMPI_WANT_SMP_LOCKS

#define LOCKS_TABLE_SIZE 8
/* make sure to get into reasonably useful bits (so shift at least 5) */
#define FIND_LOCK(addr) (&(locks_table[(((unsigned long) addr) >> 8) &  \
                                       (LOCKS_TABLE_SIZE - 1)]))

/* have to fix if you change LOCKS_TABLE_SIZE */
static ompi_lock_t locks_table[LOCKS_TABLE_SIZE] = {
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED,
    OMPI_ATOMIC_UNLOCKED
};

# else /* OMPI_WANT_SMP_LOCKS */

#define LOCKS_TABLE_SIZE 1
#define FIND_LOCK(addr) (&(locks_table[0]))

static ompi_lock_t locks_table[1] = { OMPI_ATOMIC_UNLOCKED };

#endif /* OMPI_WANT_SMP_LOCKS */


int32_t
ompi_atomic_sub_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    ompi_atomic_lock(FIND_LOCK(addr));

    ret = (*addr += delta);

    ompi_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


int32_t
ompi_atomic_sub_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    ompi_atomic_lock(FIND_LOCK(addr));

    ret = (*addr -= delta);

    ompi_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


#endif /* OMPI_ASSEMBLY_ARCH == OMPI_SPARC32 */
