/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "src/atomics/sys/atomic.h"
#include "src/atomics/sys/architecture.h"

#if PMIX_ASSEMBLY_ARCH == PMIX_SPARC

#define LOCKS_TABLE_SIZE 8
/* make sure to get into reasonably useful bits (so shift at least 5) */
#define FIND_LOCK(addr) (&(locks_table[(((unsigned long) addr) >> 8) &  \
                                       (LOCKS_TABLE_SIZE - 1)]))

/* have to fix if you change LOCKS_TABLE_SIZE */
static pmix_atomic_lock_t locks_table[LOCKS_TABLE_SIZE] = {
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } },
    { { PMIX_ATOMIC_UNLOCKED } }
};


int32_t
pmix_atomic_add_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    pmix_atomic_lock(FIND_LOCK(addr));

    ret = (*addr += delta);

    pmix_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


int32_t
pmix_atomic_sub_32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    pmix_atomic_lock(FIND_LOCK(addr));

    ret = (*addr -= delta);

    pmix_atomic_unlock(FIND_LOCK(addr));

    return ret;
}


#endif /* PMIX_ASSEMBLY_ARCH == PMIX_SPARC32 */
