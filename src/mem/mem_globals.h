/*
 * $HEADER$
 */

#ifndef MEM_GLOBALS_H
#define MEM_GLOBALS_H

#include "mem/mem_pool.h"

/* shared memory pool for use before fork.
    should be initialized during prefork init.
*/
extern lam_fixed_mpool_t    lam_shmem_pools;

extern lam_fixed_mpool_t    lam_per_proc_shmem_pools;

int lam_setup_per_proc_shmem_pools(int npools);

#endif


