/*
 * $HEADER$
 */

#ifndef MEM_GLOBALS_H
#define MEM_GLOBALS_H

#include "mem/mem_pool.h"

/* shared memory pool for use before fork.
    should be initialized during prefork init.
*/
extern ompi_fixed_mpool_t    ompi_shmem_pools;

extern ompi_fixed_mpool_t    ompi_per_proc_shmem_pools;

int ompi_setup_per_proc_shmem_pools(int npools);

#endif


