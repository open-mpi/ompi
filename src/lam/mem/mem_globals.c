/*
 * $HEADER$
 */

#include <unistd.h>
#include "include/lam_constants.h"
#include "lam/lfc/object.h"
#include "lam/mem/mem_globals.h"

lam_fixed_mpool_t    lam_shmem_pools;
lam_fixed_mpool_t    lam_per_proc_shmem_pools;

int lam_setup_per_proc_shmem_pools(int npools)
{
    int ret = LAM_SUCCESS;
    ssize_t initial_alloc = 0;
    ssize_t min_alloc_size = 4 * getpagesize();
    int n_array_elts_add = 10;
    
    STATIC_INIT(lam_per_proc_shmem_pools, &fixed_mem_pool_cls);
    lam_fmp_init_with(&lam_per_proc_shmem_pools,
                                  initial_alloc, min_alloc_size,
                                  npools, n_array_elts_add, 1);
    
    return ret;
    
}

