/*
 * $HEADER$
 */

#include <unistd.h>

#include "lam/constants.h"
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
    
    OBJ_CONSTRUCT(&lam_per_proc_shmem_pools, lam_fixed_mpool_t);
    lam_fmp_construct_with(&lam_per_proc_shmem_pools,
                                  initial_alloc, min_alloc_size,
                                  npools, n_array_elts_add, 1);
    
    return ret;
    
}

