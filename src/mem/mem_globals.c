/*
 * $HEADER$
 */

#include <unistd.h>

#include "include/constants.h"
#include "class/ompi_object.h"
#include "mem/mem_globals.h"

ompi_fixed_mpool_t    ompi_shmem_pools;
ompi_fixed_mpool_t    ompi_per_proc_shmem_pools;

int ompi_setup_per_proc_shmem_pools(int npools)
{
    int ret = OMPI_SUCCESS;
    ssize_t initial_alloc = 0;
    ssize_t min_alloc_size = 4 * getpagesize();
    int n_array_elts_add = 10;
    
    OBJ_CONSTRUCT(&ompi_per_proc_shmem_pools, ompi_fixed_mpool_t);
    ompi_fmp_construct_with(&ompi_per_proc_shmem_pools,
                                  initial_alloc, min_alloc_size,
                                  npools, n_array_elts_add, 1);
    
    return ret;
    
}

