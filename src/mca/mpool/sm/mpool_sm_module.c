/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "util/output.h"
#include "mca/mpool/sm/mpool_sm.h"
#include "mca/common/sm/common_sm_mmap.h"


mca_mpool_base_module_t mca_mpool_sm_module = {
    &mca_mpool_sm_component.super,
    mca_mpool_sm_base,
    mca_mpool_sm_alloc,
    mca_mpool_sm_realloc,
    mca_mpool_sm_free,
    NULL,
    NULL
};


/*
 * base address of shared memory mapping
 */
void* mca_mpool_sm_base(void)
{
    return (mca_common_sm_mmap != NULL) ? mca_common_sm_mmap->map_addr : NULL;
}

/**
  * allocate function 
  */
void* mca_mpool_sm_alloc(size_t size, size_t align)
{
    return mca_mpool_sm_component.sm_allocator->alc_alloc(mca_mpool_sm_component.sm_allocator, size, align);
}

/**
  * realloc function 
  */
void* mca_mpool_sm_realloc(void* addr, size_t size)
{
    return mca_mpool_sm_component.sm_allocator->alc_realloc(mca_mpool_sm_component.sm_allocator, addr, size);
}

/**
  * free function 
  */
void mca_mpool_sm_free(void * addr)
{
    mca_mpool_sm_component.sm_allocator->alc_free(mca_mpool_sm_component.sm_allocator, addr);
}
