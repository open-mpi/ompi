/*
 * $HEADER$
 */

#include <string.h>
#include "util/output.h"
#include "mca/mpool/sm/mpool_sm.h"


mca_mpool_t mca_mpool_sm = {
    &mca_mpool_sm_module.super,
    mca_mpool_sm_alloc,
    mca_mpool_sm_realloc,
    mca_mpool_sm_free,
    NULL,
    NULL
};


/**
  * allocate function 
  */
void* mca_mpool_sm_alloc(size_t size, size_t align)
{
    return mca_mpool_sm_module.sm_allocator->alc_alloc(
        mca_mpool_sm_module.sm_allocator, size, align);
}
                                                                                                                         
/**
  * realloc function 
  */
void* mca_mpool_sm_realloc(void* addr, size_t size)
{
    return mca_mpool_sm_module.sm_allocator->alc_realloc(
        mca_mpool_sm_module.sm_allocator, addr, size);
}
                                                                                                                         
/**
  * free function 
  */
void mca_mpool_sm_free(void * addr)
{
    mca_mpool_sm_module.sm_allocator->alc_free(
        mca_mpool_sm_module.sm_allocator, addr);
}
                                                                                                                         
