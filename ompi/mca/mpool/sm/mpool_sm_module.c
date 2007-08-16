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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "opal/util/output.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"


/* 
 *  Initializes the mpool module.
 */ 
void mca_mpool_sm_module_init(mca_mpool_sm_module_t* mpool)
{
  mpool->super.mpool_component = &mca_mpool_sm_component.super; 
  mpool->super.mpool_base = mca_mpool_sm_base; 
  mpool->super.mpool_alloc = mca_mpool_sm_alloc; 
  mpool->super.mpool_realloc = mca_mpool_sm_realloc; 
  mpool->super.mpool_free = mca_mpool_sm_free; 
  mpool->super.mpool_find = NULL; 
  mpool->super.mpool_register = NULL; 
  mpool->super.mpool_deregister = NULL; 
  mpool->super.mpool_release_memory = NULL;
  mpool->super.mpool_finalize = NULL; 
  mpool->super.flags = 0;
}

/*
 * base address of shared memory mapping
 */
void* mca_mpool_sm_base(mca_mpool_base_module_t* mpool)
{
    return (mca_common_sm_mmap != NULL) ? mca_common_sm_mmap->map_addr : NULL;
}

/**
  * allocate function 
  */
void* mca_mpool_sm_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags,
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool; 
    return mpool_sm->sm_allocator->alc_alloc(mpool_sm->sm_allocator, size, align, registration);
}

/**
  * realloc function 
  */
void* mca_mpool_sm_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool; 
    return mpool_sm->sm_allocator->alc_realloc(mpool_sm->sm_allocator, addr, size, registration);
}

/**
  * free function 
  */
void mca_mpool_sm_free(mca_mpool_base_module_t* mpool, void * addr, 
                       mca_mpool_base_registration_t* registration)
{
    mca_mpool_sm_module_t* mpool_sm = (mca_mpool_sm_module_t*)mpool; 
    mpool_sm->sm_allocator->alc_free(mpool_sm->sm_allocator, addr);
}
