/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "util/output.h"
#include "mca/mpool/vapi/mpool_vapi.h"
#include "mca/common/vapi/vapi_mem_reg.h"


/* 
 *  Initializes the mpool module.
 */ 
void mca_mpool_vapi_module_init(mca_mpool_vapi_module_t* mpool)
{
  mpool->super.mpool_component = &mca_mpool_vapi_component.super; 
  mpool->super.mpool_base = NULL; /* no base .. */ 
  mpool->super.mpool_alloc = mca_mpool_vapi_alloc; 
  mpool->super.mpool_realloc = mca_mpool_vapi_realloc; 
  mpool->super.mpool_free = mca_mpool_vapi_free; 
  mpool->super.mpool_register = NULL; 
  mpool->super.mpool_deregister = NULL; 
  mpool->super.mpool_finalize = NULL; 
}


/**
  * allocate function 
  */
void* mca_mpool_vapi_alloc(mca_mpool_base_module_t* mpool, size_t size, size_t align, void** user_out)
{
  mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
  return mpool_vapi->vapi_allocator->alc_alloc(mpool_vapi->vapi_allocator, size, align, user_out);
}

/**
  * realloc function 
  */
void* mca_mpool_vapi_realloc(mca_mpool_base_module_t* mpool, void* addr, size_t size, void** user_out)
{
  mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
  return mpool_vapi->vapi_allocator->alc_realloc(mpool_vapi->vapi_allocator, addr, size, user_out);
}

/**
  * free function 
  */
void mca_mpool_vapi_free(mca_mpool_base_module_t* mpool, void * addr)
{
  mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
  mpool_vapi->vapi_allocator->alc_free(mpool_vapi->vapi_allocator, addr);
}



















