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
#include "opal/util/output.h"
#include "mca/mpool/openib/mpool_openib.h"
#include <infiniband/verbs.h> 

/* 
 *  Initializes the mpool module.
 */ 
void mca_mpool_openib_module_init(mca_mpool_openib_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_openib_component.super; 
    mpool->super.mpool_base = NULL; /* no base .. */ 
    mpool->super.mpool_alloc = mca_mpool_openib_alloc; 
    mpool->super.mpool_realloc = mca_mpool_openib_realloc; 
    mpool->super.mpool_free = mca_mpool_openib_free; 
    mpool->super.mpool_register = mca_mpool_openib_register; 
    mpool->super.mpool_deregister = mca_mpool_openib_deregister; 
    mpool->super.mpool_finalize = NULL; 
}


/**
  * allocate function 
  */
void* mca_mpool_openib_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_openib_module_t* mpool_openib = (mca_mpool_openib_module_t*)mpool; 
    return mpool_openib->vapi_allocator->alc_alloc(mpool_openib->vapi_allocator, size, align, registration);
}


/* 
 * register memory 
 */ 
int mca_mpool_openib_register(mca_mpool_base_module_t* mpool, 
                              void *addr, 
                              size_t size, 
                              mca_mpool_base_registration_t** registration){
    mca_mpool_openib_module_t * mpool_module = (mca_mpool_openib_module_t*) mpool; 
    mca_mpool_openib_registration_t * vapi_reg; 
        
    *registration = (mca_mpool_base_registration_t*) OBJ_NEW(mca_mpool_openib_registration_t);    /* (void*) malloc(sizeof(mca_mpool_base_registration_t));  */
    vapi_reg = (mca_mpool_openib_registration_t*) *registration; 
    vapi_reg->base_reg.mpool = mpool;
        
    
    vapi_reg->mr = ibv_reg_mr(
                              mpool_module->resources.ib_pd, 
                              addr, 
                              size, 
                              IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE
                              ); 
   
    
    if(NULL == vapi_reg->mr){ 
        opal_output(0, "%s: error registering openib memory\n", __func__); 
        return OMPI_ERROR; 
    }
    
    vapi_reg->base_reg.base = addr; 
    vapi_reg->base_reg.bound = (void*) ((char*) addr + size - 1); 
    
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_openib_deregister(mca_mpool_base_module_t* mpool, void *addr, size_t size, 
                              mca_mpool_base_registration_t* registration){
    
    mca_mpool_openib_module_t * mpool_openib = (mca_mpool_openib_module_t*) mpool; 
    mca_mpool_openib_registration_t * openib_reg; 
    openib_reg = (mca_mpool_openib_registration_t*) registration; 
    if(! ibv_dereg_mr(openib_reg->mr)){   
        opal_output(0, "%s: error unpinning openib memory\n", __func__); 
        return OMPI_ERROR; 
    }
    free(registration); 
    return OMPI_SUCCESS; 
}

/**
  * realloc function 
  */
void* mca_mpool_openib_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_openib_module_t* mpool_openib = (mca_mpool_openib_module_t*)mpool; 
    return mpool_openib->vapi_allocator->alc_realloc( mpool_openib->vapi_allocator, addr, size, registration);
}

/**
  * free function 
  */
void mca_mpool_openib_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    
    mca_mpool_openib_module_t* mpool_openib = (mca_mpool_openib_module_t*)mpool; 
    mpool_openib->super.mpool_deregister(mpool, addr, 0, registration); 
    mpool_openib->vapi_allocator->alc_free(mpool_openib->vapi_allocator, addr);
    
    
}





















