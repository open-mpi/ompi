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
#include <errno.h> 
#include <string.h> 
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
    /* void* addr_malloc = (void*)malloc((*size) + mca_mpool_openib_component.page_size);  */
    /* void* addr = (void*)  ALIGN_ADDR(addr_malloc, mca_mpool_openib_component.page_size_log);  */
   
    void* addr_malloc = (void*)memalign(mca_mpool_openib_component.page_size, size); 
    void* addr = addr_malloc; 

    if(OMPI_SUCCESS !=  mpool->mpool_register(mpool, addr, size, registration)) { 
        free(addr_malloc);
        return NULL; 
    } 
    return addr;
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
                              IBV_ACCESS_REMOTE_WRITE
                              /* IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE */ 
                              ); 
   
    
    if(NULL == vapi_reg->mr){ 
        opal_output(0, "%s: error registering openib memory of size %lu errno says %s\n", __func__, size, strerror(errno)); 
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
        opal_output(0, "%s: error unpinning openib memory errno says %s\n", __func__, strerror(errno)); 
        return OMPI_ERROR; 
    }
    
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
    mca_mpool_base_registration_t* old_reg  = *registration; 
    void* new_mem = mpool->mpool_alloc(mpool, size, 0, registration); 
    memcpy(new_mem, addr, old_reg->bound - old_reg->base); 
    mpool->mpool_free(mpool, addr, &old_reg); 
    return new_mem; 

}

/**
  * free function 
  */
void mca_mpool_openib_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    
    mpool->mpool_deregister(mpool, addr, 0, registration); 
    free(registration->alloc_base); 
    
    
}





















