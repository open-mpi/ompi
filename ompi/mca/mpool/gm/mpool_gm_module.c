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
#include "mpool_gm.h"


/* 
 *  Initializes the mpool module.
 */ 
void mca_mpool_gm_module_init(mca_mpool_gm_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_gm_component.super; 
    mpool->super.mpool_base = NULL; /* no base .. */ 
    mpool->super.mpool_alloc = mca_mpool_gm_alloc; 
    mpool->super.mpool_realloc = mca_mpool_gm_realloc; 
    mpool->super.mpool_free = mca_mpool_gm_free; 
    mpool->super.mpool_register = mca_mpool_gm_register; 
    mpool->super.mpool_deregister = mca_mpool_gm_deregister; 
    mpool->super.mpool_finalize = NULL; 
}


/**
  * allocate function 
  */
void* mca_mpool_gm_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_gm_module_t* mpool_gm = (mca_mpool_gm_module_t*)mpool; 
    return mpool_gm->gm_allocator->alc_alloc(mpool_gm->gm_allocator, size, align, registration);
}


/* 
 * register memory 
 */ 
int mca_mpool_gm_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_gm_module_t * gm_mpool = (mca_mpool_gm_module_t*) mpool; 
    mca_mpool_base_registration_t* reg = OBJ_NEW(mca_mpool_base_registration_t);
    int rc;

    if(NULL == reg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    if((rc = gm_register_memory(gm_mpool->gm_port, addr, size)) != GM_SUCCESS) {
        ompi_output(0, "[%s:%d] error(%d) registering gm memory\n", __FILE__, __LINE__, rc);
        OBJ_RELEASE(reg);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    reg->mpool = mpool;
    reg->base = addr; 
    reg->bound = reg->base + size - 1; 
    *registration = reg;
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_gm_deregister(mca_mpool_base_module_t* mpool, void *addr, size_t size, 
                              mca_mpool_base_registration_t* reg)
{
    mca_mpool_gm_module_t * mpool_gm = (mca_mpool_gm_module_t*) mpool; 
    int rc = gm_deregister_memory(
        mpool_gm->gm_port,
        addr,
        size);
    if(GM_SUCCESS != rc) { 
        ompi_output(0, "[%s:%d] error(%d) deregistering gm memory\n", __FILE__, __LINE__, rc); 
        return OMPI_ERROR; 
    }
    return OMPI_SUCCESS; 
}

/**
  * realloc function 
  */
void* mca_mpool_gm_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_gm_module_t* mpool_gm = (mca_mpool_gm_module_t*)mpool; 
    return mpool_gm->gm_allocator->alc_realloc( mpool_gm->gm_allocator, addr, size, registration);
}

/**
  * free function 
  */
void mca_mpool_gm_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    
    mca_mpool_gm_module_t* mpool_gm = (mca_mpool_gm_module_t*)mpool; 
    mpool_gm->super.mpool_deregister(mpool, addr, 0, registration); 
    mpool_gm->gm_allocator->alc_free(mpool_gm->gm_allocator, addr);
}





















