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
#include "mpool_gm.h"
#include "mca/rcache/rcache.h"
#include "mca/rcache/base/base.h"


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
    mpool->super.rcache = 
        mca_rcache_base_module_create(mca_mpool_gm_component.rcache_name);
}


/**
  * allocate function 
  */
void* mca_mpool_gm_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    void *addr = malloc(size+align);
    if(NULL == addr)
        return NULL;
    if(OMPI_SUCCESS != mca_mpool_gm_register(mpool,addr,size+align, flags, registration)) {
        free(addr);
        return NULL;
    }
    return addr;
}


/* 
 * register memory 
 */ 
int mca_mpool_gm_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_gm_module_t * gm_mpool = (mca_mpool_gm_module_t*) mpool; 
    mca_mpool_base_registration_t* reg = OBJ_NEW(mca_mpool_base_registration_t);
    int rc;

    if(NULL == reg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    if((rc = gm_register_memory(gm_mpool->port, addr, size)) != GM_SUCCESS) {
        opal_output(0, "[%s:%d] error(%d) registering gm memory\n", __FILE__, __LINE__, rc);
        OBJ_RELEASE(reg);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    reg->mpool = mpool;
    reg->base = addr; 
    reg->bound = reg->base + size - 1; 
    
    if(flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_insert(mpool->rcache, 
                                     (mca_mpool_base_registration_t*) reg, 
                                     flags); 
    }
    
                                     
    reg->flags = flags; 
    mca_mpool_gm_retain(mpool, 
                        reg); 
    *registration = reg;
    
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_gm_deregister(mca_mpool_base_module_t* mpool, 
                              mca_mpool_base_registration_t* reg)
{
    mca_mpool_gm_module_t * mpool_gm = (mca_mpool_gm_module_t*) mpool; 
    int rc = gm_deregister_memory(
                                  mpool_gm->port,
                                  reg->base,
                                  reg->bound - reg->base + 1);
    if(GM_SUCCESS != rc) { 
        opal_output(0, "[%s:%d] error(%d) deregistering gm memory\n", __FILE__, __LINE__, rc); 
        return OMPI_ERROR; 
    }
    if(reg->flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_delete(mpool->rcache, 
                                     reg, 
                                     reg->flags); 
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
    void *new_addr = mca_mpool_gm_alloc(mpool,size,0, (*registration)->flags, registration);
    if(new_addr == NULL) {
        return NULL;
    }
    memcpy(new_addr,addr,size);
    mca_mpool_gm_free(mpool,addr,*registration);
    return new_addr;
}

/**
  * free function 
  */
void mca_mpool_gm_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    mpool->mpool_deregister(mpool, registration);
    free(addr);
}

int mca_mpool_gm_find(
                         struct mca_mpool_base_module_t* mpool, 
                         void* addr, 
                         size_t size, 
                         ompi_pointer_array_t *regs,
                         uint32_t *cnt
                         ){

    return mpool->rcache->rcache_find(mpool->rcache, 
                                      addr, 
                                      size, 
                                      regs, 
                                      cnt); 
    
}
 
int mca_mpool_gm_release(
                            struct mca_mpool_base_module_t* mpool, 
                            mca_mpool_base_registration_t* registration
                            ){
    if(0 == OPAL_THREAD_ADD32(&registration->ref_count, -1)) {
        mpool->mpool_deregister(mpool, registration);
    }
    return OMPI_SUCCESS; 
}

int mca_mpool_gm_retain(struct mca_mpool_base_module_t* mpool, 
                           mca_mpool_base_registration_t* registration
                           ){
    OPAL_THREAD_ADD32(&registration->ref_count, 1); 
    return OMPI_SUCCESS; 
}
























