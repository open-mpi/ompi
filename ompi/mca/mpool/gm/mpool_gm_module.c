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
#include "mpool_gm.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"

extern uint32_t mca_mpool_base_page_size;
extern uint32_t mca_mpool_base_page_size_log; 
uint64_t mca_mpool_gm_mem_registered; 

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
    mpool->super.mpool_find = mca_mpool_gm_find; 
    mpool->super.mpool_retain = mca_mpool_gm_retain;
    mpool->super.mpool_release = mca_mpool_gm_release; 
    mpool->super.mpool_finalize = NULL; 
    mpool->super.rcache = 
        mca_rcache_base_module_create(mca_mpool_gm_component.rcache_name);
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM; 
    mca_mpool_gm_mem_registered = 0;
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
    reg->mpool = mpool;
    reg->base = down_align_addr(addr, mca_mpool_base_page_size_log); 
    reg->flags = flags; 
    reg->bound = up_align_addr((void*) ((unsigned long) addr + size -1)
                               , mca_mpool_base_page_size_log);
    

    if((rc = gm_register_memory(gm_mpool->port, reg->base, reg->bound - reg->base + 1)) != GM_SUCCESS) {
        opal_output(0, "[%s:%d] error(%d) registering gm memory\n", __FILE__, __LINE__, rc);
        assert(0);
        OBJ_RELEASE(reg);
        
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_mpool_gm_mem_registered += reg->bound - reg->base + 1;

    
    OPAL_THREAD_ADD32(&reg->ref_count,1);

    if(flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_insert(mpool->rcache, 
                                     (mca_mpool_base_registration_t*) reg, 
                                     flags); 
    }
    *registration = reg;
    /* opal_output(0,"registered memory from %p to %p\n", reg->base, reg->bound); */
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_gm_deregister(mca_mpool_base_module_t* mpool, 
                              mca_mpool_base_registration_t* reg)
{
    int rc;
    if(reg->flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_delete(mpool->rcache, 
                                     reg, 
                                     reg->flags); 
    }
    if((rc = mca_mpool_gm_release(mpool, reg)) != GM_SUCCESS) { 
        opal_output(0, "[%s:%d] error(%d) deregistering gm memory\n", __FILE__, __LINE__, rc);
        assert(0);
        return OMPI_ERR_OUT_OF_RESOURCE;

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
    if(registration){ 
        mpool->mpool_deregister(mpool, registration);
    }
    free(addr);
}

int mca_mpool_gm_find(
    struct mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    ompi_pointer_array_t *regs,
    uint32_t *cnt)
{
    return mpool->rcache->rcache_find(mpool->rcache, 
                                      addr, 
                                      size, 
                                      regs, 
                                      cnt); 
}
 
int mca_mpool_gm_release(
    struct mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* reg)
{
    mca_mpool_gm_module_t * mpool_gm = (mca_mpool_gm_module_t*) mpool; 
    if(0 == OPAL_THREAD_ADD32(&reg->ref_count, -1)) {
        int rc = gm_deregister_memory(
                                  mpool_gm->port,
                                  reg->base,
                                  reg->bound - reg->base + 1);
        if(GM_SUCCESS != rc) { 
            opal_output(0, "[%s:%d] error(%d) deregistering gm memory\n", __FILE__, __LINE__, rc); 
            return OMPI_ERROR; 
        }
        /* opal_output(0,"deregistering gm memory\n"); */
        mca_mpool_gm_mem_registered -= reg->bound - reg->base + 1;
        OBJ_RELEASE(reg);
    }
    else { 
        /* opal_output(0, "release says ref_count is %d\n", reg->ref_count);  */
    }
    
    return OMPI_SUCCESS; 
}

int mca_mpool_gm_retain(
    struct mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* registration)
{
    OPAL_THREAD_ADD32(&registration->ref_count, 1); 
    return OMPI_SUCCESS; 
}
























