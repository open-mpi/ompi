/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
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
#include "opal/memoryhooks/memory.h"
#include "ompi/mca/mpool/mvapi/mpool_mvapi.h"
#include <vapi.h>
#include <vapi_types.h> 
#include <vapi_common.h> 
#include "ompi/mca/rcache/rcache.h" 
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

extern uint32_t mca_mpool_base_page_size;
extern uint32_t mca_mpool_base_page_size_log; 


/*
 *  Initializes the mpool module.
 */ 
void mca_mpool_mvapi_module_init(mca_mpool_mvapi_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_mvapi_component.super; 
    mpool->super.mpool_base = NULL; /* no base .. */ 
    mpool->super.mpool_alloc = mca_mpool_mvapi_alloc; 
    mpool->super.mpool_realloc = mca_mpool_mvapi_realloc; 
    mpool->super.mpool_free = mca_mpool_mvapi_free; 
    mpool->super.mpool_register = mca_mpool_mvapi_register; 
    mpool->super.mpool_deregister = mca_mpool_mvapi_deregister; 
    mpool->super.mpool_find = mca_mpool_mvapi_find; 
    mpool->super.mpool_retain = mca_mpool_mvapi_retain;
    mpool->super.mpool_release = mca_mpool_mvapi_release; 
    mpool->super.mpool_finalize = NULL; 
    mpool->super.rcache = 
        mca_rcache_base_module_create(mca_mpool_mvapi_component.rcache_name);
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM; 
        
    OBJ_CONSTRUCT(&mpool->reg_list, ompi_free_list_t); 
    ompi_free_list_init(&mpool->reg_list, sizeof(mca_mpool_mvapi_registration_t),
                        OBJ_CLASS(mca_mpool_mvapi_registration_t), 0, -1, 32, NULL); 
    
}


/**
  * allocate function 
  */
void* mca_mpool_mvapi_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align,
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    void* addr_malloc = (void*)malloc(size + mca_mpool_base_page_size); 
    void* addr = (void*)  up_align_addr(addr_malloc, mca_mpool_base_page_size_log); 
    if(OMPI_SUCCESS !=  mpool->mpool_register(mpool, addr, size, flags, registration)) { 
        free(addr_malloc);
        return NULL; 
    }
    (*registration)->alloc_base = addr_malloc; 
    return addr; 
}


/* 
 * register memory 
 */ 
int mca_mpool_mvapi_register(
    mca_mpool_base_module_t* mpool, 
    void *addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    
    mca_mpool_mvapi_module_t * mpool_module = (mca_mpool_mvapi_module_t*) mpool; 
    mca_mpool_mvapi_registration_t * vapi_reg; 
    VAPI_mrw_t mr_in, mr_out;
    VAPI_ret_t ret; 
    ompi_free_list_item_t *item;
    int rc;
    
        
    assert(size > 0); 
    memset(&mr_in, 0, sizeof(VAPI_mrw_t)); 
    memset(&mr_out, 0, sizeof(VAPI_mrw_t)); 
    
    OMPI_FREE_LIST_GET(&mpool_module->reg_list, item, rc); 
    if(OMPI_SUCCESS != rc) { 
        return rc; 
    }
    vapi_reg = (mca_mpool_mvapi_registration_t*) item;
    
    vapi_reg->base_reg.mpool = mpool;
    vapi_reg->base_reg.flags = flags; 
    vapi_reg->hndl = VAPI_INVAL_HNDL; 
    
    
    *registration = &vapi_reg->base_reg;
    mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE | VAPI_EN_REMOTE_READ;
    mr_in.l_key = 0;
    mr_in.r_key = 0;
    mr_in.pd_hndl = mpool_module->hca_pd.pd_tag;
    vapi_reg->base_reg.base = down_align_addr(addr, mca_mpool_base_page_size_log); 
    vapi_reg->base_reg.bound = up_align_addr((void*) ((unsigned long) addr + size - 1) 
                                             , mca_mpool_base_page_size_log);
    
    mr_in.size =  vapi_reg->base_reg.bound - vapi_reg->base_reg.base + 1;
    mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) vapi_reg->base_reg.base;
    mr_in.type = VAPI_MR;
    
    /* printf("registering addr %p size %d base %p bound %p\n",  */
/*            addr, size, vapi_reg->base_reg.base, vapi_reg->base_reg.bound);  */

    ret = VAPI_register_mr(
                           mpool_module->hca_pd.hca, 
                           &mr_in, 
                           &vapi_reg->hndl, 
                           &mr_out
                           ); 
    
    if(VAPI_OK != ret){ 
        opal_output(0, "error registering memory of size %d: %s ", size, VAPI_strerror(ret)); 
        OMPI_FREE_LIST_RETURN(&mpool_module->reg_list, item);
        return OMPI_ERROR; 
    }
    
    vapi_reg->l_key = mr_out.l_key; 
    vapi_reg->r_key = mr_out.r_key; 
    
    assert(vapi_reg->base_reg.bound - vapi_reg->base_reg.base >= 0);
    
    if(flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_insert(mpool->rcache, 
                                     (mca_mpool_base_registration_t*) vapi_reg, 
                                     flags); 
    }
    OPAL_THREAD_ADD32(&vapi_reg->base_reg.ref_count, 1); 
    return OMPI_SUCCESS; 
}


/* 
 * Remove the registration from the rcache immediately and
 * deregister the memory when the reference count goes to zero.
 */ 
int mca_mpool_mvapi_deregister(mca_mpool_base_module_t* mpool, 
                              mca_mpool_base_registration_t* registration)
{
    if(registration->flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_delete(mpool->rcache, 
                                     registration, 
                                     registration->flags); 
        registration->flags = 0;
    }
    return mca_mpool_mvapi_release(mpool, registration); 
}

/**
  * realloc function 
  */
void* mca_mpool_mvapi_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_base_registration_t* old_reg  = *registration; 
    void* new_mem = mpool->mpool_alloc(mpool, size, 0, old_reg->flags, registration); 
    memcpy(new_mem, addr, old_reg->bound - old_reg->base); 
    mpool->mpool_free(mpool, addr, old_reg); 
    return new_mem; 
}

/**
  * free function 
  */
void mca_mpool_mvapi_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    mpool->mpool_deregister(mpool, registration); 
    free(registration->alloc_base); 
}


int mca_mpool_mvapi_find(
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
 
int mca_mpool_mvapi_release(
    struct mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* registration
    )
{
    if(0 >= OPAL_THREAD_ADD32(&registration->ref_count, -1)) {
        VAPI_ret_t ret; 
        mca_mpool_mvapi_module_t * mpool_mvapi = (mca_mpool_mvapi_module_t*) mpool; 
        mca_mpool_mvapi_registration_t * vapi_reg; 
        vapi_reg = (mca_mpool_mvapi_registration_t*) registration; 
        ret = VAPI_deregister_mr(
            mpool_mvapi->hca_pd.hca, 
            vapi_reg->hndl 
        ); 
    
        if(VAPI_OK != ret){ 
            opal_output(0, "%s: error unpinning vapi memory\n", __func__); 
            return OMPI_ERROR; 
        }
        OMPI_FREE_LIST_RETURN(&mpool_mvapi->reg_list, (ompi_free_list_item_t*) vapi_reg);
    }
    return OMPI_SUCCESS; 
}

int mca_mpool_mvapi_retain(struct mca_mpool_base_module_t* mpool, 
                           mca_mpool_base_registration_t* registration
                           )
{
    OPAL_THREAD_ADD32(&registration->ref_count, 1); 
    return OMPI_SUCCESS; 
}
























