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
#include "mca/mpool/mvapi/mpool_openib.h"


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
    VAPI_mrw_t mr_in, mr_out;
    
    VAPI_ret_t ret; 
    
    memset(&mr_in, 0, sizeof(VAPI_mrw_t)); 
    memset(&mr_out, 0, sizeof(VAPI_mrw_t)); 
    
    *registration = (mca_mpool_base_registration_t*) OBJ_NEW(mca_mpool_openib_registration_t);    /* (void*) malloc(sizeof(mca_mpool_base_registration_t));  */
    vapi_reg = (mca_mpool_openib_registration_t*) *registration; 
    vapi_reg->base_reg.mpool = mpool;
    
    
    vapi_reg->hndl = VAPI_INVAL_HNDL; 
    
    
    mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
    mr_in.l_key = 0;
    mr_in.r_key = 0;
    mr_in.pd_hndl = mpool_module->hca_pd.pd_tag;
    mr_in.size = size;
    mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) addr;
    mr_in.type = VAPI_MR;
    

    ret = VAPI_register_mr(
                           mpool_module->hca_pd.hca, 
                           &mr_in, 
                           &vapi_reg->hndl, 
                           &mr_out
                           ); 
    
    if(VAPI_OK != ret){ 
        ompi_output(0, "error pinning vapi memory\n"); 
        return OMPI_ERROR; 
    }
    
    vapi_reg->l_key = mr_out.l_key; 
    vapi_reg->r_key = mr_out.r_key; 
    vapi_reg->base_reg.base = addr; 
    vapi_reg->base_reg.bound = (void*) ((char*) addr + size - 1); 
    
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_openib_deregister(mca_mpool_base_module_t* mpool, void *addr, size_t size, 
                              mca_mpool_base_registration_t* registration){
    
    VAPI_ret_t ret; 
    mca_mpool_openib_module_t * mpool_openib = (mca_mpool_openib_module_t*) mpool; 
    mca_mpool_openib_registration_t * vapi_reg; 
    vapi_reg = (mca_mpool_openib_registration_t*) registration; 
    ret = VAPI_deregister_mr(
                             mpool_openib->hca_pd.hca, 
                             vapi_reg->hndl 
                             ); 
    
    if(VAPI_OK != ret){ 
        ompi_output(0, "%s: error unpinning vapi memory\n", __func__); 
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





















