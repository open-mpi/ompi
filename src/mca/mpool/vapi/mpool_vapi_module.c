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
    mpool->super.mpool_register = mca_mpool_vapi_register; 
    mpool->super.mpool_deregister = mca_mpool_vapi_deregister; 
    mpool->super.mpool_finalize = NULL; 
}


/**
  * allocate function 
  */
void* mca_mpool_vapi_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    struct mca_bmi_base_registration_t** registration)
{
    mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
    return mpool_vapi->vapi_allocator->alc_alloc(mpool_vapi->vapi_allocator, size, align, registration);
}


/* 
 * register memory 
 */ 
int mca_mpool_vapi_register(mca_mpool_base_module_t* mpool, void *addr, size_t size, 
    struct mca_bmi_base_registration_t** registration){
    
    mca_mpool_vapi_module_t * mpool_module = (mca_mpool_vapi_module_t*) mpool; 
    VAPI_mrw_t mr_in, mr_out;
  
    VAPI_ret_t ret; 
    mca_bmi_base_registration_t* mem_hndl; 
    memset(&mr_in, 0, sizeof(VAPI_mrw_t)); 
    memset(&mr_out, 0, sizeof(VAPI_mrw_t)); 
    

    *registration = (void*) malloc(sizeof(mca_bmi_base_registration_t)); 
    mem_hndl = (mca_bmi_base_registration_t*) *registration;  
    memset(mem_hndl, 0, sizeof(mca_bmi_base_registration_t*)); 
    mem_hndl->hndl = VAPI_INVAL_HNDL; 
    
    
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
                           &mem_hndl->hndl, 
                           &mr_out
                           ); 
    
    if(VAPI_OK != ret){ 
        ompi_output(0, "error pinning vapi memory\n"); 
        return OMPI_ERROR; 
    }
    
    mem_hndl->l_key = mr_out.l_key; 
    mem_hndl->r_key = mr_out.r_key; 
    mem_hndl->base = addr; 
    mem_hndl->bound = (void*) ((char*) addr + size - 1); 
    
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_vapi_deregister(mca_mpool_base_module_t* mpool, void *addr, size_t size){
    
    VAPI_ret_t ret; 
    mca_mpool_vapi_module_t * mpool_vapi = (mca_mpool_vapi_module_t*) mpool; 

    ret = VAPI_deregister_mr(
                             mpool_vapi->hca_pd.hca, 
                             mpool_vapi->mem_hndl.hndl
                             ); 
    
    if(VAPI_OK != ret){ 
        ompi_output(0, "%s: error unpinning vapi memory\n", __func__); 
        return OMPI_ERROR; 
    }
    return OMPI_SUCCESS; 
}

/**
  * realloc function 
  */
void* mca_mpool_vapi_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    struct mca_bmi_base_registration_t** registration)
{
    mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
    return mpool_vapi->vapi_allocator->alc_realloc( mpool_vapi->vapi_allocator, addr, size, registration);
}

/**
  * free function 
  */
void mca_mpool_vapi_free(mca_mpool_base_module_t* mpool, void * addr)
{
    mca_mpool_vapi_module_t* mpool_vapi = (mca_mpool_vapi_module_t*)mpool; 
    mpool_vapi->vapi_allocator->alc_free(mpool_vapi->vapi_allocator, addr);
}



















