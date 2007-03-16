/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "opal/util/output.h"
#include "mpool_udapl.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"
#include "ompi/mca/mpool/base/base.h"

extern uint32_t mca_mpool_base_page_size;
extern uint32_t mca_mpool_base_page_size_log; 
uint64_t mca_mpool_udapl_mem_registered; 

/* 
 *  Initializes the mpool module.
 */ 
void mca_mpool_udapl_module_init(mca_mpool_udapl_module_t* mpool)
{
    mpool->super.mpool_component = &mca_mpool_udapl_component.super; 
    mpool->super.mpool_base = NULL; /* no base .. */ 
    mpool->super.mpool_alloc = mca_mpool_udapl_alloc; 
    mpool->super.mpool_realloc = mca_mpool_udapl_realloc; 
    mpool->super.mpool_free = mca_mpool_udapl_free; 
    mpool->super.mpool_register = mca_mpool_udapl_register; 
    mpool->super.mpool_deregister = mca_mpool_udapl_deregister; 
    mpool->super.mpool_find = mca_mpool_udapl_find; 
    mpool->super.mpool_retain = mca_mpool_udapl_retain;
    mpool->super.mpool_release = mca_mpool_udapl_release; 
    mpool->super.mpool_finalize = NULL;
    mpool->super.mpool_ft_event = mca_mpool_udapl_ft_event;
    mpool->super.rcache = 
        mca_rcache_base_module_create(mca_mpool_udapl_component.rcache_name);
    mpool->super.flags = MCA_MPOOL_FLAGS_MPI_ALLOC_MEM; 
    mca_mpool_udapl_mem_registered = 0;
}


/**
  * allocate function 
  */
void* mca_mpool_udapl_alloc(
    mca_mpool_base_module_t* mpool, 
    size_t size, 
    size_t align, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    void* addr = malloc(size+align);
    if(NULL == addr)
        return NULL;
    if(OMPI_SUCCESS != mca_mpool_udapl_register(mpool,
            addr, size + align, flags, registration)) {
        free(addr);
        return NULL;
    }
    (*registration)->alloc_base = addr;
    
    return addr;
}


/* 
 * register memory 
 */ 
int mca_mpool_udapl_register(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    uint32_t flags, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_udapl_module_t* udapl_mpool; 
    mca_mpool_udapl_registration_t* reg;
    DAT_REGION_DESCRIPTION region;
    DAT_VLEN dat_size;
    DAT_VADDR dat_addr;
    int rc;

    udapl_mpool = (mca_mpool_udapl_module_t*)mpool;
    reg = OBJ_NEW(mca_mpool_udapl_registration_t);
    if(NULL == reg) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    reg->base_reg.mpool = mpool;
    reg->base_reg.base = down_align_addr(addr, mca_mpool_base_page_size_log); 
    reg->base_reg.flags = flags; 
    reg->base_reg.bound = up_align_addr((void*)((unsigned long) addr + size -1),
                    mca_mpool_base_page_size_log);

    region.for_va = addr;
    reg->lmr_triplet.virtual_address = (DAT_VADDR)addr;
    reg->lmr_triplet.segment_length = size;

    rc = dat_lmr_create(udapl_mpool->udapl_res.udapl_ia,
            DAT_MEM_TYPE_VIRTUAL, region, reg->lmr_triplet.segment_length,
            udapl_mpool->udapl_res.udapl_pz, DAT_MEM_PRIV_ALL_FLAG,
            &reg->lmr, &reg->lmr_triplet.lmr_context,
            &reg->rmr_context, &dat_size, &dat_addr);
    if(DAT_SUCCESS != rc) {
        MCA_MPOOL_UDAPL_ERROR(rc, "dat_lmr_create");
        OBJ_RELEASE(reg);

        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    OPAL_THREAD_ADD32(&reg->base_reg.ref_count,1);

    if(flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_insert(mpool->rcache, 
                                     (mca_mpool_base_registration_t*) reg, 
                                     flags); 
    }
    
    *registration = &reg->base_reg;
    return OMPI_SUCCESS; 
}


/* 
 * deregister memory 
 */ 
int mca_mpool_udapl_deregister(mca_mpool_base_module_t* mpool, 
                              mca_mpool_base_registration_t* reg)
{
    if(reg->flags & (MCA_MPOOL_FLAGS_CACHE | MCA_MPOOL_FLAGS_PERSIST)) { 
        mpool->rcache->rcache_delete(mpool->rcache, reg, reg->flags);
        reg->flags = 0;
    }

    return mca_mpool_udapl_release(mpool, reg);
}

/**
  * realloc function 
  */
void* mca_mpool_udapl_realloc(
    mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_base_registration_t* old_reg = *registration;
    void *new_addr = mca_mpool_udapl_alloc(mpool,
            size, 0, (*registration)->flags, registration);
    if(new_addr == NULL) {
        return NULL;
    }

    memcpy(new_addr, addr, size);
    mca_mpool_udapl_free(mpool, addr, old_reg);
    return new_addr;
}

/**
  * free function 
  */
void mca_mpool_udapl_free(mca_mpool_base_module_t* mpool, void * addr,
                         mca_mpool_base_registration_t* registration)
{
    mpool->mpool_deregister(mpool, registration);
    free(addr);
}

int mca_mpool_udapl_find(
    struct mca_mpool_base_module_t* mpool, 
    void* addr, 
    size_t size, 
    ompi_pointer_array_t *regs,
    uint32_t* cnt)
{
    return mpool->rcache->rcache_find(mpool->rcache, addr, size, regs, cnt); 
}
 
int mca_mpool_udapl_release(
    struct mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* reg)
{
    if(0 == OPAL_THREAD_ADD32(&reg->ref_count, -1)) {
        mca_mpool_udapl_registration_t *udapl_reg =
                (mca_mpool_udapl_registration_t*)reg;
        int rc = dat_lmr_free(udapl_reg->lmr);
        
        if(DAT_SUCCESS != rc) {
            MCA_MPOOL_UDAPL_ERROR(rc, "dat_lmr_free");
            return OMPI_ERROR;
        }

        OBJ_RELEASE(reg);
    }

    return OMPI_SUCCESS; 
}

int mca_mpool_udapl_retain(
    struct mca_mpool_base_module_t* mpool, 
    mca_mpool_base_registration_t* registration)
{
    OPAL_THREAD_ADD32(&registration->ref_count, 1); 
    return OMPI_SUCCESS; 
}

int mca_mpool_udapl_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
