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
 *
 * Copyright (c) 2006      Voltaire. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi/mca/rcache/rcache.h"
#include "rcache_vma.h"
#include "rcache_vma_tree.h"
#include "rcache_vma_mru.h"
#include "opal/util/output.h"
#include "ompi/mca/mpool/base/base.h" 

extern unsigned int mca_mpool_base_page_size; 
extern unsigned int mca_mpool_base_page_size_log;

/**
 * Initialize the rcache 
 */ 

void mca_rcache_vma_module_init( mca_rcache_vma_module_t* rcache ) { 

    rcache->base.rcache_find = mca_rcache_vma_find; 
    rcache->base.rcache_insert = mca_rcache_vma_insert; 
    rcache->base.rcache_delete = mca_rcache_vma_delete; 
    rcache->base.rcache_finalize = mca_rcache_vma_finalize; 
    OBJ_CONSTRUCT(&rcache->base.lock, opal_mutex_t);
    mca_rcache_vma_tree_init(rcache);
    mca_rcache_vma_mru_init(rcache);
}

int mca_rcache_vma_find (
                        struct mca_rcache_base_module_t* rcache, 
                        void* addr, 
                        size_t size, 
                        ompi_pointer_array_t* regs, 
                        uint32_t *cnt
                        ){ 
    
    int rc = OMPI_SUCCESS;
    mca_mpool_base_registration_t *reg;
    void* base_addr; 
    void* bound_addr; 

    if(size == 0) { 
        return OMPI_ERROR; 
    }

    OPAL_THREAD_LOCK(&rcache->lock);
    *cnt = 0;
    
    base_addr = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((unsigned long) addr + size - 1), mca_mpool_base_page_size_log);
        
    reg = mca_rcache_vma_tree_find((mca_rcache_vma_module_t*)rcache, base_addr,
            bound_addr); 
    if (reg != NULL) {
        ompi_pointer_array_add(regs, (void*) reg);
        if(reg->flags & MCA_MPOOL_FLAGS_CACHE) {
            rc = mca_rcache_vma_mru_touch((mca_rcache_vma_module_t*)rcache, reg);
            if(OMPI_SUCCESS != rc) {
                OPAL_THREAD_UNLOCK(&rcache->lock);
                return OMPI_ERROR;
            }
        }
            
        OPAL_THREAD_ADD32((int32_t*) &reg->ref_count, 1);
        (*cnt)++;
        assert(((void*)reg->bound) >= addr);
    }

    OPAL_THREAD_UNLOCK(&rcache->lock);
    return OMPI_SUCCESS;
}

int mca_rcache_vma_insert ( 
                          struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* reg, 
                          uint32_t flags
                          ) { 
    size_t reg_size = reg->bound - reg->base + 1;
    mca_mpool_base_registration_t* old_reg;

    OPAL_THREAD_LOCK(&rcache->lock); 

    if((flags & MCA_MPOOL_FLAGS_CACHE) &&
            reg_size > ((mca_rcache_vma_module_t*)rcache)->reg_max_mru_size)
    {
        OPAL_THREAD_UNLOCK(&rcache->lock); 
       /* if the registration is too big for the rcache, 
          don't cache it and reset the flags so the upper level 
          handles things appropriatly */
        reg->flags = 0;
        return OMPI_SUCCESS;
    }

    reg->flags = flags;

    while(mca_rcache_vma_tree_insert((mca_rcache_vma_module_t*)rcache, reg) == 
            OMPI_ERR_TEMP_OUT_OF_RESOURCE) {
        /* call deregister - which removes the registration from
         * the tree and mru list. memory will be deregistered when
         * the reference count goes to zero.
         */
        old_reg = (mca_mpool_base_registration_t*)opal_list_get_first(&((mca_rcache_vma_module_t*)rcache)->mru_list);
         /* we need to retain first, because we only want the registration 
            removed from the tree and the mru */
        old_reg->mpool->mpool_retain(old_reg->mpool, old_reg);
        old_reg->mpool->mpool_deregister(old_reg->mpool, old_reg);
    }
    OPAL_THREAD_ADD32((int32_t*) &reg->ref_count, 1);

    if(flags & MCA_MPOOL_FLAGS_CACHE) {
        mca_rcache_vma_mru_insert((mca_rcache_vma_module_t*)rcache, reg);
        OPAL_THREAD_ADD32((int32_t*)&reg->ref_count, 1);
    }
    OPAL_THREAD_UNLOCK(&rcache->lock);

    return OMPI_SUCCESS;
}

int mca_rcache_vma_delete (
                          struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* reg, 
                          uint32_t flags
                          ) { 
    int rc = OMPI_SUCCESS; 
    assert(reg->ref_count >= 1); 
    OPAL_THREAD_LOCK(&rcache->lock); 
    if(flags & MCA_MPOOL_FLAGS_CACHE) { 
        assert(reg->ref_count >= 2);
        OPAL_THREAD_ADD32((int32_t*)&reg->ref_count, -1);
        rc = mca_rcache_vma_mru_delete((mca_rcache_vma_module_t*)rcache, reg); 
    }
    if(OMPI_SUCCESS != rc) { 
        OPAL_THREAD_UNLOCK(&rcache->lock);
        return rc; 
    }
    reg->flags = 0; 
    OPAL_THREAD_ADD32((int32_t*)&reg->ref_count, -1);
    rc =  mca_rcache_vma_tree_delete((mca_rcache_vma_module_t*)rcache, reg );
    OPAL_THREAD_UNLOCK(&rcache->lock);
    return rc; 
}

/**
  * finalize
  */
void mca_rcache_vma_finalize(
                            struct mca_rcache_base_module_t* rcache
                            ) { 

}
