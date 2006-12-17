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
#include "opal/util/output.h"
#include "ompi/mca/mpool/base/base.h" 

extern unsigned int mca_mpool_base_page_size; 
extern unsigned int mca_mpool_base_page_size_log;

/**
 * Initialize the rcache 
 */ 

void mca_rcache_vma_module_init( mca_rcache_vma_module_t* rcache ) { 
    rcache->base.rcache_find = mca_rcache_vma_find; 
    rcache->base.rcache_find_all = mca_rcache_vma_find_all;
    rcache->base.rcache_insert = mca_rcache_vma_insert; 
    rcache->base.rcache_delete = mca_rcache_vma_delete; 
    rcache->base.rcache_finalize = mca_rcache_vma_finalize; 
    OBJ_CONSTRUCT(&rcache->base.lock, opal_mutex_t);
    mca_rcache_vma_tree_init(rcache);
}

int mca_rcache_vma_find(struct mca_rcache_base_module_t* rcache,
        void* addr, size_t size, mca_mpool_base_registration_t **reg)
{
    void* base_addr; 
    void* bound_addr; 

    if(size == 0) { 
        return OMPI_ERROR; 
    }

    base_addr = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((unsigned long) addr + size - 1), mca_mpool_base_page_size_log);
        
    *reg = mca_rcache_vma_tree_find((mca_rcache_vma_module_t*)rcache, base_addr,
            bound_addr); 

    return OMPI_SUCCESS;
}

int mca_rcache_vma_find_all(struct mca_rcache_base_module_t* rcache,
        void* addr, size_t size, ompi_pointer_array_t *regs)
{
    void *base_addr, *bound_addr;

    if(size == 0) {
        return OMPI_ERROR;
    }

    base_addr = down_align_addr(addr, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((unsigned long) addr + size - 1), mca_mpool_base_page_size_log);

    return mca_rcache_vma_tree_find_all((mca_rcache_vma_module_t*)rcache,
            base_addr, bound_addr, regs);
}

int mca_rcache_vma_insert(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* reg, size_t limit)
{
    size_t reg_size = reg->bound - reg->base + 1;
    mca_rcache_vma_module_t *vma_rcache = (mca_rcache_vma_module_t*)rcache;

    if(limit != 0 && reg_size > limit) {
        /* return out of resources if request is bigger than cache size
         * return temp out of resources otherwise */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return mca_rcache_vma_tree_insert(vma_rcache, reg, limit);
}

int mca_rcache_vma_delete(struct mca_rcache_base_module_t* rcache,
        mca_mpool_base_registration_t* reg)
{
    mca_rcache_vma_module_t *vma_rcache = (mca_rcache_vma_module_t*)rcache;
    return mca_rcache_vma_tree_delete(vma_rcache, reg);
}

/**
  * finalize
  */
void mca_rcache_vma_finalize(struct mca_rcache_base_module_t* rcache)
{
}
