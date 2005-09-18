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

#include "mca/rcache/rcache.h"
#include "rcache_rb.h"
#include "rcache_rb_tree.h"
#include "rcache_rb_mru.h"


/**
 * Initialize the rcache 
 */ 

void mca_rcache_rb_module_init( mca_rcache_rb_module_t* rcache ) { 

    rcache->base.rcache_find = mca_rcache_rb_find; 
    rcache->base.rcache_insert = mca_rcache_rb_insert; 
    rcache->base.rcache_delete = mca_rcache_rb_delete; 
    rcache->base.rcache_finalize = mca_rcache_rb_finalize; 
    mca_rcache_rb_tree_init(rcache);
    mca_rcache_rb_mru_init(rcache);
}

int mca_rcache_rb_find (
                        struct mca_rcache_base_module_t* rcache, 
                        void* addr, 
                        size_t size, 
                        ompi_pointer_array_t* regs, 
                        uint32_t *cnt
                        ){ 
    
    int rc = OMPI_SUCCESS; 
    mca_rcache_rb_tree_item_t* tree_item; 
    OPAL_THREAD_LOCK(&rcache->lock);
    *cnt = 0;
    tree_item = mca_rcache_rb_tree_find( (mca_rcache_rb_module_t*) rcache, addr ); 
    if(NULL == tree_item) { 
        OPAL_THREAD_UNLOCK(&rcache->lock);
        return OMPI_ERROR; 
    }
    
   
    rc =  ompi_pointer_array_add(regs, (void*) tree_item->reg); 
    if(OMPI_SUCCESS != rc) { 
        OPAL_THREAD_UNLOCK(&rcache->lock);
        return rc; 
    }
    
    if( !(tree_item->reg->flags & MCA_MPOOL_FLAGS_PERSIST) ) { 
        rc = mca_rcache_rb_mru_touch((mca_rcache_rb_module_t*)rcache, tree_item->reg); 
    }
    OPAL_THREAD_ADD32((int32_t*) &tree_item->reg->ref_count, 1); 
    OPAL_THREAD_UNLOCK(&rcache->lock);
    if(rc == OMPI_SUCCESS) { 
        *cnt = 1; 
    }
    return rc;
}

int mca_rcache_rb_insert ( 
                          struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* reg, 
                          uint32_t flags
                          ) { 
    int rc = OMPI_SUCCESS;
    OPAL_THREAD_LOCK(&rcache->lock); 
    if(!(flags & MCA_MPOOL_FLAGS_PERSIST)) { 
        rc = mca_rcache_rb_mru_insert( (mca_rcache_rb_module_t*) rcache, reg); 
        if(OMPI_SUCCESS != rc) { 
            OPAL_THREAD_UNLOCK(&rcache->lock); 
            return rc; 
        } else { 
            OPAL_THREAD_ADD32((int32_t*)&reg->ref_count, 1); 
        }
    }
    rc = mca_rcache_rb_tree_insert((mca_rcache_rb_module_t*)rcache, reg );
    OPAL_THREAD_ADD32((int32_t*) &reg->ref_count, 1); 
    OPAL_THREAD_UNLOCK(&rcache->lock); 
    return rc;
}

int mca_rcache_rb_delete (
                          struct mca_rcache_base_module_t* rcache, 
                          mca_mpool_base_registration_t* reg, 
                          uint32_t flags
                          ) { 
    int rc = OMPI_SUCCESS; 
    OPAL_THREAD_LOCK(&rcache->lock); 
    if(!(flags & MCA_MPOOL_FLAGS_PERSIST)) { 
        rc = mca_rcache_rb_mru_delete( (mca_rcache_rb_module_t*) rcache, reg); 
    }
    if(OMPI_SUCCESS != rc) { 
        OPAL_THREAD_UNLOCK(&rcache->lock);
        return rc; 
    }
    reg->flags = 0; 
    rc =  mca_rcache_rb_tree_delete((mca_rcache_rb_module_t*)rcache, reg );
    OPAL_THREAD_UNLOCK(&rcache->lock);
    return rc; 
}



/**
  * finalize
  */
void mca_rcache_rb_finalize(
                            struct mca_rcache_base_module_t* rcache
                            ) { 

}





