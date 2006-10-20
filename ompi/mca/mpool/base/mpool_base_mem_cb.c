/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/**
 * @file
 */
#include "ompi_config.h"
#include "mpool_base_mem_cb.h"
#include "base.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"


extern uint32_t mca_mpool_base_page_size; 
extern uint32_t mca_mpool_base_page_size_log; 
ompi_pointer_array_t mca_mpool_base_mem_cb_array; 

/*
 *  memory hook callback, called when memory is free'd out from under us
 */
void mca_mpool_base_mem_cb(void* base, size_t size, void* cbdata, 
                           bool from_alloc)
{
    uint32_t i, cnt;
    mca_mpool_base_registration_t* reg;
    mca_mpool_base_selected_module_t* current;
    int rc;
    opal_list_item_t* item;
    void* base_addr; 
    void* bound_addr; 
    if(size == 0) { 
        return;
    }
          
    base_addr = down_align_addr( base, mca_mpool_base_page_size_log);
    bound_addr = up_align_addr((void*) ((ptrdiff_t) base + size - 1), mca_mpool_base_page_size_log);
    for(item = opal_list_get_first(&mca_mpool_base_modules);
        item != opal_list_get_end(&mca_mpool_base_modules);
        item = opal_list_get_next(item)) {
        
        current = (mca_mpool_base_selected_module_t*) item; 
        
        if(NULL != current->mpool_module->mpool_find) { 
            rc = current->mpool_module->mpool_find(
                                                   current->mpool_module, 
                                                   base_addr,
                                                   size,
                                                   &mca_mpool_base_mem_cb_array, 
                                                   &cnt 
                                                   ); 
            if(OMPI_SUCCESS != rc) { 
                continue;
            }
            for(i = 0; i < cnt; i++) { 
                reg = (mca_mpool_base_registration_t*)ompi_pointer_array_get_item(&mca_mpool_base_mem_cb_array, i);
#if !defined(NDEBUG)
                if(reg->flags & MCA_MPOOL_FLAGS_CACHE) { 
                    assert(reg->ref_count <= 3);
                } else if(reg->flags & MCA_MPOOL_FLAGS_PERSIST) { 
                    assert(reg->ref_count <= 2); 
                } else { 
                    assert(reg->ref_count <= 1); 
                }
#endif
                current->mpool_module->mpool_deregister(current->mpool_module, reg); 
            }
            ompi_pointer_array_remove_all(&mca_mpool_base_mem_cb_array);
        }
    }
    
}
