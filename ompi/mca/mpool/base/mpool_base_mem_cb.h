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
/**
 * @file
 */
#ifndef MCA_MPOOL_BASE_MEM_CB_H
#define MCA_MPOOL_BASE_MEM_CB_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "class/ompi_rb_tree.h"
#include "mca/mca.h"
#include "mca/mpool/mpool.h"
#include "opal/threads/mutex.h" 
#include "opal/memory/memory.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 *  memory hook callback, called when memory is free'd out from under us
 */
static void mca_mpool_base_mem_cb(void* base, size_t size, void* cbdata){ 
    uint32_t i, cnt;
    ompi_pointer_array_t regs;
    mca_mpool_base_registration_t* reg;
    mca_mpool_base_selected_module_t* current;
    int rc;
    opal_list_item_t* item;

    for(item = opal_list_get_first(&mca_mpool_base_modules);
        item != opal_list_get_end(&mca_mpool_base_modules);
        item = opal_list_get_next(item)) {
        
        current = (mca_mpool_base_selected_module_t*) item; 
        if(NULL != current->mpool_module->mpool_find) { 
            rc = current->mpool_module->mpool_find(
                                                   current->mpool_module, 
                                                   base,
                                                   size,
                                                   &regs, 
                                                   &cnt 
                                                   ); 
            if(OMPI_SUCCESS != rc) { 
                return;
            }
            if(0 < cnt) { 
                for(i = 0; i < cnt; i++) { 
                    reg = (mca_mpool_base_registration_t*) 
                        ompi_pointer_array_get_item(&regs, i);
                    
                    rc = current->mpool_module->mpool_deregister(
                                                                 current->mpool_module, 
                                                                 reg
                                                                 ); 
                    if(OMPI_SUCCESS != rc) { 
                        return; 
                    }
                }
            }
        }    
    }    
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_MPOOL_BASE_MEM_CB_H */



