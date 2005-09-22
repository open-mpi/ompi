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
#include "mpool_base_mem_cb.h"
#include "base.h"

/*
 *  memory hook callback, called when memory is free'd out from under us
 */
void mca_mpool_base_mem_cb(void* base, size_t size, void* cbdata)
{
    uint32_t i, cnt;
    ompi_pointer_array_t regs;
    mca_mpool_base_registration_t* reg;
    mca_mpool_base_selected_module_t* current;
    int rc;
    opal_list_item_t* item;

    OBJ_CONSTRUCT(&regs, ompi_pointer_array_t);
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
                continue;
            }
            for(i = 0; i < cnt; i++) { 
                reg = (mca_mpool_base_registration_t*)ompi_pointer_array_get_item(&regs, i);
                current->mpool_module->mpool_deregister(current->mpool_module, reg); 
            }
        }    
    }    
    OBJ_DESTRUCT(&regs);
}



