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
#include "opal/util/output.h"
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
    mca_mpool_base_selected_module_t* current;
    int rc;
    opal_list_item_t* item;
    if(size == 0) { 
        return;
    }
          
    for(item = opal_list_get_first(&mca_mpool_base_modules);
        item != opal_list_get_end(&mca_mpool_base_modules);
        item = opal_list_get_next(item)) {
        bool warn = true;
        
        current = (mca_mpool_base_selected_module_t*) item; 

        if(current->mpool_module->mpool_release_memory != NULL) {
            rc = current->mpool_module->mpool_release_memory(current->mpool_module,
                    base, size);

            if(rc != OMPI_SUCCESS && true == warn) {
                 opal_output(0, "Memory %p:%lu cannot be freed from the "
                         "registration cache. Possible memory corruption.\n",
                         base, (unsigned long)size);
                 warn = false;
            }
        }
    }
}
