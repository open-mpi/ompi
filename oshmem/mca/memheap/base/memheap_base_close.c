/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/runtime/opal.h"
#include "oshmem/constants.h"
#include "opal/mca/mca.h"
#include "oshmem/mca/memheap/base/base.h"


int mca_memheap_base_close(void)
{
    int rc = OSHMEM_SUCCESS;

    if( mca_memheap_base_already_opened <= 0 ) {
        return OSHMEM_ERROR;
    }
    mca_memheap_base_already_opened--; 
    if (mca_memheap_base_already_opened > 0) {
        return OSHMEM_SUCCESS;
    }
#if 0
    /* disable event processing while cleaning up memheaps */
    opal_event_disable();
#endif

    /* Free allocated module */
    /*free(mca_memheap_base_module_initialized);
    if(NULL != mca_memheap_base_module_initialized){
        rc = mca_memheap_base_module_initialized->memheap_finalize(mca_memheap_base_module_initialized);
        if (OSHMEM_SUCCESS != rc){
            return OSHMEM_ERROR;
        }
   }*/

    memheap_oob_destruct();

    rc = mca_memheap_base_deregister(&mca_memheap_base_map);

    mca_memheap_base_alloc_exit(&mca_memheap_base_map);
    mca_memheap_base_static_exit(&mca_memheap_base_map);

    /* Close the maximal priority component which is the only component remained opened */
    if (0 != opal_list_get_size(&mca_memheap_base_components_opened)) {
        mca_base_components_close(mca_memheap_base_output, 
                                  &mca_memheap_base_components_opened, NULL);
    }
 
    /* cleanup */
    if(NULL != mca_memheap_base_include)
        free(mca_memheap_base_include);
    if(NULL != mca_memheap_base_exclude)
        free(mca_memheap_base_exclude);

#if 0 
    /* restore event processing */
    opal_event_enable();
#endif

    /* All done */
    return OSHMEM_SUCCESS;
}
