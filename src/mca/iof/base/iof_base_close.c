/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "event/event.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"


int orte_iof_base_close(void)
{
    ompi_list_item_t* item;

    /* We only need to flush if an iof component was successfully
       selected */

    if (orte_iof_base.iof_flush) {
        orte_iof_base_flush();
        orte_iof_base.iof_flush = false;
    }

    /* shutdown any remaining opened components */
    if (0 != ompi_list_get_size(&orte_iof_base.iof_components_opened)) {
        mca_base_components_close(orte_iof_base.iof_output, 
                              &orte_iof_base.iof_components_opened, NULL);
    }

    /* final cleanup of resources */
    OMPI_THREAD_LOCK(&orte_iof_base.iof_lock);
    while((item = ompi_list_remove_first(&orte_iof_base.iof_endpoints)) != NULL) {
        OBJ_RELEASE(item);
    }
    OMPI_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    return OMPI_SUCCESS;
}

