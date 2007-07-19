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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>

#include "opal/event/event.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"


int orte_iof_base_close(void)
{
    opal_list_item_t* item;

    /* We only need to flush if an iof component was successfully
       selected */

    if (orte_iof_base.iof_flush) {
        orte_iof.iof_flush();
        orte_iof_base.iof_flush = false;
    }

    /* finalize component */
    if (NULL != orte_iof.iof_finalize) {
        orte_iof.iof_finalize();
    }

    /* shutdown any remaining opened components */
    if (0 != opal_list_get_size(&orte_iof_base.iof_components_opened)) {
        mca_base_components_close(orte_iof_base.iof_output, 
                              &orte_iof_base.iof_components_opened, NULL);
    }

    /* final cleanup of resources */
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    while((item = opal_list_remove_first(&orte_iof_base.iof_endpoints)) != NULL) {
        OBJ_RELEASE(item);
    }

    if (NULL != orte_iof_base.iof_service) {
        free(orte_iof_base.iof_service);
    }

    OBJ_DESTRUCT(&orte_iof_base.iof_components_opened);
    OBJ_DESTRUCT(&orte_iof_base.iof_endpoints);
    OBJ_DESTRUCT(&orte_iof_base.iof_lock);
    OBJ_DESTRUCT(&orte_iof_base.iof_condition);
    OBJ_DESTRUCT(&orte_iof_base.iof_fragments);

    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);

    return ORTE_SUCCESS;
}

