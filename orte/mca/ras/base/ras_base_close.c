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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/base/base.h"


int orte_ras_base_finalize(void)
{
    opal_list_item_t* item;
    int rc;

    if (orte_ras_base.ras_available_valid) {
        /* Finalize all available modules */
        while (NULL != 
               (item = opal_list_remove_first(&orte_ras_base.ras_available))) {
            orte_ras_base_cmp_t* cmp = (orte_ras_base_cmp_t*)item;
            cmp->module->finalize();
            OBJ_RELEASE(cmp);
        }
        OBJ_DESTRUCT(&orte_ras_base.ras_available);

        /* if we are an HNP, stop the receive */
        if (orte_process_info.seed) {
            if (ORTE_SUCCESS != (rc = orte_ras_base_comm_stop())) {
                ORTE_ERROR_LOG(rc);
            }
        }
    }

    return ORTE_SUCCESS;
}


int orte_ras_base_close(void)
{
    if (orte_ras_base.ras_opened_valid) {
        /* Close all remaining available components (may be one if this is a
        Open RTE program, or [possibly] multiple if this is ompi_info) */

        mca_base_components_close(orte_ras_base.ras_output, 
                                  &orte_ras_base.ras_opened, NULL);
    }
  
    return ORTE_SUCCESS;
}

