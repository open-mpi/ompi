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
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"


int orte_rmaps_base_finalize(void)
{
    opal_list_item_t* item;
    int rc;

    if (!orte_rmaps_base.no_op_selected) {
        /* Finalize all available modules */
        
        while (NULL != 
               (item = opal_list_remove_first(&orte_rmaps_base.rmaps_available))) {
            orte_rmaps_base_cmp_t* cmp = (orte_rmaps_base_cmp_t*) item;
            opal_output(orte_rmaps_base.rmaps_output,
                        "orte:rmaps:base:close: finalizing module %s",
                        cmp->component->rmaps_version.mca_component_name);
            if (NULL != cmp->module->finalize) {
                cmp->module->finalize();
            }
            OBJ_RELEASE(cmp);
        }
    }

    /* if we are an HNP, then cancel our receive */
    if (orte_process_info.seed) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_comm_stop())) {
            return rc;
        }
    }

    
    return ORTE_SUCCESS;
}

int orte_rmaps_base_close(void)
{
    if (!orte_rmaps_base.no_op_selected) {
        /* Close all remaining open components */

        mca_base_components_close(orte_rmaps_base.rmaps_output, 
                                  &orte_rmaps_base.rmaps_opened, NULL);
    }

    return ORTE_SUCCESS;
}

