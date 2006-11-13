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
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/pls_private.h"
#include "orte/mca/pls/base/base.h"

int orte_pls_base_finalize(void)
{
    int rc;
    
    /* Finalize the selected module */
    orte_pls.finalize();

    /* if we are an HNP, then stop our receive */
    if (orte_process_info.seed) {
        if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}


int orte_pls_base_close(void)
{
    /* finalize selected module */
    if (orte_pls_base.selected) {
        orte_pls.finalize();
    }
    
    /* Close all open components */
    mca_base_components_close(orte_pls_base.pls_output, 
                                &orte_pls_base.available_components, NULL);
    OBJ_DESTRUCT(&orte_pls_base.available_components);

    /* clearout the orted cmd locks */
    OBJ_DESTRUCT(&orte_pls_base.orted_cmd_lock);
    OBJ_DESTRUCT(&orte_pls_base.orted_cmd_cond);
    
    return ORTE_SUCCESS;
}

