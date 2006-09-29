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
#include "orte/mca/pls/base/base.h"
#include "opal/util/output.h"


int orte_pls_base_finalize(void)
{
    /* Finalize the selected module */
    orte_pls.finalize();
    
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

    return ORTE_SUCCESS;
}

