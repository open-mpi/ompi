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
#include "orte/mca/sds/base/base.h"
#include "opal/util/output.h"

extern opal_list_t orte_sds_base_components_available;
extern orte_sds_base_module_t *orte_sds_base_module;

int
orte_sds_base_close(void)
{
    /* finalize running component */
    if (NULL != orte_sds_base_module) {
        orte_sds_base_module->finalize();
    }

    /* shutdown any remaining opened components */
    if (! opal_list_is_empty(&orte_sds_base_components_available)) {
        mca_base_components_close(0, 
                                  &orte_sds_base_components_available, NULL);
    }
    OBJ_DESTRUCT(&orte_sds_base_components_available);
    return ORTE_SUCCESS;
}

