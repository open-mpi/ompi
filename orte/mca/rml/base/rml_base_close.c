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
#include "orte/mca/rml/base/base.h"


int orte_rml_base_close(void)
{
    /* shutdown any remaining opened components */
    if (! opal_list_is_empty(&orte_rml_base.rml_components)) {
        mca_base_components_close(orte_rml_base.rml_output, 
                              &orte_rml_base.rml_components, NULL);
    }
    OBJ_DESTRUCT(&orte_rml_base.rml_components);
    return ORTE_SUCCESS;
}

