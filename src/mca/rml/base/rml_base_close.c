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
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/rml/base/base.h"


int orte_rml_base_close(void)
{
    /* shutdown any remaining opened components */
    if (! ompi_list_is_empty(&orte_rml_base.rml_components)) {
        mca_base_components_close(orte_rml_base.rml_output, 
                              &orte_rml_base.rml_components, NULL);
    }
    OBJ_DESTRUCT(&orte_rml_base.rml_components);
    return OMPI_SUCCESS;
}

