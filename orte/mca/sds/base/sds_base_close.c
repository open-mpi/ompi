/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include <stdio.h>

#include "include/orte_constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/sds/base/base.h"
#include "opal/util/output.h"

extern opal_list_t orte_sds_base_components_available;
extern orte_sds_base_module_t *orte_sds_base_module;

int
orte_sds_base_close(void)
{
    /* finalize running component */
    orte_sds_base_module->finalize();

    /* shutdown any remaining opened components */
    if (! opal_list_is_empty(&orte_sds_base_components_available)) {
        mca_base_components_close(0, 
                                  &orte_sds_base_components_available, NULL);
    }
    OBJ_DESTRUCT(&orte_sds_base_components_available);
    return OMPI_SUCCESS;
}

