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
#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/sds/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/sds/base/static-components.h"

opal_list_t orte_sds_base_components_available;
orte_sds_base_module_t *orte_sds_base_module = NULL;

int
orte_sds_base_open(void)
{
    OBJ_CONSTRUCT(&orte_sds_base_components_available, opal_list_t);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("sds", 0, mca_sds_base_static_components, 
                                 &orte_sds_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}
