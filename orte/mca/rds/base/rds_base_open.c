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
#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rds/base/static-components.h"

/**
 * Local functions.
 */

static void orte_rds_base_cell_desc_constructor(orte_rds_cell_desc_t *cell)
{
    cell->site = NULL;
    cell->name = NULL;
    cell->type = NULL;
    
    OBJ_CONSTRUCT(&cell->attributes, opal_list_t);
}

static void orte_rds_base_cell_desc_destructor(orte_rds_cell_desc_t *cell)
{
    if (NULL != cell->site) free(cell->site);
    if (NULL != cell->name) free(cell->name);
    if (NULL != cell->type) free(cell->type);
    
    OBJ_DESTRUCT(&cell->attributes);
}

OBJ_CLASS_INSTANCE(
    orte_rds_cell_desc_t, 
    opal_list_item_t,
    orte_rds_base_cell_desc_constructor, 
    orte_rds_base_cell_desc_destructor);


static void orte_rds_base_cell_attr_constructor(orte_rds_cell_attr_t *cell)
{
    OBJ_CONSTRUCT(&cell->keyval, orte_gpr_keyval_t);
}

static void orte_rds_base_cell_attr_destructor(orte_rds_cell_attr_t *cell)
{
    OBJ_DESTRUCT(&cell->keyval);
}

OBJ_CLASS_INSTANCE(
    orte_rds_cell_attr_t, 
    opal_list_item_t,
    orte_rds_base_cell_attr_constructor, 
    orte_rds_base_cell_attr_destructor);


/*
 * Global variables
 */
orte_rds_base_module_t orte_rds = {
    orte_rds_base_query,
    orte_rds_base_store_resource
};

orte_rds_base_module_t orte_rds_no_op = {
    orte_rds_base_no_op_query,
    orte_rds_base_no_op_store_resource
};

orte_rds_base_t orte_rds_base;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rds_base_open(void)
{
    int param, value;
    char *requested;

    /* Debugging / verbose output */

    param = mca_base_param_reg_int_name("rds_base", "verbose", 
                                        "Verbosity level for the rds framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_rds_base.rds_output = opal_output_open(NULL);
    } else {
        orte_rds_base.rds_output = -1;
    }

    /* Some systems do not want any RDS support. In those cases,
     * memory consumption is also an issue. For those systems, we
     * avoid opening the RDS components by checking for a directive
     * to use the "null" component.
     */
    param = mca_base_param_reg_string_name("rds", NULL, NULL,
                                            false, false, NULL, &requested);
    if (NULL != requested && 0 == strcmp(requested, "null")) {
        /* the user has specifically requested that we use the "null"
         * component. In this case, that means we do NOT open any
         * components, and we simply use the default module we have
         * already defined above
         */
        orte_rds_base.no_op_selected = true;
        orte_rds = orte_rds_no_op; /* use the no_op module */
        return ORTE_SUCCESS;
    }
    orte_rds_base.no_op_selected = false;
    
    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rds", orte_rds_base.rds_output, 
                                 mca_rds_base_static_components, 
                                 &orte_rds_base.rds_components, true)) {
        return ORTE_ERROR;
    }
    OBJ_CONSTRUCT(&orte_rds_base.rds_selected, opal_list_t);

    /* All done */

    return ORTE_SUCCESS;
}

