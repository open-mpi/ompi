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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/base/proxy/ras_base_proxy.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/ras/base/static-components.h"


/*
 * Global variables
 */
orte_ras_base_module_t orte_ras = {
    orte_ras_base_allocate,
    orte_ras_base_node_insert,
    orte_ras_base_node_query,
    orte_ras_base_node_query_alloc,
    orte_ras_base_node_lookup,
    orte_ras_base_deallocate,
    orte_ras_base_finalize
};

orte_ras_base_module_t orte_ras_no_op = {
    orte_ras_base_allocate_no_op,
    orte_ras_base_node_insert_no_op,
    orte_ras_base_node_query_no_op,
    orte_ras_base_node_query_alloc_no_op,
    orte_ras_base_node_lookup_no_op,
    orte_ras_base_deallocate_no_op,
    orte_ras_base_finalize
};

orte_ras_base_t orte_ras_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_ras_base_open(void)
{
    int value, rc, param;
    orte_data_type_t tmp;
    char *requested;

    /* Debugging / verbose output */

    orte_ras_base.ras_output = opal_output_open(NULL);
    mca_base_param_reg_int_name("ras", "base_verbose", 
                                "Enable debugging for the RAS framework (nonzero = enabled)",
                                false, false, 0, &value);
    if (value != 0) {
        orte_ras_base.ras_output = opal_output_open(NULL);
    } else {
        orte_ras_base.ras_output = -1;
    }

    /* Defaults */

    orte_ras_base.ras_opened_valid = false;
    orte_ras_base.ras_using_proxy = false;
    orte_ras_base.ras_available_valid = false;

    /** register the base system types with the DSS */
    tmp = ORTE_RAS_NODE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_ras_base_pack_node,
                                                     orte_ras_base_unpack_node,
                                                     (orte_dss_copy_fn_t)orte_ras_base_copy_node,
                                                     (orte_dss_compare_fn_t)orte_ras_base_compare_node,
                                                     (orte_dss_size_fn_t)orte_ras_base_size_node,
                                                     (orte_dss_print_fn_t)orte_ras_base_print_node,
                                                     (orte_dss_release_fn_t)orte_ras_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_RAS_NODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Some systems do not want any RAS support. In those cases,
        * memory consumption is also an issue. For those systems, we
        * avoid opening the RAS components by checking for a directive
        * to use the "null" component.
        */
    param = mca_base_param_reg_string_name("ras", NULL, NULL,
                                           false, false, NULL, NULL);
    if (ORTE_ERROR == mca_base_param_lookup_string(param, &requested)) {
        return ORTE_ERROR;
    }
    if (NULL != requested && 0 == strcmp(requested, "null")) {
        /* the user has specifically requested that we use the "null"
        * component. In this case, that means we do NOT open any
        * components, and we simply use the default module we have
        * already defined above
        */
        orte_ras_base.ras_opened_valid = false;
        orte_ras = orte_ras_no_op; /* use the no_op module */
        return ORTE_SUCCESS;
    }

    /* check for timing tests */
    param = mca_base_param_reg_int_name("orte", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_ras_base.timing = true;
    } else {
        orte_ras_base.timing = false;
    }
    
    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("ras", orte_ras_base.ras_output,
                                 mca_ras_base_static_components, 
                                 &orte_ras_base.ras_opened, true)) {
        return ORTE_ERROR;
    }

    /* if we are not on a HNP, select the proxy 'module' */
    if (!orte_process_info.seed) {
        orte_ras = orte_ras_base_proxy_module;
        /* initialize the module */
        orte_ras_base_proxy_init(&rc);
        orte_ras_base.ras_using_proxy = true;
        return ORTE_SUCCESS;
    }

    /* All done */

    orte_ras_base.ras_opened_valid = true;
    return ORTE_SUCCESS;
}

