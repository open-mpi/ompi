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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmaps/base/static-components.h"


/*
 * Global variables
 */
orte_rmaps_base_t orte_rmaps_base;

/*
 * Declare the RMAPS module to hold the API function pointers
 */
orte_rmaps_base_module_t orte_rmaps = {
    orte_rmaps_base_map_job,
    orte_rmaps_base_get_job_map,
    orte_rmaps_base_get_node_map,
    orte_rmaps_base_finalize
};

/*
 * Include all the RMAPS class instance declarations
 */
#include "orte/mca/rmaps/base/rmaps_class_instances.h"


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmaps_base_open(void)
{
    int param, rc, value;
    char *policy;
    orte_data_type_t tmp;

    /* Debugging / verbose output */

    param = mca_base_param_reg_int_name("rmaps", "base_verbose",
                                        "Verbosity level for the rmaps framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_rmaps_base.rmaps_output = opal_output_open(NULL);
    } else {
        orte_rmaps_base.rmaps_output = -1;
    }

    /* Are we scheduling by node or by slot? */

    param = mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                           "Scheduling Policy for RMAPS. [slot | node]",
                                           false, false, "unspec", &policy);
    
    opal_output(orte_rmaps_base.rmaps_output, "Scheduling policy: %s", policy);
    
    if (0 == strcmp(policy, "unspec")) {
        orte_rmaps_base.user_specified_policy = false;
        orte_rmaps_base.bynode = false;  /* default to byslot */
    } else if (0 == strcmp(policy, "node")) {
        orte_rmaps_base.user_specified_policy = true;
       orte_rmaps_base.bynode = true;
    } else {
        orte_rmaps_base.user_specified_policy = true;
        orte_rmaps_base.bynode = false;
    }

    /* Do we want one ppn if num_procs not specified */
    param = mca_base_param_reg_int_name("rmaps", "base_pernode",
                                        "Launch one ppn as directed",
                                        false, false, (int)false, &value);

    if ((int)true == value) {
        orte_rmaps_base.per_node = true;
    } else {
        orte_rmaps_base.per_node = false;
    }
    
    /* Do we want n ppn */
    param = mca_base_param_reg_int_name("rmaps", "base_n_pernode",
                                        "Launch n procs/node",
                                        false, false, -1, &value);
    orte_rmaps_base.n_per_node = value;
    
    /* Should we schedule on the local node or not? */

    mca_base_param_reg_int_name("rmaps", "base_no_schedule_local",
                                "If false, allow scheduling MPI applications on the same node as mpirun (default).  If true, do not schedule any MPI applications on the same node as mpirun",
                                false, false, (int)false, &value);
    if ((int)true == value) {
        orte_rmaps_base.no_use_local = true;
    } else {
        orte_rmaps_base.no_use_local = false;
    }

    /* Should we oversubscribe or not? */
    
    mca_base_param_reg_int_name("rmaps", "base_no_oversubscribe",
                                "If true, then do not allow oversubscription of nodes - mpirun will return an error if there aren't enough nodes to launch all processes without oversubscribing",
                                false, false, (int)false, &value);
    if ((int)false == value) {
        orte_rmaps_base.oversubscribe = true;  /** default condition that allows oversubscription */
    } else {
        orte_rmaps_base.oversubscribe = false;
    }
    
    /* should we display the map after determining it? */
    mca_base_param_reg_int_name("rmaps_base", "display_map",
                                "Whether to display the process map after it is computed",
                                false, false, (int)false, &value);
    if ((int)false == value) {
        orte_rmaps_base.display_map = false;
    } else {
        orte_rmaps_base.display_map = true;
    }
    
    /** register the base system types with the DSS */
    tmp = ORTE_JOB_MAP;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmaps_base_pack_map,
                                                     orte_rmaps_base_unpack_map,
                                                     (orte_dss_copy_fn_t)orte_rmaps_base_copy_map,
                                                     (orte_dss_compare_fn_t)orte_rmaps_base_compare_map,
                                                     (orte_dss_size_fn_t)orte_rmaps_base_size_map,
                                                     (orte_dss_print_fn_t)orte_rmaps_base_print_map,
                                                     (orte_dss_release_fn_t)orte_rmaps_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_JOB_MAP", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_MAPPED_PROC;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmaps_base_pack_mapped_proc,
                                                     orte_rmaps_base_unpack_mapped_proc,
                                                     (orte_dss_copy_fn_t)orte_rmaps_base_copy_mapped_proc,
                                                     (orte_dss_compare_fn_t)orte_rmaps_base_compare_mapped_proc,
                                                     (orte_dss_size_fn_t)orte_rmaps_base_size_mapped_proc,
                                                     (orte_dss_print_fn_t)orte_rmaps_base_print_mapped_proc,
                                                     (orte_dss_release_fn_t)orte_rmaps_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_MAPPED_PROC", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_MAPPED_NODE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmaps_base_pack_mapped_node,
                                                     orte_rmaps_base_unpack_mapped_node,
                                                     (orte_dss_copy_fn_t)orte_rmaps_base_copy_mapped_node,
                                                     (orte_dss_compare_fn_t)orte_rmaps_base_compare_mapped_node,
                                                     (orte_dss_size_fn_t)orte_rmaps_base_size_mapped_node,
                                                     (orte_dss_print_fn_t)orte_rmaps_base_print_mapped_node,
                                                     (orte_dss_release_fn_t)orte_rmaps_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_MAPPED_NODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    
    /* Open up all the components that we can find */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rmaps", orte_rmaps_base.rmaps_output,
                                 mca_rmaps_base_static_components, 
                                 &orte_rmaps_base.rmaps_opened, true)) {
       return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
