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
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

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
orte_rmaps_t orte_rmaps = {
    orte_rmaps_base_map_job,
    orte_rmaps_base_get_job_map
};


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmaps_base_open(void)
{
    int param, value;
    char *policy;

    /* init the globals */
    orte_rmaps_base.active_module = NULL;
    
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_rmaps_base.rmaps_output = opal_output_open(NULL);

    /* Are we scheduling by node or by slot? */
    param = mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                           "Scheduling Policy for RMAPS. [slot | node]",
                                           false, false, "unspec", &policy);
    
    if (0 == strcmp(policy, "unspec")) {
        orte_rmaps_base.policy = ORTE_RMAPS_BYSLOT;  /* default to byslot */
    } else if (0 == strcmp(policy, "node")) {
       orte_rmaps_base.policy = ORTE_RMAPS_BYNODE;
    } else {
        orte_rmaps_base.policy = ORTE_RMAPS_BYSLOT;  /* default to byslot */
    }

    /* Do we want one ppn if num_procs not specified */
    param = mca_base_param_reg_int_name("rmaps", "base_pernode",
                                        "Launch one ppn as directed",
                                        false, false, (int)false, &value);
    orte_rmaps_base.pernode = OPAL_INT_TO_BOOL(value);
    
    /* if pernode is set, we do not allow npernode to also be set - instead
     * we default the npernode value to 1
     */
    if (orte_rmaps_base.pernode) {
        orte_rmaps_base.npernode = 1;
    } else {
        /* Do we want n ppn */
        param = mca_base_param_reg_int_name("rmaps", "base_n_pernode",
                                            "Launch n procs/node",
                                            false, false, 0, &value);
        orte_rmaps_base.npernode = value;
        if (0 < orte_rmaps_base.npernode) {
            orte_rmaps_base.pernode = true;
        }
    }
    
    /* Should we schedule on the local node or not? */

    mca_base_param_reg_int_name("rmaps", "base_no_schedule_local",
                                "If false, allow scheduling MPI applications on the same node as mpirun (default).  If true, do not schedule any MPI applications on the same node as mpirun",
                                false, false, (int)false, &value);
    if (value) {
        orte_rmaps_base.policy |= ORTE_RMAPS_NO_USE_LOCAL;
    }

    /* Should we oversubscribe or not? */
    /** default condition that allows oversubscription */
    mca_base_param_reg_int_name("rmaps", "base_no_oversubscribe",
                                "If true, then do not allow oversubscription of nodes - mpirun will return an error if there aren't enough nodes to launch all processes without oversubscribing",
                                false, false, (int)false, &value);
    if (value) {
        orte_rmaps_base.oversubscribe = false;
    } else {
        orte_rmaps_base.oversubscribe = true;
    }
    
    /* Do we want to loadbalance the job */
    param = mca_base_param_reg_int_name("rmaps", "base_loadbalance",
                                        "Balance total number of procs across all allocated nodes",
                                        false, false, (int)false, &value);
    orte_rmaps_base.loadbalance = OPAL_INT_TO_BOOL(value);
    /* if we are doing npernode or pernode, then we cannot loadbalance */
    if (orte_rmaps_base.pernode) {
        orte_rmaps_base.loadbalance = false;
    }
    
    /* should we display the map after determining it? */
    mca_base_param_reg_int_name("rmaps", "base_display_map",
                                "Whether to display the process map after it is computed",
                                false, false, (int)false, &value);
    orte_rmaps_base.display_map = OPAL_INT_TO_BOOL(value);
    
    /* Open up all the components that we can find */
    if (ORTE_SUCCESS != 
        mca_base_components_open("rmaps", orte_rmaps_base.rmaps_output,
                                 mca_rmaps_base_static_components, 
                                 &orte_rmaps_base.available_components, true)) {
       return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
