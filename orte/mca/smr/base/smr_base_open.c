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

#include "orte/dss/dss.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/mca/oob/base/base.h"

#include "orte/mca/smr/base/base.h"
#include "orte/mca/smr/base/smr_private.h"

#include "stdio.h" /* just for gef debug */


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/smr/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
orte_smr_base_t orte_smr_base;

orte_smr_base_module_t orte_smr = {

    orte_smr_base_get_proc_state,
    orte_smr_base_set_proc_state,
    orte_smr_base_get_node_state,
    orte_smr_base_set_node_state,
    orte_smr_base_get_job_state,
    orte_smr_base_set_job_state,
    orte_smr_base_begin_monitoring_not_available,
    orte_smr_base_init_job_stage_gates,
    orte_smr_base_init_orted_stage_gates,
    orte_smr_base_define_alert_monitor,
    orte_smr_base_job_stage_gate_subscribe,
    orte_smr_base_module_finalize_not_available
};

/*
 * OBJ constructors/desctructors for SMR types
 */
static void orte_smr_node_tracker_construct(orte_smr_node_state_tracker_t* node)
{
    node->cell = ORTE_CELLID_INVALID;
    node->nodename = NULL;
    node->state = ORTE_NODE_STATE_UNKNOWN;
}

static void orte_smr_node_tracker_destruct(orte_smr_node_state_tracker_t* node)
{
    if (NULL != node->nodename) free(node->nodename);
}

OBJ_CLASS_INSTANCE(orte_smr_node_state_tracker_t,
                   opal_list_item_t,
                   orte_smr_node_tracker_construct,
                   orte_smr_node_tracker_destruct);

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_smr_base_open(void)
{

    int param, value, rc;
    orte_data_type_t tmp;

  /* setup output for debug messages */

    orte_smr_base.smr_output = opal_output_open(NULL);
    param = mca_base_param_reg_int_name("smr_base", "verbose",
                                        "Verbosity level for the smr framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_smr_base.smr_output = opal_output_open(NULL);
    } else {
        orte_smr_base.smr_output = -1;
    }


    /* register the base system types with the DPS */
    tmp = ORTE_NODE_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_smr_base_pack_node_state,
                                        orte_smr_base_unpack_node_state,
                                        (orte_dss_copy_fn_t)orte_smr_base_copy_node_state,
                                        (orte_dss_compare_fn_t)orte_smr_base_compare_node_state,
                                        (orte_dss_size_fn_t)orte_smr_base_std_size,
                                        (orte_dss_print_fn_t)orte_smr_base_std_print,
                                        (orte_dss_release_fn_t)orte_smr_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_NODE_STATE", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_PROC_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_smr_base_pack_proc_state,
                                        orte_smr_base_unpack_proc_state,
                                        (orte_dss_copy_fn_t)orte_smr_base_copy_proc_state,
                                        (orte_dss_compare_fn_t)orte_smr_base_compare_proc_state,
                                        (orte_dss_size_fn_t)orte_smr_base_std_size,
                                        (orte_dss_print_fn_t)orte_smr_base_std_print,
                                        (orte_dss_release_fn_t)orte_smr_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_PROC_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_JOB_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_smr_base_pack_job_state,
                                        orte_smr_base_unpack_job_state,
                                        (orte_dss_copy_fn_t)orte_smr_base_copy_job_state,
                                        (orte_dss_compare_fn_t)orte_smr_base_compare_job_state,
                                        (orte_dss_size_fn_t)orte_smr_base_std_size,
                                        (orte_dss_print_fn_t)orte_smr_base_std_print,
                                        (orte_dss_release_fn_t)orte_smr_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_JOB_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_EXIT_CODE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_smr_base_pack_exit_code,
                                        orte_smr_base_unpack_exit_code,
                                        (orte_dss_copy_fn_t)orte_smr_base_copy_exit_code,
                                        (orte_dss_compare_fn_t)orte_smr_base_compare_exit_code,
                                        (orte_dss_size_fn_t)orte_smr_base_std_size,
                                        (orte_dss_print_fn_t)orte_smr_base_std_print,
                                        (orte_dss_release_fn_t)orte_smr_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_EXIT_CODE", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

                    /* Open up all available components */

  if (ORTE_SUCCESS !=
      mca_base_components_open("smr", orte_smr_base.smr_output,
                               mca_smr_base_static_components,
                               &orte_smr_base.smr_components, true)) {
      return ORTE_ERROR;
  }

  /* All done */

  return ORTE_SUCCESS;
}
