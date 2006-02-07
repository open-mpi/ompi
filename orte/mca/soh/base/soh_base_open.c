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

#include "include/orte_constants.h"

#include "dss/dss.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "opal/util/output.h"
#include "util/proc_info.h"
#include "mca/oob/base/base.h"

#include "mca/soh/base/base.h"

#include "stdio.h" /* just for gef debug */


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/soh/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
orte_soh_base_t orte_soh_base;

orte_soh_base_module_t orte_soh = {

    orte_soh_base_get_proc_soh,
    orte_soh_base_set_proc_soh,
    orte_soh_base_get_node_soh_not_available,
    orte_soh_base_set_node_soh_not_available,
    orte_soh_base_get_job_soh,
    orte_soh_base_set_job_soh,
    orte_soh_base_begin_monitoring_not_available,
    orte_soh_base_module_finalize_not_available
};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_soh_base_open(void)
{

    int param, value, rc;
    orte_data_type_t tmp;

/* fprintf(stderr,"orte_soh_base_open:enter\n"); */

  /* setup output for debug messages */

    orte_soh_base.soh_output = opal_output_open(NULL);
    param = mca_base_param_reg_int_name("soh_base", "verbose",
                                        "Verbosity level for the soh framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_soh_base.soh_output = opal_output_open(NULL);
    } else {
        orte_soh_base.soh_output = -1;
    }


    /* register the base system types with the DPS */
    tmp = ORTE_NODE_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_soh_base_pack_node_state,
                                        orte_soh_base_unpack_node_state,
                                        (orte_dss_copy_fn_t)orte_soh_base_copy_node_state,
                                        (orte_dss_compare_fn_t)orte_soh_base_compare_node_state,
                                        (orte_dss_size_fn_t)orte_soh_base_std_size,
                                        (orte_dss_print_fn_t)orte_soh_base_std_print,
                                        (orte_dss_release_fn_t)orte_soh_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_NODE_STATE", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_PROC_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_soh_base_pack_proc_state,
                                        orte_soh_base_unpack_proc_state,
                                        (orte_dss_copy_fn_t)orte_soh_base_copy_proc_state,
                                        (orte_dss_compare_fn_t)orte_soh_base_compare_proc_state,
                                        (orte_dss_size_fn_t)orte_soh_base_std_size,
                                        (orte_dss_print_fn_t)orte_soh_base_std_print,
                                        (orte_dss_release_fn_t)orte_soh_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_PROC_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_JOB_STATE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_soh_base_pack_job_state,
                                        orte_soh_base_unpack_job_state,
                                        (orte_dss_copy_fn_t)orte_soh_base_copy_job_state,
                                        (orte_dss_compare_fn_t)orte_soh_base_compare_job_state,
                                        (orte_dss_size_fn_t)orte_soh_base_std_size,
                                        (orte_dss_print_fn_t)orte_soh_base_std_print,
                                        (orte_dss_release_fn_t)orte_soh_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_JOB_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_EXIT_CODE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_soh_base_pack_exit_code,
                                        orte_soh_base_unpack_exit_code,
                                        (orte_dss_copy_fn_t)orte_soh_base_copy_exit_code,
                                        (orte_dss_compare_fn_t)orte_soh_base_compare_exit_code,
                                        (orte_dss_size_fn_t)orte_soh_base_std_size,
                                        (orte_dss_print_fn_t)orte_soh_base_std_print,
                                        (orte_dss_release_fn_t)orte_soh_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_EXIT_CODE", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

                    /* Open up all available components */

  if (OMPI_SUCCESS !=
      mca_base_components_open("soh", orte_soh_base.soh_output,
                               mca_soh_base_static_components,
                               &orte_soh_base.soh_components, true)) {
      return ORTE_ERROR;
  }

  /* All done */

  return ORTE_SUCCESS;
}
