/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/mca/base/mca_base_param.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/class/opal_pointer_array.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"

/* need the data type support functions here */
#include "orte/runtime/data_type_support/orte_dt_support.h"

/* include the class instance definitions */
#include "orte/runtime/orte_globals_class_instances.h"

/* globals used by RTE */
bool orte_timing;
bool orte_debug_flag, orte_debug_daemons_flag, orte_debug_daemons_file_flag;
bool orted_spin_flag;
int orte_debug_output = -1;
bool orte_daemon_died = false;
char **orte_launch_environ;
char **orted_cmd_line=NULL;
int orte_exit, orteds_exit;
int orte_exit_status = 0;
bool orte_abnormal_term_ordered = false;
bool orte_wakeup_ordered = false;
bool orte_abort_in_progress = false;
int orte_timeout_usec_per_proc;
float orte_max_timeout;
char *orte_default_hostfile;

orte_process_name_t orte_globals_name_wildcard = {ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD};
orte_process_name_t orte_globals_name_invalid = {ORTE_JOBID_INVALID, ORTE_VPID_INVALID}; 

/* global arrays for data storage */
opal_pointer_array_t *orte_job_data;
opal_pointer_array_t *orte_node_pool;

/*
 * Whether we have completed orte_init or we are in orte_finalize
 */
bool orte_initialized = false;
bool orte_finalizing = false;

/* whether we have registered params or not */
static bool params_set = false;

int orte_register_params(void)
{
    int value;
    bool orte_debug_flag;
    int orte_debug_verbosity;
    
    if (params_set) {
        return ORTE_SUCCESS;
    }
    
    /* set default output */
    orte_debug_output = -1;
    
    mca_base_param_reg_int_name("orte", "debug",
                                "Top-level ORTE debug switch (default verbosity: 1)",
                                false, false, (int)false, &value);
    orte_debug_flag = OPAL_INT_TO_BOOL(value);
    
    mca_base_param_reg_int_name("orte", "debug_verbose",
                                "Verbosity level for ORTE debug messages (default: 1)",
                                false, false, -1, &orte_debug_verbosity);
    
   mca_base_param_reg_int_name("orte", "debug_daemons",
                                "Whether to debug the ORTE daemons or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_flag = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("orte", "debug_daemons_file",
                                "Whether want stdout/stderr of daemons to go to a file or not",
                                false, false, (int)false, &value);
    orte_debug_daemons_file_flag = OPAL_INT_TO_BOOL(value);
    /* If --debug-daemons-file was specified, that also implies
        --debug-daemons */
    if (orte_debug_daemons_file_flag) {
        orte_debug_daemons_flag = true;
    }
    
    /* open up the verbose output for ORTE debugging */
    if (orte_debug_flag || 0 < orte_debug_verbosity ||
        (orte_debug_daemons_flag && (orte_process_info.daemon || orte_process_info.hnp))) {
        orte_debug_output = opal_output_open(NULL);
        if (0 < orte_debug_verbosity) {
            opal_output_set_verbosity(orte_debug_output, orte_debug_verbosity);
        } else {
            opal_output_set_verbosity(orte_debug_output, 1);
        }
    }
    
    mca_base_param_reg_int_name("orted", "spin",
                                "Have any orteds spin until we can connect a debugger to them",
                                false, false, (int)false, &value);
    orted_spin_flag = OPAL_INT_TO_BOOL(value);

    /* check for timing requests */
    mca_base_param_reg_int_name("orte", "timing",
                                "Request that critical timing loops be measured",
                                false, false, (int)false, &value);
    orte_timing = OPAL_INT_TO_BOOL(value);
    
    /* User-level debugger info string */

    mca_base_param_reg_string_name("orte", "base_user_debugger",
                                   "Sequence of user-level debuggers to search for in orterun",
                                   false, false, "totalview @mpirun@ -a @mpirun_args@ : ddt -n @np@ -start @executable@ @executable_argv@ @single_app@ : fxp @mpirun@ -a @mpirun_args@", NULL);


    mca_base_param_reg_int_name("orte", "abort_timeout",
                                "Max time to wait [in secs] before aborting an ORTE operation (default: 1sec)",
                                false, false, 1, &value);
    orte_max_timeout = 1000000.0 * value;  /* convert to usec */

    mca_base_param_reg_int_name("orte", "timeout_step",
                                "Time to wait [in usecs/proc] before aborting an ORTE operation (default: 100 usec/proc)",
                                false, false, 100, &orte_timeout_usec_per_proc);
    
    /* default hostfile */
    mca_base_param_reg_string_name("default", "hostfile",
                                   "Name of the default hostfile (relative or absolute path)",
                                   false, false, NULL, &orte_default_hostfile);
    
    
    /* All done */
    params_set = true;
    return ORTE_SUCCESS;
}

int orte_dt_init(void)
{
    int rc;
    opal_data_type_t tmp;
    
    /** register the base system types with the DSS */
    tmp = ORTE_STD_CNTR;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_std_cntr,
                                                     orte_dt_unpack_std_cntr,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_std_cntr,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_std_cntr,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_STD_CNTR", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_NAME;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_name,
                                                     orte_dt_unpack_name,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_name,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_name,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_print_name,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_NAME", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_VPID;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_vpid,
                                                     orte_dt_unpack_vpid,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_vpid,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_vpid,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_VPID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_JOBID;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_jobid,
                                                     orte_dt_unpack_jobid,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_jobid,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_jobid,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_JOBID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    tmp = ORTE_JOB;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_job,
                                                     orte_dt_unpack_job,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_job,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_job,
                                                     (opal_dss_size_fn_t)orte_dt_size_job,
                                                     (opal_dss_print_fn_t)orte_dt_print_job,
                                                     (opal_dss_release_fn_t)orte_dt_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "ORTE_JOB", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_NODE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_node,
                                                     orte_dt_unpack_node,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_node,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_node,
                                                     (opal_dss_size_fn_t)orte_dt_size_node,
                                                     (opal_dss_print_fn_t)orte_dt_print_node,
                                                     (opal_dss_release_fn_t)orte_dt_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "ORTE_NODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_PROC;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_proc,
                                                     orte_dt_unpack_proc,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_proc,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_proc,
                                                     (opal_dss_size_fn_t)orte_dt_size_proc,
                                                     (opal_dss_print_fn_t)orte_dt_print_proc,
                                                     (opal_dss_release_fn_t)orte_dt_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "ORTE_PROC", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_APP_CONTEXT;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_app_context,
                                                     orte_dt_unpack_app_context,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_app_context,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_app_context,
                                                     (opal_dss_size_fn_t)orte_dt_size_app_context,
                                                     (opal_dss_print_fn_t)orte_dt_print_app_context,
                                                     (opal_dss_release_fn_t)orte_dt_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "ORTE_APP_CONTEXT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_NODE_STATE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_node_state,
                                                     orte_dt_unpack_node_state,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_node_state,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_node_state,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_NODE_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_PROC_STATE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_proc_state,
                                                     orte_dt_unpack_proc_state,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_proc_state,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_proc_state,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_PROC_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_JOB_STATE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_job_state,
                                                     orte_dt_unpack_job_state,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_job_state,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_job_state,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_JOB_STATE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_EXIT_CODE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_exit_code,
                                                     orte_dt_unpack_exit_code,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_exit_code,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_exit_code,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_EXIT_CODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_JOB_MAP;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_map,
                                                     orte_dt_unpack_map,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_map,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_map,
                                                     (opal_dss_size_fn_t)orte_dt_size_map,
                                                     (opal_dss_print_fn_t)orte_dt_print_map,
                                                     (opal_dss_release_fn_t)orte_dt_std_obj_release,
                                                     OPAL_DSS_STRUCTURED,
                                                     "ORTE_JOB_MAP", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_RML_TAG;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_tag,
                                                      orte_dt_unpack_tag,
                                                      (opal_dss_copy_fn_t)orte_dt_copy_tag,
                                                      (opal_dss_compare_fn_t)orte_dt_compare_tags,
                                                      (opal_dss_size_fn_t)orte_dt_std_size,
                                                      (opal_dss_print_fn_t)orte_dt_std_print,
                                                      (opal_dss_release_fn_t)orte_dt_std_release,
                                                      OPAL_DSS_UNSTRUCTURED,
                                                      "ORTE_RML_TAG", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_DAEMON_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_daemon_cmd,
                                                     orte_dt_unpack_daemon_cmd,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_daemon_cmd,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_daemon_cmd,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_DAEMON_CMD", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GRPCOMM_MODE;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_grpcomm_mode,
                                                     orte_dt_unpack_grpcomm_mode,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_grpcomm_mode,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_grpcomm_mode,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_GRPCOMM_MODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;    
}

int orte_hnp_globals_init(void)
{
    int rc;

    orte_job_data = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (rc = opal_pointer_array_init(orte_job_data,
                                                      1,
                                                      ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                      1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    orte_node_pool = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (rc = opal_pointer_array_init(orte_node_pool,
                                                      ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                                      ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                      ORTE_GLOBAL_ARRAY_BLOCK_SIZE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

orte_job_t* orte_get_job_data_object(orte_jobid_t job)
{
    orte_job_t **jptr;
    orte_std_cntr_t i;
    
    /* if I am not an HNP, I cannot provide this object */
    if (!orte_process_info.hnp) {
        return NULL;
    }
    
    jptr = (orte_job_t**)orte_job_data->addr;
    for (i=0; i < orte_job_data->size; i++) {
        if (NULL != jptr[i] && job == jptr[i]->jobid) {
            return jptr[i];
        }
    }
    
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    return NULL;
}
