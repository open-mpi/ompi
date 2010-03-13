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
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
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
#include "opal/mca/paffinity/paffinity.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/show_help.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/runtime_internals.h"
#include "orte/runtime/orte_globals.h"

#if !ORTE_DISABLE_FULL_SUPPORT

/* need the data type support functions here */
#include "orte/runtime/data_type_support/orte_dt_support.h"

/* globals used by RTE */
bool orte_timing;
bool orte_debug_daemons_file_flag = false;
bool orte_leave_session_attached;
bool orte_do_not_launch = false;
bool orted_spin_flag = false;
bool orte_static_ports = false;
bool orte_keep_fqdn_hostnames = false;
bool orte_show_resolved_nodenames;
int orted_debug_failure;
int orted_debug_failure_delay;
bool orte_homogeneous_nodes = false;
bool orte_hetero_apps = false;
bool orte_devel_level_output = false;

int32_t orte_contiguous_nodes;
char **orte_launch_environ;
opal_pointer_array_t orte_daemonmap;

bool orte_hnp_is_allocated = false;
bool orte_allocation_required;

char *orte_launch_agent;
char **orted_cmd_line=NULL;

orte_trigger_event_t orte_exit, orteds_exit;
int orte_exit_status = 0;
bool orte_abnormal_term_ordered = false;
bool orte_shutdown_in_progress = false;
bool orte_job_term_ordered = false;

int orte_heartbeat_rate;
int orte_startup_timeout;

int orte_timeout_usec_per_proc;
float orte_max_timeout;

opal_buffer_t *orte_tree_launch_cmd = NULL;

/* global arrays for data storage */
opal_pointer_array_t *orte_job_data;
opal_pointer_array_t *orte_node_pool;

/* a clean output channel without prefix */
int orte_clean_output = -1;

/* list of local children on a daemon */
opal_list_t orte_local_children;
/* list of job data for local children on a daemon */
opal_list_t orte_local_jobdata;

/* IOF controls */
bool orte_tag_output;
bool orte_timestamp_output;
char *orte_output_filename;
/* generate new xterm windows to display output from specified ranks */
char *orte_xterm;

/* whether or not to forward SIGTSTP and SIGCONT signals */
bool orte_forward_job_control;

/* report launch progress */
bool orte_report_launch_progress = false;

/* cluster hardware info */
uint8_t orte_default_num_boards;
uint8_t orte_default_num_sockets_per_board;
uint8_t orte_default_num_cores_per_socket;

/* allocation specification */
char *orte_default_cpu_set;
char *orte_default_hostfile = NULL;
char *orte_rankfile;
#ifdef __WINDOWS__
char *orte_ccp_headnode;
#endif

/* default rank assigment and binding policy */
orte_mapping_policy_t orte_default_mapping_policy = 0;

/* basename of the orte/mpirun cmd */
char *orte_cmd_basename = NULL;

#endif /* !ORTE_DISABLE_FULL_RTE */

int orte_debug_output = -1;
bool orte_debug_daemons_flag = false;
bool orte_xml_output = false;
FILE *orte_xml_fp = NULL;
char *orte_job_ident = NULL;

/* See comment in orte/tools/orterun/debuggers.c about this MCA
   param */
bool orte_in_parallel_debugger = false;


int orte_dt_init(void)
{
    int rc;
    opal_data_type_t tmp;

    /* set default output */
    orte_debug_output = opal_output_open(NULL);
    
    /* open up the verbose output for ORTE debugging */
    if (orte_debug_flag || 0 < orte_debug_verbosity ||
        (orte_debug_daemons_flag && (orte_process_info.daemon || orte_process_info.hnp))) {
        if (0 < orte_debug_verbosity) {
            opal_output_set_verbosity(orte_debug_output, orte_debug_verbosity);
        } else {
            opal_output_set_verbosity(orte_debug_output, 1);
        }
    }

#if !ORTE_DISABLE_FULL_SUPPORT
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

    /* get a clean output channel too */
    {
        opal_output_stream_t lds;
        OBJ_CONSTRUCT(&lds, opal_output_stream_t);
        lds.lds_want_stdout = true;
        orte_clean_output = opal_output_open(&lds);
        OBJ_DESTRUCT(&lds);
        
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

    tmp = ORTE_IOF_TAG;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_iof_tag,
                                                     orte_dt_unpack_iof_tag,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_iof_tag,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_iof_tag,
                                                     (opal_dss_size_fn_t)orte_dt_std_size,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     (opal_dss_release_fn_t)orte_dt_std_release,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_IOF_TAG", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif /* !ORTE_DISABLE_FULL_SUPPORT */
    
    return ORTE_SUCCESS;    
}

#if !ORTE_DISABLE_FULL_SUPPORT
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


/*
 * CONSTRUCTORS, DESTRUCTORS, AND CLASS INSTANTIATIONS
 * FOR ORTE CLASSES
 */

static void orte_app_context_construct(orte_app_context_t* app_context)
{
    app_context->idx=0;
    app_context->app=NULL;
    app_context->num_procs=0;
    app_context->argv=NULL;
    app_context->env=NULL;
    app_context->cwd=NULL;
    app_context->user_specified_cwd=false;
    app_context->hostfile=NULL;
    app_context->add_hostfile=NULL;
    app_context->dash_host = NULL;
    app_context->prefix_dir = NULL;
    app_context->preload_binary = false;
    app_context->preload_files  = NULL;
    app_context->preload_files_dest_dir  = NULL;
    app_context->used_on_node = false;
}

static void orte_app_context_destructor(orte_app_context_t* app_context)
{
    if (NULL != app_context->app) {
        free (app_context->app);
    }
    
    /* argv and env lists created by util/argv copy functions */
    if (NULL != app_context->argv) {
        opal_argv_free(app_context->argv);
    }
    
    if (NULL != app_context->env) {
        opal_argv_free(app_context->env);
    }
    
    if (NULL != app_context->cwd) {
        free (app_context->cwd);
    }
    
    if (NULL != app_context->hostfile) {
        free(app_context->hostfile);
    }
    
    if (NULL != app_context->add_hostfile) {
        free(app_context->add_hostfile);
    }
    
    if (NULL != app_context->dash_host) {
        opal_argv_free(app_context->dash_host); 
    }
    
    if (NULL != app_context->prefix_dir) {
        free(app_context->prefix_dir);
    }
    
    app_context->preload_binary = false;
    
    if(NULL != app_context->preload_files) {
        free(app_context->preload_files);
    }
    
    if(NULL != app_context->preload_files_dest_dir) {
        free(app_context->preload_files_dest_dir);
    }
}

OBJ_CLASS_INSTANCE(orte_app_context_t,
                   opal_object_t,
                   orte_app_context_construct,
                   orte_app_context_destructor);

static void orte_job_construct(orte_job_t* job)
{
    job->jobid = ORTE_JOBID_INVALID;
    job->apps = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->apps,
                            1,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            2);
    job->num_apps = 0;
    job->controls = ORTE_JOB_CONTROL_FORWARD_OUTPUT;
    job->stdin_target = ORTE_VPID_INVALID;
    job->total_slots_alloc = 0;
    job->num_procs = 0;
    job->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    
    job->map = NULL;
    job->bookmark = NULL;
    job->oversubscribe_override = false;
    job->state = ORTE_JOB_STATE_UNDEF;

    job->num_launched = 0;
    job->num_reported = 0;
    job->num_terminated = 0;
    job->abort = false;
    job->aborted_proc = NULL;

#if OPAL_ENABLE_FT == 1
    job->ckpt_state = 0;
    job->ckpt_snapshot_ref = NULL;
    job->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_job_destruct(orte_job_t* job)
{
    orte_std_cntr_t i;
    orte_vpid_t j;
    
    for (i=0; i < job->num_apps; i++) {
        if (NULL != job->apps->addr[i]) OBJ_RELEASE(job->apps->addr[i]);
    }
    OBJ_RELEASE(job->apps);
    
    for (j=0; j < job->num_procs; j++) {
        if (NULL != job->procs->addr[j]) OBJ_RELEASE(job->procs->addr[j]);
    }
    OBJ_RELEASE(job->procs);
    
    if (NULL != job->map) OBJ_RELEASE(job->map);

#if OPAL_ENABLE_FT == 1
    if (NULL != job->ckpt_snapshot_ref) {
        free(job->ckpt_snapshot_ref);
    }
    if (NULL != job->ckpt_snapshot_loc) {
        free(job->ckpt_snapshot_loc);
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_job_t,
                   opal_list_item_t,
                   orte_job_construct,
                   orte_job_destruct);


static void orte_node_construct(orte_node_t* node)
{
    node->name = NULL;
    node->alias = NULL;
    node->index = -1;
    node->daemon = NULL;
    node->daemon_launched = false;
    node->launch_id = -1;

    node->num_procs = 0;
    node->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(node->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    node->next_node_rank = 0;
    
    node->oversubscribed = false;
    node->arch = 0;
    node->state = ORTE_NODE_STATE_UNKNOWN;
    node->slots = 0;
    node->slots_inuse = 0;
    node->slots_alloc = 0;
    node->slots_max = 0;

    node->boards = orte_default_num_boards;
    node->sockets_per_board = orte_default_num_sockets_per_board;
    node->cores_per_socket = orte_default_num_cores_per_socket;
    if (NULL != orte_default_cpu_set) {
        node->cpu_set = strdup(orte_default_cpu_set);
    } else {
        node->cpu_set = NULL;
    }
    
    node->username = NULL;
    node->slot_list = NULL;
}

static void orte_node_destruct(orte_node_t* node)
{
    orte_vpid_t i;
    
    if (NULL != node->name) {
        free(node->name);
    }

    if (NULL != node->alias) {
        opal_argv_free(node->alias);
    }
    
    if (NULL != node->daemon) OBJ_RELEASE(node->daemon);
    
    for (i=0; i < node->num_procs; i++) {
        if (NULL != node->procs->addr[i]) OBJ_RELEASE(node->procs->addr[i]);
    }
    OBJ_RELEASE(node->procs);
    
    if (NULL != node->cpu_set) {
        free(node->cpu_set);
        node->cpu_set = NULL;
    }
    if (NULL != node->username) {
        free(node->username);
    }
}


OBJ_CLASS_INSTANCE(orte_node_t,
                   opal_list_item_t,
                   orte_node_construct,
                   orte_node_destruct);



static void orte_proc_construct(orte_proc_t* proc)
{
    proc->name = *ORTE_NAME_INVALID;
    proc->pid = 0;
    proc->local_rank = ORTE_LOCAL_RANK_INVALID;
    proc->node_rank = ORTE_NODE_RANK_INVALID;
    proc->state = ORTE_PROC_STATE_UNDEF;
    proc->app_idx = -1;
    proc->slot_list = NULL;
    proc->node = NULL;
    proc->nodename = NULL;
    proc->rml_uri = NULL;
    proc->beat = 0;
#if OPAL_ENABLE_FT == 1
    proc->ckpt_state = 0;
    proc->ckpt_snapshot_ref = NULL;
    proc->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_proc_destruct(orte_proc_t* proc)
{
    /* do NOT free the nodename field as this is
     * simply a pointer to a field in the
     * associated node object - the node object
     * will free it
     */
    
    if (NULL != proc->slot_list) {
        free(proc->slot_list);
    }

    if (NULL != proc->node) OBJ_RELEASE(proc->node);
    
    if (NULL != proc->rml_uri) free(proc->rml_uri);
    
#if OPAL_ENABLE_FT == 1
    if (NULL != proc->ckpt_snapshot_ref) {
        free(proc->ckpt_snapshot_ref);
    }
    if (NULL != proc->ckpt_snapshot_loc) {
        free(proc->ckpt_snapshot_loc);
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_proc_t,
                   opal_list_item_t,
                   orte_proc_construct,
                   orte_proc_destruct);

static void orte_nid_construct(orte_nid_t *ptr)
{
    ptr->name = NULL;
    ptr->daemon = ORTE_VPID_INVALID;
    ptr->arch = orte_process_info.arch;
}

static void orte_nid_destruct(orte_nid_t *ptr)
{
    if (NULL != ptr->name) {
        free(ptr->name);
    }
}

OBJ_CLASS_INSTANCE(orte_nid_t,
                   opal_object_t,
                   orte_nid_construct,
                   orte_nid_destruct);

static void orte_pmap_construct(orte_pmap_t *ptr)
{
    ptr->node = -1;
    ptr->local_rank = 0;
    ptr->node_rank = 0;
}

OBJ_CLASS_INSTANCE(orte_pmap_t,
                   opal_object_t,
                   orte_pmap_construct,
                   NULL);


static void orte_jmap_construct(orte_jmap_t *ptr)
{
    ptr->job = ORTE_JOBID_INVALID;
    OBJ_CONSTRUCT(&ptr->pmap, opal_value_array_t);
    opal_value_array_init(&ptr->pmap, sizeof(orte_pmap_t));
}

static void orte_jmap_destruct(orte_jmap_t *ptr)
{
    OBJ_DESTRUCT(&ptr->pmap);
}

OBJ_CLASS_INSTANCE(orte_jmap_t,
                   opal_object_t,
                   orte_jmap_construct,
                   orte_jmap_destruct);



static void orte_job_map_construct(orte_job_map_t* map)
{
    map->policy = 0;
    map->npernode = 0;
    map->nperboard = 0;
    map->npersocket = 0;
    map->cpus_per_rank = 1;
    map->stride = 1;
    map->oversubscribe = true;  /* default to allowing oversubscribe */
    map->display_map = false;
    map->cpu_lists = false;
    map->num_new_daemons = 0;
    map->daemon_vpid_start = ORTE_VPID_INVALID;
    map->num_nodes = 0;
    map->nodes = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(map->nodes,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
}

static void orte_job_map_destruct(orte_job_map_t* map)
{
    orte_std_cntr_t i;
    
    for (i=0; i < map->nodes->size; i++) {
        if (NULL != map->nodes->addr[i]) {
            OBJ_RELEASE(map->nodes->addr[i]);
        }
    }
    OBJ_RELEASE(map->nodes);
}

OBJ_CLASS_INSTANCE(orte_job_map_t,
                   opal_object_t,
                   orte_job_map_construct,
                   orte_job_map_destruct);
#endif
