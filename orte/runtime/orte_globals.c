/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
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

#include "opal/mca/db/db.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_value_array.h"
#include "opal/dss/dss.h"
#include "opal/threads/threads.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/runtime_internals.h"
#include "orte/runtime/orte_globals.h"

/* need the data type support functions here */
#include "orte/runtime/data_type_support/orte_dt_support.h"

/* State Machine */
opal_list_t orte_job_states;
opal_list_t orte_proc_states;

/* a clean output channel without prefix */
int orte_clean_output = -1;

/* globals used by RTE */
bool orte_timing;
FILE *orte_timing_output = NULL;
bool orte_timing_details;
bool orte_debug_daemons_file_flag = false;
bool orte_leave_session_attached;
bool orte_do_not_launch = false;
bool orted_spin_flag = false;
char *orte_local_cpu_type = NULL;
char *orte_local_cpu_model = NULL;
char *orte_basename = NULL;
bool orte_coprocessors_detected = false;
opal_hash_table_t *orte_coprocessors = NULL;

/* ORTE OOB port flags */
bool orte_static_ports = false;
char *orte_oob_static_ports = NULL;
bool orte_standalone_operation = false;

bool orte_keep_fqdn_hostnames = false;
bool orte_have_fqdn_allocation = false;
bool orte_show_resolved_nodenames;
bool orte_retain_aliases;
int orte_use_hostname_alias;

int orted_debug_failure;
int orted_debug_failure_delay;
bool orte_homogeneous_nodes = false;
bool orte_hetero_apps = false;
bool orte_hetero_nodes = false;
bool orte_never_launched = false;
bool orte_devel_level_output = false;
bool orte_display_topo_with_map = false;
bool orte_display_diffable_output = false;

char **orte_launch_environ;

bool orte_hnp_is_allocated = false;
bool orte_allocation_required;
bool orte_managed_allocation = false;
char *orte_set_slots = NULL;
bool orte_display_allocation;
bool orte_display_devel_allocation;
bool orte_soft_locations = false;

/* launch agents */
char *orte_launch_agent = NULL;
char **orted_cmd_line=NULL;
char **orte_fork_agent=NULL;

/* debugger job */
bool orte_debugger_dump_proctable;
char *orte_debugger_test_daemon;
bool orte_debugger_test_attach;
int orte_debugger_check_rate;

/* exit flags */
int orte_exit_status = 0;
bool orte_abnormal_term_ordered = false;
bool orte_routing_is_enabled = true;
bool orte_job_term_ordered = false;
bool orte_orteds_term_ordered = false;
bool orte_allowed_exit_without_sync = false;

int orte_startup_timeout;
int orte_timeout_usec_per_proc;
float orte_max_timeout;
orte_timer_t *orte_mpiexec_timeout = NULL;

opal_buffer_t *orte_tree_launch_cmd = NULL;

/* global arrays for data storage */
opal_pointer_array_t *orte_job_data;
opal_pointer_array_t *orte_node_pool;
opal_pointer_array_t *orte_node_topologies;
opal_pointer_array_t *orte_local_children;
orte_vpid_t orte_total_procs = 0;

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

/* allocation specification */
char *orte_default_hostfile = NULL;
bool orte_default_hostfile_given = false;
char *orte_rankfile = NULL;
int orte_num_allocated_nodes = 0;
char *orte_node_regex = NULL;

/* tool communication controls */
bool orte_report_events = false;
char *orte_report_events_uri = NULL;

/* report bindings */
bool orte_report_bindings = false;

/* barrier control */
bool orte_do_not_barrier = false;

/* process recovery */
bool orte_enable_recovery;
int32_t orte_max_restarts;

/* exit status reporting */
bool orte_report_child_jobs_separately;
struct timeval orte_child_time_to_exit;
bool orte_abort_non_zero_exit;

/* length of stat history to keep */
int orte_stat_history_size;

/* envars to forward */
char *orte_forward_envars = NULL;
char **orte_forwarded_envars = NULL;

/* map-reduce mode */
bool orte_map_reduce = false;
bool orte_staged_execution = false;

/* map stddiag output to stderr so it isn't forwarded to mpirun */
bool orte_map_stddiag_to_stderr = false;

/* maximum size of virtual machine - used to subdivide allocation */
int orte_max_vm_size = -1;

/* progress thread */
opal_thread_t orte_progress_thread;

/* global nidmap/pidmap for daemons to give to apps */
opal_byte_object_t orte_nidmap;
opal_byte_object_t orte_pidmap;

/* user debugger */
char *orte_base_user_debugger = NULL;

int orte_debug_output = -1;
bool orte_debug_daemons_flag = false;
bool orte_xml_output = false;
FILE *orte_xml_fp = NULL;
char *orte_job_ident = NULL;
bool orte_execute_quiet = false;
bool orte_report_silent_errors = false;

/* See comment in orte/tools/orterun/debuggers.c about this MCA
   param */
bool orte_in_parallel_debugger = false;

char *orte_daemon_cores = NULL;

int orte_dt_init(void)
{
    int rc;
    opal_data_type_t tmp;

    /* set default output */
    orte_debug_output = opal_output_open(NULL);
    
    /* open up the verbose output for ORTE debugging */
    if (orte_debug_flag || 0 < orte_debug_verbosity ||
        (orte_debug_daemons_flag && (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP))) {
        if (0 < orte_debug_verbosity) {
            opal_output_set_verbosity(orte_debug_output, orte_debug_verbosity);
        } else {
            opal_output_set_verbosity(orte_debug_output, 1);
        }
    }

    /** register the base system types with the DSS */
    tmp = ORTE_STD_CNTR;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_std_cntr,
                                                     orte_dt_unpack_std_cntr,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_std_cntr,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_std_cntr,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_name,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_job,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_node,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_proc,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_app_context,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_print_map,
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
                                                      (opal_dss_print_fn_t)orte_dt_std_print,
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
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_DAEMON_CMD", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_IOF_TAG;
    if (ORTE_SUCCESS != (rc = opal_dss.register_type(orte_dt_pack_iof_tag,
                                                     orte_dt_unpack_iof_tag,
                                                     (opal_dss_copy_fn_t)orte_dt_copy_iof_tag,
                                                     (opal_dss_compare_fn_t)orte_dt_compare_iof_tag,
                                                     (opal_dss_print_fn_t)orte_dt_std_print,
                                                     OPAL_DSS_UNSTRUCTURED,
                                                     "ORTE_IOF_TAG", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;    
}

orte_job_t* orte_get_job_data_object(orte_jobid_t job)
{
    int32_t ljob;
    
    /* if the job data wasn't setup, we cannot provide the data */
    if (NULL == orte_job_data) {
        return NULL;
    }
    
    /* the job is indexed by its local jobid, so we can
     * just look it up here. it is not an error for this
     * to not be found - could just be
     * a race condition whereby the job has already been
     * removed from the array. The get_item function
     * will just return NULL in that case.
     */
    ljob = ORTE_LOCAL_JOBID(job);
    return (orte_job_t*)opal_pointer_array_get_item(orte_job_data, ljob);
}

orte_proc_t* orte_get_proc_object(orte_process_name_t *proc)
{
    orte_job_t *jdata;
    orte_proc_t *proct;

    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        return NULL;
    }
    proct = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
    return proct;
}

orte_vpid_t orte_get_proc_daemon_vpid(orte_process_name_t *proc)
{
    orte_job_t *jdata;
    orte_proc_t *proct;

    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        return ORTE_VPID_INVALID;
    }
    if (NULL == (proct = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid))) {
        return ORTE_VPID_INVALID;
    }
    if (NULL == proct->node || NULL == proct->node->daemon) {
        return ORTE_VPID_INVALID;
    }
    return proct->node->daemon->name.vpid;
}

char* orte_get_proc_hostname(orte_process_name_t *proc)
{
    orte_proc_t *proct;
    char *hostname;
    int rc;

    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        /* look it up on our arrays */
        if (NULL == (proct = orte_get_proc_object(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        if (NULL == proct->node || NULL == proct->node->name) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        return proct->node->name;
    }

    /* if we are an app, get the pointer from the modex db */
    if (ORTE_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)proc,
                                                    ORTE_DB_HOSTNAME,
                                                    (void**)&hostname, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    return hostname;
}

orte_node_rank_t orte_get_proc_node_rank(orte_process_name_t *proc)
{
    orte_proc_t *proct;
    orte_node_rank_t noderank, *nr;
    int rc;

    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        /* look it up on our arrays */
        if (NULL == (proct = orte_get_proc_object(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_NODE_RANK_INVALID;
        }
        return proct->node_rank;
    }

    /* if we are an app, get the value from the modex db */
    nr = &noderank;
    if (ORTE_SUCCESS != (rc = opal_db.fetch_pointer((opal_identifier_t*)proc,
                                                    ORTE_DB_NODERANK,
                                                    (void**)&nr, ORTE_NODE_RANK))) {
        ORTE_ERROR_LOG(rc);
        return ORTE_NODE_RANK_INVALID;
    }
    return noderank;
}

orte_vpid_t orte_get_lowest_vpid_alive(orte_jobid_t job)
{
    int i;
    orte_job_t *jdata;
    orte_proc_t *proc;

    if (NULL == (jdata = orte_get_job_data_object(job))) {
        return ORTE_VPID_INVALID;
    }

    if (ORTE_PROC_IS_DAEMON &&
        ORTE_PROC_MY_NAME->jobid == job &&
        NULL != orte_process_info.my_hnp_uri) {
        /* if we were started by an HNP, then the lowest vpid
         * is always 1
         */
        return 1;
    }

    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        if (proc->state == ORTE_PROC_STATE_RUNNING) {
            /* must be lowest one alive */
            return proc->name.vpid;
        }
    }
    /* only get here if no live proc found */
    return ORTE_VPID_INVALID;
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
    OBJ_CONSTRUCT(&app_context->procs, opal_pointer_array_t);
    opal_pointer_array_init(&app_context->procs,
                            1,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            16);
    app_context->state = ORTE_APP_STATE_UNDEF;
    app_context->first_rank = 0;
    app_context->argv=NULL;
    app_context->env=NULL;
    app_context->cwd=NULL;
    app_context->user_specified_cwd=false;
    app_context->set_cwd_to_session_dir = false;
    app_context->hostfile=NULL;
    app_context->add_hostfile=NULL;
    app_context->add_host = NULL;
    app_context->dash_host = NULL;
    OBJ_CONSTRUCT(&app_context->resource_constraints, opal_list_t);
    app_context->prefix_dir = NULL;
    app_context->preload_binary = false;
    app_context->preload_files  = NULL;
    app_context->used_on_node = false;

#if OPAL_ENABLE_FT_CR == 1
    app_context->sstore_load = NULL;
#endif
    app_context->recovery_defined = false;
    app_context->max_restarts = -1000;
    app_context->max_procs_per_node = 0;
    app_context->mandatory = false;
    app_context->min_number_of_nodes = -1;  /* no minimum */
}

static void orte_app_context_destructor(orte_app_context_t* app_context)
{
    opal_list_item_t *item;
    int i;
    orte_proc_t *proc;

    if (NULL != app_context->app) {
        free (app_context->app);
        app_context->app = NULL;
    }
    
    for (i=0; i < app_context->procs.size; i++) {
        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(&app_context->procs, i))) {
            OBJ_RELEASE(proc);
        }
    }
    OBJ_DESTRUCT(&app_context->procs);

    /* argv and env lists created by util/argv copy functions */
    if (NULL != app_context->argv) {
        opal_argv_free(app_context->argv);
        app_context->argv = NULL;
    }
    
    if (NULL != app_context->env) {
        opal_argv_free(app_context->env);
        app_context->env = NULL;
    }
    
    if (NULL != app_context->cwd) {
        free (app_context->cwd);
        app_context->cwd = NULL;
    }
    
    if (NULL != app_context->hostfile) {
        free(app_context->hostfile);
        app_context->hostfile = NULL;
    }
    
    if (NULL != app_context->add_hostfile) {
        free(app_context->add_hostfile);
        app_context->add_hostfile = NULL;
    }
    
    if (NULL != app_context->add_host) {
        opal_argv_free(app_context->add_host);
        app_context->add_host = NULL;
    }
    
    if (NULL != app_context->dash_host) {
        opal_argv_free(app_context->dash_host);
        app_context->dash_host = NULL;
    }
    
    while (NULL != (item = opal_list_remove_first(&app_context->resource_constraints))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&app_context->resource_constraints);

    if (NULL != app_context->prefix_dir) {
        free(app_context->prefix_dir);
        app_context->prefix_dir = NULL;
    }
    
    app_context->preload_binary = false;

    if(NULL != app_context->preload_files) {
        free(app_context->preload_files);
        app_context->preload_files = NULL;
    }
    
#if OPAL_ENABLE_FT_CR == 1
    if( NULL != app_context->sstore_load ) {
        free(app_context->sstore_load);
        app_context->sstore_load = NULL;
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_app_context_t,
                   opal_object_t,
                   orte_app_context_construct,
                   orte_app_context_destructor);

static void orte_job_construct(orte_job_t* job)
{
    job->jobid = ORTE_JOBID_INVALID;
    job->offset = 0;
    job->updated = true;
    job->apps = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->apps,
                            1,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            2);
    job->num_apps = 0;
    job->controls = ORTE_JOB_CONTROL_FORWARD_OUTPUT;
    job->failure_timer = NULL;
    job->gang_launched = true;
    job->stdin_target = ORTE_VPID_INVALID;
    job->stdout_target = ORTE_JOBID_INVALID;
    job->total_slots_alloc = 0;
    job->num_procs = 0;
    job->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    job->map = NULL;
    job->bookmark = NULL;
    job->state = ORTE_JOB_STATE_UNDEF;
    job->restart = false;

    job->num_mapped = 0;
    job->num_launched = 0;
    job->num_reported = 0;
    job->num_terminated = 0;
    job->num_daemons_reported = 0;
    job->num_non_zero_exit = 0;
    job->abort = false;
    job->aborted_proc = NULL;
    
    job->originator.jobid = ORTE_JOBID_INVALID;
    job->originator.vpid = ORTE_VPID_INVALID;

    job->recovery_defined = false;
    job->enable_recovery = false;
    job->num_local_procs = 0;

    job->file_maps = NULL;

#if OPAL_ENABLE_FT_CR == 1
    job->ckpt_state = 0;
    job->ckpt_snapshot_ref = NULL;
    job->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_job_destruct(orte_job_t* job)
{
    orte_proc_t *proc;
    orte_app_context_t *app;
    orte_job_t *jdata;
    int n;

    if (NULL == job) {
        /* probably just a race condition - just return */
        return;
    }
    
    if (orte_debug_flag) {
        opal_output(0, "%s Releasing job data for %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(job->jobid));
    }
    
    for (n=0; n < job->apps->size; n++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(job->apps, n))) {
            continue;
        }
        OBJ_RELEASE(app);
    }
    OBJ_RELEASE(job->apps);
    
    if (NULL != job->failure_timer) {
        OBJ_RELEASE(job->failure_timer);
    }

    if (NULL != job->map) {
        OBJ_RELEASE(job->map);
        job->map = NULL;
    }
    
    for (n=0; n < job->procs->size; n++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(job->procs, n))) {
            continue;
        }
        OBJ_RELEASE(proc);
    }
    OBJ_RELEASE(job->procs);
    
    if (NULL != job->file_maps) {
        OBJ_RELEASE(job->file_maps);
    }

#if OPAL_ENABLE_FT_CR == 1
    if (NULL != job->ckpt_snapshot_ref) {
        free(job->ckpt_snapshot_ref);
        job->ckpt_snapshot_ref = NULL;
    }
    if (NULL != job->ckpt_snapshot_loc) {
        free(job->ckpt_snapshot_loc);
        job->ckpt_snapshot_loc = NULL;
    }
#endif
    
    /* find the job in the global array */
    if (NULL != orte_job_data) {
        for (n=0; n < orte_job_data->size; n++) {
            if (NULL == (jdata = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, n))) {
                continue;
            }
            if (jdata->jobid == job->jobid) {
                /* set the entry to NULL */
                opal_pointer_array_set_item(orte_job_data, n, NULL);
                break;
            }
        }
    }
}

OBJ_CLASS_INSTANCE(orte_job_t,
                   opal_list_item_t,
                   orte_job_construct,
                   orte_job_destruct);


static void orte_node_construct(orte_node_t* node)
{
    node->index = -1;
    node->name = NULL;
    node->alias = NULL;
    node->serial_number = NULL;
    node->hostid = ORTE_VPID_INVALID;
    node->daemon = NULL;
    node->daemon_launched = false;
    node->location_verified = false;
    node->launch_id = -1;
    node->mapped = false;

    node->num_procs = 0;
    node->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(node->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    node->next_node_rank = 0;
    
    node->oversubscribed = false;
    node->state = ORTE_NODE_STATE_UNKNOWN;
    node->slots = 0;
    node->slots_given = false;
    node->slots_inuse = 0;
    node->slots_max = 0;
    
    node->username = NULL;
    
#if OPAL_HAVE_HWLOC
    node->topology = NULL;
#endif

    OBJ_CONSTRUCT(&node->stats, opal_ring_buffer_t);
    opal_ring_buffer_init(&node->stats, orte_stat_history_size);
}

static void orte_node_destruct(orte_node_t* node)
{
    int i;
    opal_node_stats_t *stats;
    orte_proc_t *proc;

    if (NULL != node->name) {
        free(node->name);
        node->name = NULL;
    }

    if (NULL != node->serial_number) {
        free(node->serial_number);
        node->serial_number = NULL;
    }

    if (NULL != node->alias) {
        opal_argv_free(node->alias);
        node->alias = NULL;
    }
    
    if (NULL != node->daemon) {
        node->daemon->node = NULL;
        OBJ_RELEASE(node->daemon);
        node->daemon = NULL;
    }
    
    for (i=0; i < node->procs->size; i++) {
        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
            opal_pointer_array_set_item(node->procs, i, NULL);
            OBJ_RELEASE(proc);
        }
    }
    OBJ_RELEASE(node->procs);
    
    /* we release the topology elsewhere */

    if (NULL != node->username) {
        free(node->username);
        node->username = NULL;
    }
    
    /* do NOT destroy the topology */

    while (NULL != (stats = (opal_node_stats_t*)opal_ring_buffer_pop(&node->stats))) {
        OBJ_RELEASE(stats);
    }
    OBJ_DESTRUCT(&node->stats);
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
    proc->app_rank = -1;
    proc->last_errmgr_state = ORTE_PROC_STATE_UNDEF;
    proc->state = ORTE_PROC_STATE_UNDEF;
    proc->alive = false;
    proc->aborted = false;
    proc->updated = true;
    proc->app_idx = 0;
#if OPAL_HAVE_HWLOC
    proc->locale = NULL;
    proc->bind_location = NULL;
    proc->cpu_bitmap = NULL;
#endif
    proc->node = NULL;
    proc->local_proc = false;
    proc->do_not_barrier = false;
    proc->prior_node = NULL;
    proc->nodename = NULL;
    proc->exit_code = 0;      /* Assume we won't fail unless otherwise notified */
    proc->rml_uri = NULL;
    proc->restarts = 0;
    proc->fast_failures = 0;
    proc->last_failure.tv_sec = 0;
    proc->last_failure.tv_usec = 0;
    proc->reported = false;
    proc->beat = 0;
    OBJ_CONSTRUCT(&proc->stats, opal_ring_buffer_t);
    opal_ring_buffer_init(&proc->stats, orte_stat_history_size);
    proc->registered = false;
    proc->mpi_proc = false;
    proc->deregistered = false;
    proc->iof_complete = false;
    proc->waitpid_recvd = false;
#if OPAL_ENABLE_FT_CR == 1
    proc->ckpt_state = 0;
    proc->ckpt_snapshot_ref = NULL;
    proc->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_proc_destruct(orte_proc_t* proc)
{
    opal_pstats_t *stats;

    /* do NOT free the nodename field as this is
     * simply a pointer to a field in the
     * associated node object - the node object
     * will free it
     */
#if OPAL_HAVE_HWLOC
    if (NULL != proc->cpu_bitmap) {
        free(proc->cpu_bitmap);
    }
#endif

    if (NULL != proc->node) {
        OBJ_RELEASE(proc->node);
        proc->node = NULL;
    }
    
    if (NULL != proc->rml_uri) {
        free(proc->rml_uri);
        proc->rml_uri = NULL;
    }

    while (NULL != (stats = (opal_pstats_t*)opal_ring_buffer_pop(&proc->stats))) {
        OBJ_RELEASE(stats);
    }
    OBJ_DESTRUCT(&proc->stats);

#if OPAL_ENABLE_FT_CR == 1
    if (NULL != proc->ckpt_snapshot_ref) {
        free(proc->ckpt_snapshot_ref);
        proc->ckpt_snapshot_ref = NULL;
    }
    if (NULL != proc->ckpt_snapshot_loc) {
        free(proc->ckpt_snapshot_loc);
        proc->ckpt_snapshot_loc = NULL;
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_proc_t,
                   opal_list_item_t,
                   orte_proc_construct,
                   orte_proc_destruct);

static void orte_job_map_construct(orte_job_map_t* map)
{
    map->req_mapper = NULL;
    map->last_mapper = NULL;
    map->mapping = 0;
    map->ranking = 0;
#if OPAL_HAVE_HWLOC
    map->binding = 0;
#endif
    map->ppr = NULL;
    map->cpus_per_rank = 1;
    map->display_map = false;
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
    orte_node_t *node;

    if (NULL != map->req_mapper) {
        free(map->req_mapper);
    }
    if (NULL != map->last_mapper) {
        free(map->last_mapper);
    }
    if (NULL != map->ppr) {
        free(map->ppr);
    }
    for (i=0; i < map->nodes->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            OBJ_RELEASE(node);
            opal_pointer_array_set_item(map->nodes, i, NULL);
        }
    }
    OBJ_RELEASE(map->nodes);
}

OBJ_CLASS_INSTANCE(orte_job_map_t,
                   opal_object_t,
                   orte_job_map_construct,
                   orte_job_map_destruct);
