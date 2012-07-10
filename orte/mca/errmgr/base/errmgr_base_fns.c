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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif /* HAVE_DIRENT_H */
#include <time.h>

#include <stdlib.h>
#include <stdarg.h>

#if WANT_PMI_SUPPORT
#include <pmi.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/proc_info.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

/*
 * Object stuff
 */
void orte_errmgr_predicted_proc_construct(orte_errmgr_predicted_proc_t *item);
void orte_errmgr_predicted_proc_destruct( orte_errmgr_predicted_proc_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_proc_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_proc_construct,
                   orte_errmgr_predicted_proc_destruct);

void orte_errmgr_predicted_proc_construct(orte_errmgr_predicted_proc_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;
}

void orte_errmgr_predicted_proc_destruct( orte_errmgr_predicted_proc_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;
}

void orte_errmgr_predicted_node_construct(orte_errmgr_predicted_node_t *item);
void orte_errmgr_predicted_node_destruct( orte_errmgr_predicted_node_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_node_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_node_construct,
                   orte_errmgr_predicted_node_destruct);

void orte_errmgr_predicted_node_construct(orte_errmgr_predicted_node_t *item)
{
    item->node_name = NULL;
}

void orte_errmgr_predicted_node_destruct( orte_errmgr_predicted_node_t *item)
{
    if( NULL != item->node_name ) {
        free(item->node_name);
        item->node_name = NULL;
    }
}

void orte_errmgr_predicted_map_construct(orte_errmgr_predicted_map_t *item);
void orte_errmgr_predicted_map_destruct( orte_errmgr_predicted_map_t *item);

OBJ_CLASS_INSTANCE(orte_errmgr_predicted_map_t,
                   opal_list_item_t,
                   orte_errmgr_predicted_map_construct,
                   orte_errmgr_predicted_map_destruct);

void orte_errmgr_predicted_map_construct(orte_errmgr_predicted_map_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;

    item->node_name = NULL;

    item->map_proc_name.vpid  = ORTE_VPID_INVALID;
    item->map_proc_name.jobid = ORTE_JOBID_INVALID;

    item->map_node_name = NULL;
    item->off_current_node = false;
    item->pre_map_fixed_node = NULL;
}

void orte_errmgr_predicted_map_destruct( orte_errmgr_predicted_map_t *item)
{
    item->proc_name.vpid  = ORTE_VPID_INVALID;
    item->proc_name.jobid = ORTE_JOBID_INVALID;

    if( NULL != item->node_name ) {
        free(item->node_name);
        item->node_name = NULL;
    }

    item->map_proc_name.vpid  = ORTE_VPID_INVALID;
    item->map_proc_name.jobid = ORTE_JOBID_INVALID;

    if( NULL != item->map_node_name ) {
        free(item->map_node_name);
        item->map_node_name = NULL;
    }

    item->off_current_node = false;

    if( NULL != item->pre_map_fixed_node ) {
        free(item->pre_map_fixed_node);
        item->pre_map_fixed_node = NULL;
    }
}

/*
 * Public interfaces
 */
void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    char *errstring = NULL;

    OPAL_TRACE(1);
    
    errstring = (char*)ORTE_ERROR_NAME(error_code);

    if (NULL == errstring) {
        /* if the error is silent, say nothing */
        return;
    }
    
    opal_output(0, "%s ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                errstring, filename, line);
}

#if WANT_PMI_SUPPORT
/* useful util */
char* orte_errmgr_base_pmi_error(int pmi_err)
{
    char * err_msg;

    switch(pmi_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid keyvalp argument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
#if defined(PMI_ERR_INVALID_KVS)
	/* pmi.h calls this a valid return code but mpich doesn't define it (slurm does). */
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
#endif
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
#endif


void orte_errmgr_base_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, "%s", buffer );
        free( buffer );
    }
    va_end(arglist);
    
#if !ORTE_DISABLE_FULL_SUPPORT
    /* if I am a daemon or the HNP... */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* whack my local procs */
        orte_odls.kill_local_procs(NULL);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    } else {
        /* cleanup my session directory */
        orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    }
#else
    orte_session_dir_finalize(ORTE_PROC_MY_NAME);
#endif

    /* if a critical connection failed, or a sensor limit was exceeded, exit without dropping a core */
    if (ORTE_ERR_CONNECTION_FAILED == error_code ||
        ORTE_ERR_SENSOR_LIMIT_EXCEEDED == error_code) {
        orte_ess.abort(error_code, false);
    } else {
        orte_ess.abort(error_code, true);
    }

    /*
     * We must exit in orte_ess.abort; all implementations of orte_ess.abort
     * contain __opal_attribute_noreturn__
     */
    /* No way to reach here */
}

void orte_errmgr_base_register_migration_warning(struct timeval *tv)
{
    /* stub function - ignore */
    return;
}

int orte_errmgr_base_abort_peers(orte_process_name_t *procs, orte_std_cntr_t num_procs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/********************
 * Utility functions
 ********************/
#if OPAL_ENABLE_FT_CR

void orte_errmgr_base_migrate_state_notify(int state)
{
    switch(state) {
    case ORTE_ERRMGR_MIGRATE_STATE_ERROR:
    case ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS:
        opal_output(0, "%d: Migration failed for process %s.",
                    orte_process_info.pid, ORTE_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_FINISH:
        opal_output(0, "%d: Migration successful for process %s.",
                    orte_process_info.pid, ORTE_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        break;

    case ORTE_ERRMGR_MIGRATE_STATE_NONE:
    case ORTE_ERRMGR_MIGRATE_STATE_REQUEST:
    case ORTE_ERRMGR_MIGRATE_STATE_RUNNING:
    case ORTE_ERRMGR_MIGRATE_STATE_RUN_CKPT:
    case ORTE_ERRMGR_MIGRATE_STATE_STARTUP:
    case ORTE_ERRMGR_MIGRATE_MAX:
    default:
        break;
    }
}

void orte_errmgr_base_proc_state_notify(orte_proc_state_t state, orte_process_name_t *proc)
{
    if (NULL != proc) {
        switch(state) {
        case ORTE_PROC_STATE_ABORTED:
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
        case ORTE_PROC_STATE_TERM_WO_SYNC:
        case ORTE_PROC_STATE_TERMINATED:
        case ORTE_PROC_STATE_KILLED_BY_CMD:
        case ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            opal_output(0, "%d: Process %s is dead.",
                        orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));
            break;

        case ORTE_PROC_STATE_HEARTBEAT_FAILED:
            opal_output(0, "%d: Process %s is unreachable.",
                        orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));

        case ORTE_PROC_STATE_COMM_FAILED:
            opal_output(0, "%d: Failed to communicate with process %s.",
                        orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));
            break;

        case ORTE_PROC_STATE_CALLED_ABORT:
        case ORTE_PROC_STATE_FAILED_TO_START:
            opal_output(0, "%d: Process %s has called abort.",
                        orte_process_info.pid, ORTE_JOBID_PRINT(proc->jobid));
            break;
        case ORTE_PROC_STATE_MIGRATING:
        default:
            break;
        }
    }
}

int orte_errmgr_base_migrate_state_str(char ** state_str, int state)
{
    switch(state) {
    case ORTE_ERRMGR_MIGRATE_STATE_NONE:
        *state_str = strdup(" -- ");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_REQUEST:
        *state_str = strdup("Requested");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_RUNNING:
        *state_str = strdup("Running");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_RUN_CKPT:
        *state_str = strdup("Checkpointing");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_STARTUP:
        *state_str = strdup("Restarting");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_FINISH:
        *state_str = strdup("Finished");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_ERROR:
        *state_str = strdup("Error");
        break;
    case ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS:
        *state_str = strdup("Error: Migration in progress");
        break;
    default:
        asprintf(state_str, "Unknown %d", state);
        break;
    }

    return ORTE_SUCCESS;
}
#endif

#if OPAL_ENABLE_FT_CR
int orte_errmgr_base_update_app_context_for_cr_recovery(orte_job_t *jobdata,
                                                        orte_proc_t *proc,
                                                        opal_list_t *local_snapshots)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t *item = NULL;
    orte_std_cntr_t i_app;
    int argc = 0;
    orte_app_context_t *cur_app_context = NULL;
    orte_app_context_t *new_app_context = NULL;
    orte_sstore_base_local_snapshot_info_t *vpid_snapshot = NULL;
    char *reference_fmt_str = NULL;
    char *location_str = NULL;
    char *cache_location_str = NULL;
    char *ref_location_fmt_str = NULL;
    char *tmp_str = NULL;
    char *global_snapshot_ref = NULL;
    char *global_snapshot_seq = NULL;

    /*
     * Get the snapshot restart command for this process
     * JJH CLEANUP: Pass in the vpid_snapshot, so we don't have to look it up every time?
     */
    for(item  = opal_list_get_first(local_snapshots);
        item != opal_list_get_end(local_snapshots);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_sstore_base_local_snapshot_info_t*)item;
        if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                       &vpid_snapshot->process_name,
                                                       &proc->name) ) {
            break;
        }
        else {
            vpid_snapshot = NULL;
        }
    }

    if( NULL == vpid_snapshot ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    orte_sstore.get_attr(vpid_snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_REF_FMT,
                         &reference_fmt_str);
    orte_sstore.get_attr(vpid_snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_LOC,
                         &location_str);
    orte_sstore.get_attr(vpid_snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_REF_LOC_FMT,
                         &ref_location_fmt_str);
    orte_sstore.get_attr(vpid_snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_REF,
                         &global_snapshot_ref);
    orte_sstore.get_attr(vpid_snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_SEQ,
                         &global_snapshot_seq);

    /*
     * Find current app_context
     */
    cur_app_context = NULL;
    for(i_app = 0; i_app < opal_pointer_array_get_size(jobdata->apps); ++i_app) {
        cur_app_context = (orte_app_context_t *)opal_pointer_array_get_item(jobdata->apps,
                                                                            i_app);
        if( NULL == cur_app_context ) {
            continue;
        }
        if(proc->app_idx == cur_app_context->idx) {
            break;
        }
    }

    if( NULL == cur_app_context ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * if > 1 processes in this app context
     *   Create a new app_context
     *   Copy over attributes
     *   Add it to the job_t data structure
     *   Associate it with this process in the job
     * else
     *   Reuse this app_context
     */
    if( cur_app_context->num_procs > 1 ) {
        /* Create a new app_context */
        new_app_context = OBJ_NEW(orte_app_context_t);

        /* Copy over attributes */
        new_app_context->idx                    = cur_app_context->idx;
        new_app_context->app                    = NULL; /* strdup(cur_app_context->app); */
        new_app_context->num_procs              = 1;
        new_app_context->argv                   = NULL; /* opal_argv_copy(cur_app_context->argv); */
        new_app_context->env                    = opal_argv_copy(cur_app_context->env);
        new_app_context->cwd                    = (NULL == cur_app_context->cwd ? NULL :
                                                   strdup(cur_app_context->cwd));
        new_app_context->user_specified_cwd     = cur_app_context->user_specified_cwd;
        new_app_context->hostfile               = (NULL == cur_app_context->hostfile ? NULL :
                                                   strdup(cur_app_context->hostfile));
        new_app_context->add_hostfile           = (NULL == cur_app_context->add_hostfile ? NULL :
                                                   strdup(cur_app_context->add_hostfile));
        new_app_context->dash_host              = opal_argv_copy(cur_app_context->dash_host);
        new_app_context->prefix_dir             = (NULL == cur_app_context->prefix_dir ? NULL :
                                                   strdup(cur_app_context->prefix_dir));
        new_app_context->preload_binary         = false;
        new_app_context->preload_libs           = false;
        new_app_context->preload_files_dest_dir = NULL;
        new_app_context->preload_files_src_dir  = NULL;

        asprintf(&tmp_str, reference_fmt_str, vpid_snapshot->process_name.vpid);
        asprintf(&(new_app_context->sstore_load),
                 "%s:%s:%s:%s:%s:%s",
                 location_str,
                 global_snapshot_ref,
                 tmp_str,
                 (vpid_snapshot->compress_comp == NULL ? "" : vpid_snapshot->compress_comp),
                 (vpid_snapshot->compress_postfix == NULL ? "" : vpid_snapshot->compress_postfix),
                 global_snapshot_seq);

        new_app_context->used_on_node           = cur_app_context->used_on_node;

        /* Add it to the job_t data structure */
        /*current_global_jobdata->num_apps++; */
        new_app_context->idx = (jobdata->num_apps);
        proc->app_idx = new_app_context->idx;

        opal_pointer_array_add(jobdata->apps, new_app_context);
        ++(jobdata->num_apps);

        /* Remove association with the old app_context */
        --(cur_app_context->num_procs);
    }
    else {
        new_app_context = cur_app_context;

        /* Cleanout old stuff */
        free(new_app_context->app);
        new_app_context->app = NULL;

        opal_argv_free(new_app_context->argv);
        new_app_context->argv = NULL;

        asprintf(&tmp_str, reference_fmt_str, vpid_snapshot->process_name.vpid);
        asprintf(&(new_app_context->sstore_load),
                 "%s:%s:%s:%s:%s:%s",
                 location_str,
                 global_snapshot_ref,
                 tmp_str,
                 (vpid_snapshot->compress_comp == NULL ? "" : vpid_snapshot->compress_comp),
                 (vpid_snapshot->compress_postfix == NULL ? "" : vpid_snapshot->compress_postfix),
                 global_snapshot_seq);
    }

    /*
     * Update the app_context with the restart informaiton
     */
    new_app_context->app = strdup("opal-restart");
    opal_argv_append(&argc, &(new_app_context->argv), new_app_context->app);
    opal_argv_append(&argc, &(new_app_context->argv), "-l");
    opal_argv_append(&argc, &(new_app_context->argv), location_str);
    opal_argv_append(&argc, &(new_app_context->argv), "-m");
    opal_argv_append(&argc, &(new_app_context->argv), orte_sstore_base_local_metadata_filename);
    opal_argv_append(&argc, &(new_app_context->argv), "-r");
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    asprintf(&tmp_str, reference_fmt_str, vpid_snapshot->process_name.vpid);
    opal_argv_append(&argc, &(new_app_context->argv), tmp_str);

 cleanup:
    if( NULL != tmp_str) {
        free(tmp_str);
        tmp_str = NULL;
    }
    if( NULL != location_str ) {
        free(location_str);
        location_str = NULL;
    }
    if( NULL != cache_location_str ) {
        free(cache_location_str);
        cache_location_str = NULL;
    }
    if( NULL != reference_fmt_str ) {
        free(reference_fmt_str);
        reference_fmt_str = NULL;
    }
    if( NULL != ref_location_fmt_str ) {
        free(ref_location_fmt_str);
        ref_location_fmt_str = NULL;
    }

    return exit_status;
}
#endif

#if OPAL_ENABLE_FT_CR
int orte_errmgr_base_restart_job(orte_jobid_t jobid, char * global_handle, int seq_num)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_process_name_t loc_proc;
    orte_job_t *jdata;
    orte_sstore_base_handle_t prev_sstore_handle = ORTE_SSTORE_HANDLE_INVALID;

    /* JJH First determine if we can recover this way */

    /*
     * Find the corresponding sstore handle
     */
    prev_sstore_handle = orte_sstore_handle_last_stable;
    if( ORTE_SUCCESS != (ret = orte_sstore.request_restart_handle(&orte_sstore_handle_last_stable,
                                                                  NULL,
                                                                  global_handle,
                                                                  seq_num,
                                                                  NULL)) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /* get the job object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        exit_status = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(exit_status);
        goto cleanup;
    }

    /*
     * Start the recovery
     */
    orte_snapc_base_has_recovered = false;
    loc_proc.jobid = jobid;
    loc_proc.vpid  = 0;
    ORTE_ACTIVATE_PROC_STATE(&loc_proc, ORTE_PROC_STATE_KILLED_BY_CMD);
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_CONTROL_RESTART);
    while( !orte_snapc_base_has_recovered ) {
        opal_progress();
    }
    orte_sstore_handle_last_stable = prev_sstore_handle;

 cleanup:
    return exit_status;
}

int orte_errmgr_base_migrate_job(orte_jobid_t jobid, orte_snapc_base_request_op_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    int i;
    opal_list_t *proc_list = NULL;
    opal_list_t *node_list = NULL;
    opal_list_t *suggested_map_list = NULL;
    orte_errmgr_predicted_map_t  *onto_map = NULL;
#if 0
    orte_errmgr_predicted_proc_t *off_proc = NULL;
    orte_errmgr_predicted_node_t *off_node = NULL;
#endif

    proc_list = OBJ_NEW(opal_list_t);
    node_list = OBJ_NEW(opal_list_t);
    suggested_map_list = OBJ_NEW(opal_list_t);

    for( i = 0; i < datum->mig_num; ++i ) {
        /*
         * List all processes that are included in the migration.
         * We will sort them out in the component.
         */
        onto_map = OBJ_NEW(orte_errmgr_predicted_map_t);

        if( (datum->mig_off_node)[i] ) {
            onto_map->off_current_node = true;
        } else {
            onto_map->off_current_node = false;
        }

        /* Who to migrate */
        onto_map->proc_name.jobid = jobid;
        onto_map->proc_name.vpid = (datum->mig_vpids)[i];

        /* Destination */
        onto_map->map_proc_name.jobid = jobid;
        onto_map->map_proc_name.vpid  = (datum->mig_vpid_pref)[i];

        if( ((datum->mig_host_pref)[i])[0] == '\0') {
            onto_map->map_node_name = NULL;
        } else {
            onto_map->map_node_name = strdup((datum->mig_host_pref)[i]);
        }

        opal_list_append(suggested_map_list, &(onto_map->super));
    }

    if( ORTE_SUCCESS != (ret = orte_errmgr.predicted_fault(proc_list, node_list, suggested_map_list)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

#endif

orte_errmgr_fault_callback_t *orte_errmgr_base_set_fault_callback(orte_errmgr_fault_callback_t *cbfunc) {
    orte_errmgr_fault_callback_t *temp_cbfunc = fault_cbfunc;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output, 
                "%s errmgr:base Called set_fault_callback", 
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    fault_cbfunc = cbfunc;

    return temp_cbfunc;
}

/********************
 * Local Functions
 ********************/
