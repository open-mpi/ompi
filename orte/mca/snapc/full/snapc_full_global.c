/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2011 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/include/opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

#include MCA_timer_IMPLEMENTATION_HEADER

/************************************
 * Locally Global vars & functions :)
 ************************************/
#define INC_SEQ_NUM()                         \
 {                                            \
   if(orte_snapc_base_store_only_one_seq) {   \
     orte_snapc_base_snapshot_seq_number = 0; \
   } else {                                   \
     orte_snapc_base_snapshot_seq_number++;   \
   }                                          \
 }

static orte_jobid_t current_global_jobid = ORTE_JOBID_INVALID;
static orte_snapc_base_global_snapshot_t global_snapshot;
static int current_total_orteds = 0;
static bool updated_job_to_running;
static int current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
static bool cleanup_on_establish = false;
static bool global_coord_has_local_children = false;

static bool currently_migrating = false;
static opal_list_t *migrating_procs = NULL;

static int global_init_job_structs(void);
static int global_refresh_job_structs(void);

static bool snapc_orted_recv_issued = false;
static bool is_orte_checkpoint_connected = false;
static bool is_app_checkpointable = false;
static int snapc_full_global_start_listener(void);
static int snapc_full_global_stop_listener(void);
static void snapc_full_global_orted_recv(int status,
                                         orte_process_name_t* sender,
                                         opal_buffer_t* buffer,
                                         orte_rml_tag_t tag,
                                         void* cbdata);

static void snapc_full_process_restart_proc_info_cmd(orte_process_name_t* sender,
                                                     opal_buffer_t* buffer);

static void snapc_full_process_request_op_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer);

/*** Command Line Interactions */
static orte_process_name_t orte_checkpoint_sender;
static bool snapc_cmdline_recv_issued = false;
static int snapc_full_global_start_cmdline_listener(void);
static int snapc_full_global_stop_cmdline_listener(void);
static void snapc_full_global_cmdline_recv(int status,
                                           orte_process_name_t* sender,
                                           opal_buffer_t* buffer,
                                           orte_rml_tag_t tag,
                                           void* cbdata);

static int snapc_full_establish_snapshot_dir(bool empty_metadata);

/*** */
static int snapc_full_global_checkpoint(opal_crs_base_ckpt_options_t *options);
static int snapc_full_global_notify_checkpoint(orte_jobid_t jobid,
                                               opal_crs_base_ckpt_options_t *options);
static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     int ckpt_state,
                                                     orte_sstore_base_handle_t handle,
                                                     bool quick,
                                                     opal_crs_base_ckpt_options_t *options);
int global_coord_job_state_update(orte_jobid_t jobid,
                                  int job_ckpt_state,
                                  orte_sstore_base_handle_t handle,
                                  opal_crs_base_ckpt_options_t *options);
static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer,
                                              bool quick);
static int snapc_full_process_orted_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer,
                                               bool quick);
static orte_snapc_full_orted_snapshot_t *find_orted_snapshot(orte_process_name_t *name );

static int snapc_full_global_get_min_state(void);
static int write_out_global_metadata(void);

static int orte_snapc_full_global_reset_coord(void);

/*
 * Timer stuff
 */
static void snapc_full_set_time(int idx);
static void snapc_full_display_all_timers(void);
static void snapc_full_display_recovered_timers(void);
static void snapc_full_clear_timers(void);

static double snapc_full_get_time(void);
static void snapc_full_display_indv_timer_core(double diff, char *str);

#define SNAPC_FULL_TIMER_START     0
#define SNAPC_FULL_TIMER_RUNNING   1
#define SNAPC_FULL_TIMER_FIN_LOCAL 2
#define SNAPC_FULL_TIMER_SS_SYNC   3
#define SNAPC_FULL_TIMER_ESTABLISH 4
#define SNAPC_FULL_TIMER_RECOVERED 5
#define SNAPC_FULL_TIMER_MAX       6

static double timer_start[SNAPC_FULL_TIMER_MAX];

#define SNAPC_FULL_CLEAR_TIMERS()                                       \
    {                                                                   \
        if(OPAL_UNLIKELY(orte_snapc_full_timing_enabled)) {             \
            snapc_full_clear_timers();                                  \
        }                                                               \
    }

#define SNAPC_FULL_SET_TIMER(idx)                                       \
    {                                                                   \
        if(OPAL_UNLIKELY(orte_snapc_full_timing_enabled)) {             \
            snapc_full_set_time(idx);                                   \
        }                                                               \
    }

#define SNAPC_FULL_DISPLAY_ALL_TIMERS()                                 \
    {                                                                   \
        if(OPAL_UNLIKELY(orte_snapc_full_timing_enabled)) {             \
            snapc_full_display_all_timers();                            \
        }                                                               \
    }
#define SNAPC_FULL_DISPLAY_RECOVERED_TIMER()                            \
    {                                                                   \
        if(OPAL_UNLIKELY(orte_snapc_full_timing_enabled)) {             \
            snapc_full_display_recovered_timers();                      \
        }                                                               \
    }

/*
 * Progress
 */
static void snapc_full_report_progress(orte_snapc_full_orted_snapshot_t *orted_snapshot,
                                       int total,
                                       int min_state);
static int    report_progress_cur_loc_finished = 0;
static double report_progress_last_reported_loc_finished = 0;
#define SNAPC_FULL_REPORT_PROGRESS(orted, total, min_state)             \
    {                                                                   \
        if(OPAL_UNLIKELY(orte_snapc_full_progress_meter > 0)) {         \
            snapc_full_report_progress(orted, total, min_state);        \
        }                                                               \
    }

/************************
 * Function Definitions
 ************************/
int global_coord_init(void)
{
    current_global_jobid = ORTE_JOBID_INVALID;
    orte_snapc_base_snapshot_seq_number = -1;

    orte_checkpoint_sender = orte_name_invalid;

    SNAPC_FULL_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

int global_coord_finalize(void)
{
    current_global_jobid = ORTE_JOBID_INVALID;
    orte_snapc_base_snapshot_seq_number = -1;

    SNAPC_FULL_CLEAR_TIMERS();

    return ORTE_SUCCESS;
}

int global_coord_setup_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;
    orte_job_t *jdata = NULL;

    /*
     * Only allow one job at a time.
     *
     * It is possible to pass through this function twice since HNP may also be
     * a local daemon. So it may be both a global and local coordinator.
     *  Global: orte_plm_base_setup_job()
     *  Local : odls_default_module.c
     */
    /* Global Coordinator pass */
    if( ORTE_JOBID_INVALID == current_global_jobid ) {
        current_global_jobid = jobid;
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Setup job %s as the Global Coordinator\n",
                             ORTE_JOBID_PRINT(jobid)));

        SNAPC_FULL_CLEAR_TIMERS();
        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_START);
    }
    /* Local Coordinator pass - Always happens after global coordinator pass */
    else if ( jobid == current_global_jobid ) {

        /* look up job data object */
        if (NULL == (jdata = orte_get_job_data_object(current_global_jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }

        if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_RESTART)) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Restarting Job %s...",
                                 ORTE_JOBID_PRINT(jobid)));
            SNAPC_FULL_CLEAR_TIMERS();
            SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_START);

            if( ORTE_SUCCESS != (ret = global_refresh_job_structs()) ) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE) ) {
                return local_coord_setup_job(jobid);
            }
            return ORTE_SUCCESS;
        }

        /* If there are no local children, do not become a local coordinator */
        if( !global_coord_has_local_children ) {
            return ORTE_SUCCESS;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Setup job %s as the Local Coordinator\n",
                             ORTE_JOBID_PRINT(jobid)));
        orte_snapc_coord_type |= ORTE_SNAPC_LOCAL_COORD_TYPE;
        return local_coord_setup_job(jobid);
    }
    /* Only allow one job at a time */
    else {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Setup of job %s Failed! Already setup job %s\n",
                    ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(current_global_jobid));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    /*
     * Start out with a sequence number just below the first
     * This will be incremented when we checkpoint
     */
    orte_snapc_base_snapshot_seq_number = -1;

    /*
     * Allocate structure to track node status
     */
    if( ORTE_SUCCESS != (ret = global_init_job_structs()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup Global Coordinator command processing listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup command line tool checkpoint request listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If requested pre-establish the global snapshot directory
     */
#if 0
    if(orte_snapc_base_establish_global_snapshot_dir) {
        opal_output(0, "Global) Error: Pre-establishment of snapshot directory currently not supported!");
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Pre-establish the global snapshot directory\n"));
        if( ORTE_SUCCESS != (ret = snapc_full_establish_snapshot_dir(true))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
#endif

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finished setup of job %s ",
                         ORTE_JOBID_PRINT(jobid)));

 cleanup:
    return exit_status;
}

int global_coord_release_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Make sure we are not waiting on a checkpoint to complete
     */
    if( is_orte_checkpoint_connected ) {
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                                global_snapshot.ss_handle,
                                                                                ORTE_SNAPC_CKPT_STATE_ERROR)) ) {
            ORTE_ERROR_LOG(ret);
        }
    }

    /*
     * Clean up listeners
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    OBJ_DESTRUCT(&global_snapshot);

    return exit_status;
}

int global_coord_start_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t i_proc;
    orte_proc_t *proc = NULL;
    orte_proc_t *new_proc = NULL;
    opal_list_item_t *item = NULL;
    opal_crs_base_ckpt_options_t *options = NULL;
    char *tmp_str = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Starting checkpoint (internally requested)"));

    orte_checkpoint_sender = orte_name_invalid;

    /*
     * If migrating
     */
    if( datum->migrating ) {
        currently_migrating = true;
        if( NULL != migrating_procs ) {
            while( NULL != (item = opal_list_remove_first(migrating_procs)) ) {
                proc = (orte_proc_t*)item;
                OBJ_RELEASE(proc);
            }
        } else {
            migrating_procs = OBJ_NEW(opal_list_t);
        }

        /*
         * Copy over the procs into a list
         */
        for(i_proc = 0; i_proc < opal_pointer_array_get_size(&(datum->migrating_procs)); ++i_proc) {
            proc = (orte_proc_t*)opal_pointer_array_get_item(&(datum->migrating_procs), i_proc);
            if( NULL == proc ) {
                continue;
            }

            new_proc = OBJ_NEW(orte_proc_t);
            new_proc->name.jobid = proc->name.jobid;
            new_proc->name.vpid  = proc->name.vpid;
            new_proc->node = OBJ_NEW(orte_node_t);
            new_proc->node->name = proc->node->name;
            opal_list_append(migrating_procs, &new_proc->super);
            OBJ_RETAIN(new_proc);
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) SnapC Migrating Processes: (%d procs) [Updated]\n",
                             (int)opal_list_get_size(migrating_procs) ));
        for (item  = opal_list_get_first(migrating_procs);
             item != opal_list_get_end(migrating_procs);
             item  = opal_list_get_next(item)) {
            new_proc = (orte_proc_t*)item;
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "\t\"%s\" [%s]\n",
                                 ORTE_NAME_PRINT(&new_proc->name),new_proc->node->name));
        }
    }

    /*************************
     * Kick off the checkpoint (local coord will release the processes)
     *************************/
    options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait for checkpoint to locally finish on all nodes
     */
    while(((currently_migrating  && current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_MIGRATING) ||
           (!currently_migrating && current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL)) &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ESTABLISHED &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_RECOVERED &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
        opal_progress();
    }

    /*
     * Update the quiesce structure with the handle
     */
    datum->snapshot = OBJ_NEW(orte_snapc_base_global_snapshot_t);

    datum->ss_handle = global_snapshot.ss_handle;
    datum->ss_snapshot = OBJ_NEW(orte_sstore_base_global_snapshot_info_t);
    if( ORTE_SUCCESS != (ret = orte_sstore.request_global_snapshot_data(&(datum->ss_handle), datum->ss_snapshot)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* JJH Is the snapc structure useful with the sstore structure ??? */
    orte_sstore.get_attr(global_snapshot.ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_SEQ,
                         &tmp_str);
    datum->epoch = atoi(tmp_str);

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return exit_status;
}

int global_coord_end_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finishing checkpoint (internally requested) [%3d]",
                         current_job_ckpt_state));

    if( currently_migrating ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) End Ckpt: Flush the modex cached data\n"));

        /* TODO: You can't pass NULL as the identifier - what you'll need to do is
         * close all open dstore handles, and then open the ones you need
         */
#if 0
        if (OPAL_SUCCESS != (ret = opal_dstore.remove(NULL, NULL))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
#endif

        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_ESTABLISH);
        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                            ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL,
                                                                            global_snapshot.ss_handle,
                                                                            true, NULL) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    while(current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_RECOVERED &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
        opal_progress();
    }

    /*
     * Update the job structure since processes may have moved around
     */
    if( ORTE_SUCCESS != (ret = global_refresh_job_structs()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finished checkpoint (internally requested) [%d]",
                         current_job_ckpt_state));

    if( currently_migrating ) {
        current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
        cleanup_on_establish = false;

        report_progress_cur_loc_finished = 0;
        report_progress_last_reported_loc_finished = 0;
    }

 cleanup:

    currently_migrating = false;
    if( NULL != migrating_procs ) {
        while( NULL != (item = opal_list_remove_first(migrating_procs)) ) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(migrating_procs);
        migrating_procs = NULL;
    }

    return exit_status;
}

/******************
 * Local functions
 ******************/
static int global_init_job_structs(void)
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;
    orte_node_t *cur_node = NULL;
    orte_job_map_t *map = NULL;
    orte_job_t *jdata = NULL;
    orte_proc_t **procs = NULL;
    orte_std_cntr_t i = 0;
    orte_vpid_t p = 0;
    orte_ns_cmp_bitmask_t mask;
    bool found = false;

    /* look up job data object */
    if (NULL == (jdata = orte_get_job_data_object(current_global_jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    OBJ_CONSTRUCT(&global_snapshot, orte_snapc_base_global_snapshot_t);

    map = jdata->map;

    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (cur_node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }

        procs = (orte_proc_t**)cur_node->procs->addr;

        /*
         * Look out for duplicates
         * JJH: Should not happen, but does if rmaps get a bug in setting up the map.
         */
        found = false;
        for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
            orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
            orted_item  = opal_list_get_next(orted_item) ) {
            orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;
            /*
             * Is in list?
             */
            if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                           &(cur_node->daemon->name),
                                                           &(orted_snapshot->process_name) )) {
                found = true;
                break;
            }
        }
        if( found ) {
            OPAL_OUTPUT_VERBOSE((1, mca_snapc_full_component.super.output_handle,
                                 "Global) [%d] Found Daemon %s with %d procs - Duplicate!! - Should not happen!",
                                 i, ORTE_NAME_PRINT(&(cur_node->daemon->name)), cur_node->num_procs));
            continue;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) [%d] Found Daemon %s with %d procs",
                             i, ORTE_NAME_PRINT(&(cur_node->daemon->name)), cur_node->num_procs));

        orted_snapshot = OBJ_NEW(orte_snapc_full_orted_snapshot_t);

        orted_snapshot->process_name.jobid  = cur_node->daemon->name.jobid;
        orted_snapshot->process_name.vpid   = cur_node->daemon->name.vpid;

        mask = ORTE_NS_CMP_JOBID;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, &orted_snapshot->process_name, ORTE_PROC_MY_NAME)) {
            global_coord_has_local_children = true;
        }

        for(p = 0; p < cur_node->num_procs; ++p) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) \t [%d] Found Process %s on Daemon %s",
                                 p, ORTE_NAME_PRINT(&(procs[p]->name)), ORTE_NAME_PRINT(&(cur_node->daemon->name)) ));

            app_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

            app_snapshot->process_name.jobid = procs[p]->name.jobid;
            app_snapshot->process_name.vpid = procs[p]->name.vpid;

            opal_list_append(&(orted_snapshot->super.local_snapshots), &(app_snapshot->super));
        }


        opal_list_append(&global_snapshot.local_snapshots, &(orted_snapshot->super.super));
    }

    return ORTE_SUCCESS;
}

static int global_refresh_job_structs(void)
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;
    opal_list_item_t* app_item = NULL;
    opal_list_item_t* item = NULL;
    orte_node_t *cur_node = NULL;
    orte_job_map_t *map = NULL;
    orte_job_t *jdata = NULL;
    orte_proc_t **procs = NULL;
    orte_proc_t *new_proc = NULL;
    orte_std_cntr_t i = 0;
    orte_vpid_t p = 0;
    bool found = false;
    orte_ns_cmp_bitmask_t mask;

    /* look up job data object */
    if (NULL == (jdata = orte_get_job_data_object(current_global_jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Refreshing Job Structures... [%3d]",
                         current_job_ckpt_state));

    if( NULL != migrating_procs ) {
        for (item  = opal_list_get_first(migrating_procs);
             item != opal_list_get_end(migrating_procs);
             item  = opal_list_get_next(item)) {
            new_proc = (orte_proc_t*)item;

            /*
             * Look through all daemons
             */
            found = false;
            for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
                orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
                orted_item  = opal_list_get_next(orted_item) ) {
                orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

                /*
                 * Look through all processes tracked by this daemon
                 */
                for(app_item  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
                    app_item != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
                    app_item  = opal_list_get_next(app_item) ) {
                    app_snapshot = (orte_snapc_base_local_snapshot_t*)app_item;

                    if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                                   &(new_proc->name),
                                                                   &(app_snapshot->process_name) )) {
                        found = true;
                        opal_list_remove_item(&(orted_snapshot->super.local_snapshots), app_item);
                        break;
                    }
                }

                if( found ) {
                    break;
                }
            }
        }
    }

    /*
     * First make sure that all of the orted's have the proper number of
     * children, if no children, then stop tracking.
     */
    map = jdata->map;
    for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
        orted_item  = opal_list_get_next(orted_item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

        /* Make sure this orted is in the map */
        found = false;
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (cur_node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }

            if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                           &(cur_node->daemon->name),
                                                           &(orted_snapshot->process_name) )) {
                found = true;
                break;
            }
        }
        /* If not, then remove all processes, keep ref. we might reuse it later */
        if( !found ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Found Empty Daemon %s not in map (Refresh)",
                                 ORTE_NAME_PRINT(&(orted_snapshot->process_name)) ));
            while( NULL != (item = opal_list_remove_first(&(orted_snapshot->super.local_snapshots))) ) {
                OBJ_RELEASE(item);
            }
        }
    }

    /*
     * Look for new nodes
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (cur_node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }

        procs = (orte_proc_t**)cur_node->procs->addr;

        /*
         * See if we are already tracking it, if so refresh it
         * (This daemon could have been restarted, and processes migrated back to it)
         */
        found = false;
        for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
            orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
            orted_item  = opal_list_get_next(orted_item) ) {
            orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

            if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                           &(cur_node->daemon->name),
                                                           &(orted_snapshot->process_name) )) {
                found = true;
                break;
            }
        }
        if( found ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) [%d] Found Daemon %s with %d procs (Refresh)",
                                 i, ORTE_NAME_PRINT(&(cur_node->daemon->name)), cur_node->num_procs));

            /* Remove all old processes */
            while( NULL != (item = opal_list_remove_first(&(orted_snapshot->super.local_snapshots))) ) {
                OBJ_RELEASE(item);
            }

            /* Add back new processes (a bit of overkill, sure, but it works) */
            for(p = 0; p < cur_node->num_procs; ++p) {
                if( NULL == procs[p] ) {
                    continue;
                }

                OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                     "Global) \t [%d] Found Process %s on Daemon %s",
                                     p, ORTE_NAME_PRINT(&(procs[p]->name)), ORTE_NAME_PRINT(&(cur_node->daemon->name)) ));

                app_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

                app_snapshot->process_name.jobid = procs[p]->name.jobid;
                app_snapshot->process_name.vpid = procs[p]->name.vpid;

                opal_list_append(&(orted_snapshot->super.local_snapshots), &(app_snapshot->super));
            }

            continue;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) [%d] Found Daemon %s with %d procs",
                             i, ORTE_NAME_PRINT(&(cur_node->daemon->name)), cur_node->num_procs));

        orted_snapshot = OBJ_NEW(orte_snapc_full_orted_snapshot_t);

        orted_snapshot->process_name.jobid  = cur_node->daemon->name.jobid;
        orted_snapshot->process_name.vpid   = cur_node->daemon->name.vpid;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, &orted_snapshot->process_name, ORTE_PROC_MY_NAME)) {
            global_coord_has_local_children = true;
        }
        for(p = 0; p < cur_node->num_procs; ++p) {
            if( NULL == procs[p] ) {
                continue;
            }

            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) \t [%d] Found Process %s on Daemon %s",
                                 p, ORTE_NAME_PRINT(&(procs[p]->name)), ORTE_NAME_PRINT(&(cur_node->daemon->name)) ));

            app_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

            app_snapshot->process_name.jobid = procs[p]->name.jobid;
            app_snapshot->process_name.vpid = procs[p]->name.vpid;

            opal_list_append(&(orted_snapshot->super.local_snapshots), &(app_snapshot->super));
        }

        opal_list_append(&global_snapshot.local_snapshots, &(orted_snapshot->super.super));
    }

    return ORTE_SUCCESS;
}

/*****************
 * Setup listeners
 *****************/
static int snapc_full_global_start_listener(void)
{
    if (snapc_orted_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Startup Coordinator Channel"));

    /*
     * Coordinator command listener
     */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC_FULL,
                            ORTE_RML_PERSISTENT, snapc_full_global_orted_recv, NULL);

    snapc_orted_recv_issued = true;

    return ORTE_SUCCESS;
}

static int snapc_full_global_stop_listener(void)
{
    if (!snapc_orted_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Shutdown Coordinator Channel"));

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC_FULL);

    snapc_orted_recv_issued = false;
    return ORTE_SUCCESS;
}

static int snapc_full_global_start_cmdline_listener(void)
{
    if (snapc_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Startup Command Line Channel"));

    /*
     * Coordinator command listener
     */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_CKPT, 0,
                            snapc_full_global_cmdline_recv, NULL);

    snapc_cmdline_recv_issued = true;
    return ORTE_SUCCESS;
}

static int snapc_full_global_stop_cmdline_listener(void)
{
    if (!snapc_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Shutdown Command Line Channel"));

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_CKPT);

    snapc_cmdline_recv_issued = false;
    return ORTE_SUCCESS;
}

/*****************
 * Listener Callbacks
 *****************/
static void snapc_full_global_cmdline_recv(int status,
                                           orte_process_name_t* sender,
                                           opal_buffer_t* buffer,
                                           orte_rml_tag_t tag,
                                           void* cbdata)
{
    int ret;
    orte_snapc_cmd_flag_t command;
    orte_std_cntr_t count = 1;
    orte_jobid_t jobid;
    opal_crs_base_ckpt_options_t *options = NULL;

    if( ORTE_RML_TAG_CKPT != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Command Line: Start a checkpoint operation [Sender = %s]",
                         ORTE_NAME_PRINT(sender)));

    snapc_cmdline_recv_issued = false; /* Not a persistent RML message */

    options = OBJ_NEW(opal_crs_base_ckpt_options_t);

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * orte_checkpoint has requested that a checkpoint be taken
     */
    if (ORTE_SNAPC_GLOBAL_INIT_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested a checkpoint [command %d]\n",
                             command));

        /*
         * Unpack the buffer from the orte_checkpoint command
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_init_cmd(sender,
                                                                              buffer,
                                                                              options,
                                                                              &jobid)) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        orte_checkpoint_sender = *sender;
        is_orte_checkpoint_connected = true;

        /*
         * If the application is not ready for a checkpoint,
         * then send back an error.
         */
        if( !is_app_checkpointable ) {
            OPAL_OUTPUT_VERBOSE((1, mca_snapc_full_component.super.output_handle,
                                 "Global) request_cmd(): Checkpointing currently disabled, rejecting request"));
            if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                                    0,
                                                                                    ORTE_SNAPC_CKPT_STATE_ERROR))) {
                ORTE_ERROR_LOG(ret);
            }

            orte_checkpoint_sender = orte_name_invalid;
            is_orte_checkpoint_connected = false;

            /* Reset the listener */
            if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener() ) ){
                ORTE_ERROR_LOG(ret);
            }

            goto cleanup;
        }

        /*
         * If the jobid was specified, and does not match the current job, then fail
         */
        if( ORTE_JOBID_INVALID != jobid && jobid != current_global_jobid) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "Global) Error: Jobid %s does not match the current jobid %s",
                        ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(current_global_jobid));
            ORTE_ERROR_LOG(ORTE_ERROR);
            goto cleanup;
        }

        /*************************
         * Kick off the checkpoint
         *************************/
        SNAPC_FULL_CLEAR_TIMERS();
        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_START);

        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

    }
    /*
     * Terminate the connection (Not currently implemented)
     */
    else if (ORTE_SNAPC_GLOBAL_TERM_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested to terminate connection (command %d)\n",
                             command));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        goto cleanup;
    }
    /*
     * Unknown command
     */
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line sent an unknown command (command %d)\n",
                             command));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        goto cleanup;
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}

void snapc_full_global_orted_recv(int status,
                                  orte_process_name_t* sender,
                                  opal_buffer_t* buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata)
{
    int ret;
    orte_snapc_full_cmd_flag_t command;
    orte_std_cntr_t count;
    static int num_inside = 0;

    if( ORTE_RML_TAG_SNAPC_FULL != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    /*
     * This is a message from a Local Coordinator
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive a command message from %s.",
                         ORTE_NAME_PRINT(sender)));

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        return;
    }

    ++num_inside;

    switch (command) {
        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Job State Update (quick)"));

            snapc_full_process_job_update_cmd(sender, buffer, true);
            break;

        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Job State Update"));

            snapc_full_process_job_update_cmd(sender, buffer, false);
            break;

        case ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_QUICK_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Daemon State Update (quick)"));

            snapc_full_process_orted_update_cmd(sender, buffer, true);
            break;

        case ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Daemon State Update"));

            snapc_full_process_orted_update_cmd(sender, buffer, false);
            break;

        case ORTE_SNAPC_FULL_RESTART_PROC_INFO:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Update hostname/pid associations"));

            snapc_full_process_restart_proc_info_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_FULL_REQUEST_OP_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Request Op"));

            snapc_full_process_request_op_cmd(sender, buffer);
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }

    return;
}

static void snapc_full_process_request_op_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* sbuffer)
{
    int ret;
    orte_std_cntr_t count = 1;
    orte_jobid_t jobid;
    int op_event, op_state;
    opal_crs_base_ckpt_options_t *options = NULL;
    opal_buffer_t *buffer = NULL;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_REQUEST_OP_CMD;
    int seq_num = -1, i;
    char * global_handle = NULL, *tmp_str = NULL;
    orte_snapc_base_request_op_t *datum = NULL;

    orte_checkpoint_sender = orte_name_invalid;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &op_event, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) process_request_op(): Op Code %2d\n",
                         op_event));

    /************************************
     * Application have been initialized, and are ready for checkpointing
     ************************************/
    if( ORTE_SNAPC_OP_INIT == op_event ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Checkpointing Enabled (%2d)\n",
                             op_event));
        is_app_checkpointable = true;
    }
    /************************************
     * Application is finalizing, and no longer ready for checkpointing.
     ************************************/
    else if( ORTE_SNAPC_OP_FIN == op_event ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Checkpointing Disabled (%2d)\n",
                             op_event));
        is_app_checkpointable = false;

        /*
         * Wait for any ongoing checkpoints to finish
         */
        if( current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
            current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
            OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                                 "Global) process_request_op(): Wait for ongoing checkpoint to complete..."));
            while( current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
                   current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
                opal_progress();
            }
        }

        /*
         * Tell application that it is now ok to finailze
         */
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Send Finalize ACK to the job"));

        buffer = OBJ_NEW(opal_buffer_t);
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        op_event = ORTE_SNAPC_OP_FIN_ACK;
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &op_event, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(sender, buffer, ORTE_RML_TAG_SNAPC_FULL,
                                                           orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        /* buffer should not be released here; the callback releases it */
        buffer = NULL;
    }
    /************************************
     * Start a checkpoint operation
     ************************************/
    else if( ORTE_SNAPC_OP_CHECKPOINT == op_event ) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Starting checkpoint (%2d)\n",
                             op_event));

        options = OBJ_NEW(opal_crs_base_ckpt_options_t);
        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        /*
         * Wait for the operation to complete
         */
        while( current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
               current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
            opal_progress();
        }

        if( ORTE_SNAPC_CKPT_STATE_ERROR == current_job_ckpt_state ) {
            op_state = -1;
        } else {
            op_state = 0;
        }

        /*
         * Tell the sender that the operation is finished
         */
        buffer = OBJ_NEW(opal_buffer_t);
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &op_event, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &op_state, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(sender, buffer, ORTE_RML_TAG_SNAPC_FULL,
                                                           orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        /* buffer should not be released here; the callback releases it */
        buffer = NULL;
    }
    /************************************
     * Start the Restart operation
     ************************************/
    else if( ORTE_SNAPC_OP_RESTART == op_event ) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Starting restart (%2d)\n",
                             op_event));

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &seq_num, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            orte_snapc_ckpt_state_notify(ORTE_SNAPC_CKPT_STATE_NO_RESTART);
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &global_handle, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            orte_snapc_ckpt_state_notify(ORTE_SNAPC_CKPT_STATE_NO_RESTART);
            goto cleanup;
        }

        /*
         * Kick off the restart
         */
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_restart_job(current_global_jobid, global_handle, seq_num) ) ) {
            ORTE_ERROR_LOG(ret);
            orte_snapc_ckpt_state_notify(ORTE_SNAPC_CKPT_STATE_NO_RESTART);
            goto cleanup;
        }
    }
    /************************************
     * Start the Migration operation
     ************************************/
    else if( ORTE_SNAPC_OP_MIGRATE == op_event ) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Starting migration (%2d)\n",
                             op_event));

        datum = OBJ_NEW(orte_snapc_base_request_op_t);

        /*
         * Unpack migration information
         */
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &(datum->mig_num), &count, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        datum->mig_vpids = malloc(sizeof(int) * datum->mig_num);
        datum->mig_host_pref = malloc(sizeof(char) * datum->mig_num * OPAL_MAX_PROCESSOR_NAME);
        datum->mig_vpid_pref = malloc(sizeof(int) * datum->mig_num);
        datum->mig_off_node  = malloc(sizeof(int) * datum->mig_num);

        for( i = 0; i < datum->mig_num; ++i ) {
            (datum->mig_vpids)[i] = 0;
            (datum->mig_host_pref)[i][0] = '\0';
            (datum->mig_vpid_pref)[i] = 0;
            (datum->mig_off_node)[i] = (int)false;
        }

        for( i = 0; i < datum->mig_num; ++i ) {
            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &((datum->mig_vpids)[i]), &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            if(NULL != tmp_str ) {
                free(tmp_str);
                tmp_str = NULL;
            }
            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &tmp_str, &count, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }
            strncpy( ((datum->mig_host_pref)[i]), tmp_str, OPAL_MAX_PROCESSOR_NAME);

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &((datum->mig_vpid_pref)[i]), &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &((datum->mig_off_node)[i]), &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global) Migration %3d/%3d: Received Rank %3d - Requested <%s> (%3d) %c\n",
                                 datum->mig_num, i,
                                 (datum->mig_vpids)[i],
                                 (datum->mig_host_pref)[i],
                                 (datum->mig_vpid_pref)[i],
                                 (OPAL_INT_TO_BOOL((datum->mig_off_node)[i]) ? 'T' : 'F')
                                 ));
        }

        /*
         * Kick off the migration
         */
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) ------ Kick Off Migration -----"));
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_migrate_job(current_global_jobid, datum) ) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        /*
         * Tell the sender that the operation is finished
         */
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) ------ Finished Migration. Release processes (%15s )-----",
                             ORTE_NAME_PRINT(sender) ));
        buffer = OBJ_NEW(opal_buffer_t);
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &op_event, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        op_state = 0;
        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &op_state, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(sender, buffer, ORTE_RML_TAG_SNAPC_FULL,
                                                           orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) ------ Finished Migration. Released processes (%15s )-----",
                             ORTE_NAME_PRINT(sender) ));
    }
    /************************************
     * Start the Quiesce operation
     ************************************/
    else if( ORTE_SNAPC_OP_QUIESCE_START == op_event) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Starting quiesce (%2d)\n",
                             op_event));

        options = OBJ_NEW(opal_crs_base_ckpt_options_t);
        options->inc_prep_only = true;
        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        /*
         * Wait for quiescence
         */
        while( current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
               current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_INC_PREPED ) {
            opal_progress();
        }

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Quiesce_start finished(%2d)\n",
                             op_event));
    }
    /************************************
     * End the Quiesce operation
     ************************************/
    else if( ORTE_SNAPC_OP_QUIESCE_END == op_event) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Ending quiesce (%2d)\n",
                             op_event));

        /*
         * Wait for the checkpoint operation to finish
         */
        while( current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
               current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
            opal_progress();
        }

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Global) process_request_op(): Quiesce_end finished(%2d)\n",
                             op_event));
    }

cleanup:
    if (NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    if(NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    return;
}

static int snapc_full_process_orted_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer,
                                               bool quick)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    int remote_ckpt_state;
    opal_list_item_t* item = NULL;
    opal_list_item_t* aitem = NULL;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    int loc_min_state;
    char *state_str = NULL;

    orted_snapshot = find_orted_snapshot(sender);
    if( NULL == orted_snapshot ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown Daemon %s",
                    ORTE_NAME_PRINT(sender) );
        exit_status = ORTE_ERROR;
        ORTE_ERROR_LOG(ORTE_ERROR);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Daemon %s: Changed state to:\n",
                         ORTE_NAME_PRINT(&(orted_snapshot->process_name)) ));

    /*
     * Unpack the data (quick)
     * - state
     * Unpack the data (long)
     * - state
     * - # procs
     * - Foreach proc
     *   - process name
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_state, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    orted_snapshot->state = remote_ckpt_state;
    orte_snapc_ckpt_state_str(&state_str, orted_snapshot->state);
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   State:        %d (%s)\n",
                         (int)(orted_snapshot->state), state_str));
    free(state_str);
    state_str = NULL;

    /* JJH: Though there is currently no additional information sent in a long
     *      message versus a small message, keep this logic so that in the
     *      future it can be easily reused without substantially modifying
     *      the component.
     */
    if( quick ) {
        exit_status = ORTE_SUCCESS;
        goto post_process;
    }

 post_process:
    loc_min_state = snapc_full_global_get_min_state();

    SNAPC_FULL_REPORT_PROGRESS(orted_snapshot, current_total_orteds, loc_min_state);

    /*
     * Notify the orte-checkpoint command once we have everyone running.
     * No need to broadcast this to everyone since they already know.
     */
    if( ORTE_SNAPC_CKPT_STATE_RUNNING == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_RUNNING != current_job_ckpt_state) {
        current_job_ckpt_state = loc_min_state;

        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_RUNNING);

        if( is_orte_checkpoint_connected &&
            ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                                global_snapshot.ss_handle,
                                                                                current_job_ckpt_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * If we are just prep'ing the INC, then acknowledge the state change
     */
    if( ORTE_SNAPC_CKPT_STATE_INC_PREPED == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_INC_PREPED > current_job_ckpt_state) {
        current_job_ckpt_state = loc_min_state;

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)    All Processes have finished the INC prep!\n"));
    }

    /*
     * Notify the orte-checkpoint command once we have everyone stopped.
     * No need to broadcast this to everyone since they already know.
     */
    if( ORTE_SNAPC_CKPT_STATE_STOPPED == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_STOPPED > current_job_ckpt_state) {
        current_job_ckpt_state = loc_min_state;

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)    All Processes have been stopped!\n"));

        if( is_orte_checkpoint_connected &&
            ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                                global_snapshot.ss_handle,
                                                                                current_job_ckpt_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /* orte-checkpoint detaches at this point */
        is_orte_checkpoint_connected = false;

        /*
         * Synchronize the checkpoint here
         */
        write_out_global_metadata();
    }

    /*
     * If all daemons have finished, let everyone know we are locally finished.
     */
    if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL > current_job_ckpt_state) {

        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_FIN_LOCAL);

        if( ORTE_SNAPC_CKPT_STATE_NONE != current_job_ckpt_state ) {
            if( loc_min_state == current_job_ckpt_state) {
                opal_output(0, "Global) JJH WARNING!!: (%d) == (%d)", loc_min_state, current_job_ckpt_state);
            }
        }

        if( currently_migrating ) {
            write_out_global_metadata();
            current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_MIGRATING;
        }
        else {
            current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL;
        }

        if( NULL != state_str ) {
            free(state_str);
        }
        orte_snapc_ckpt_state_str(&state_str, current_job_ckpt_state);
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Job State Changed: %d (%s)\n",
                             (int)current_job_ckpt_state, state_str ));
        free(state_str);
        state_str = NULL;

        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                            current_job_ckpt_state,
                                                                            global_snapshot.ss_handle,
                                                                            true, NULL) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Now that we have finished locally,
         * - Write out the metadata
         * - Sync the snapshot to SStore
         * if we are stopping then we have already written out this data.
         */
        if( !(global_snapshot.options->stop) && !currently_migrating ) {
            write_out_global_metadata();
        }

        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_ESTABLISH);

        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                            ORTE_SNAPC_CKPT_STATE_ESTABLISHED,
                                                                            global_snapshot.ss_handle,
                                                                            true, NULL) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * If all daemons have confirmed that their local proces are finished
     * and we have finished establishing the checkpoint,
     * then let the command line tool know and cleanup.
     */
    if( ORTE_SNAPC_CKPT_STATE_RECOVERED == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_RECOVERED > current_job_ckpt_state ) {

        /*
         * If this is a job restarting then we do something different
         */
        if( current_job_ckpt_state == ORTE_SNAPC_CKPT_STATE_NONE ) {
            OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                 "Global) Job has been successfully restarted"));

            /*current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_RECOVERED;*/
            orte_snapc_ckpt_state_notify(ORTE_SNAPC_CKPT_STATE_RECOVERED);

            for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
                item != opal_list_get_end(&(global_snapshot.local_snapshots));
                item  = opal_list_get_next(item) ) {
                orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

                orted_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

                for(aitem  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
                    aitem != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
                    aitem  = opal_list_get_next(aitem) ) {
                    app_snapshot = (orte_snapc_base_local_snapshot_t*)aitem;

                    app_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;
                }
            }

            SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_RECOVERED);
            SNAPC_FULL_DISPLAY_RECOVERED_TIMER();
            orte_snapc_base_has_recovered = true;
            is_app_checkpointable = true;

            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }

        /*
         * If the checkpoint has not been established yet, then do not clear the
         * snapshot structure just yet.
         */
        if(ORTE_SNAPC_CKPT_STATE_ESTABLISHED != current_job_ckpt_state ) {
            cleanup_on_establish = true;
        }

        current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_RECOVERED;

        if( NULL != state_str ) {
            free(state_str);
        }
        orte_snapc_ckpt_state_str(&state_str, current_job_ckpt_state);
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Job State Changed: %d (%s)\n",
                             (int)current_job_ckpt_state, state_str ));
        free(state_str);
        state_str = NULL;

        /*
         * Notify the orte-checkpoint command
         */
        if( is_orte_checkpoint_connected &&
            ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                                global_snapshot.ss_handle,
                                                                                current_job_ckpt_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_RECOVERED);

        /*
         * If the checkpoint has been established at this point, then cleanup.
         */
        if( !cleanup_on_establish && ORTE_SNAPC_CKPT_STATE_RECOVERED == current_job_ckpt_state) {
            if( ORTE_SUCCESS != (ret = orte_snapc_full_global_reset_coord()) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }

 cleanup:
    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    return exit_status;
}

static void snapc_full_process_restart_proc_info_cmd(orte_process_name_t* sender,
                                                     opal_buffer_t* buffer)
{
    int ret;
    orte_std_cntr_t count;
    size_t num_vpids = 0, i;
    pid_t tmp_pid;
    char * tmp_hostname = NULL;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &tmp_hostname, &count, OPAL_STRING))) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) vpid_assoc: Failed to unpack process Hostname from peer %s\n",
                    ORTE_NAME_PRINT(sender));
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_vpids, &count, OPAL_SIZE))) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) vpid_assoc: Failed to unpack num_vpids from peer %s\n",
                    ORTE_NAME_PRINT(sender));
        goto cleanup;
    }

    for(i = 0; i < num_vpids; ++i) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &tmp_pid, &count, OPAL_PID))) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "Global) vpid_assoc: Failed to unpack process PID from peer %s\n",
                        ORTE_NAME_PRINT(sender));
            goto cleanup;
        }

        global_coord_restart_proc_info(tmp_pid, tmp_hostname);
    }

    /* stdout may be buffered by the C library so it needs to be flushed so
     * that the debugger can read the process info.
     */
    fflush(stdout);

 cleanup:
    return;
}

int global_coord_restart_proc_info(pid_t local_pid, char * local_hostname)
{
    printf("MPIR_debug_info) %s:%d\n", local_hostname, local_pid);
    return 0;
}

static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer,
                                              bool quick)
{
    int ret;
    orte_std_cntr_t count;
    orte_jobid_t jobid;
    int   job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
    opal_crs_base_ckpt_options_t *options = NULL;
    bool loc_migrating = false;
    size_t loc_num_procs = 0;
    orte_proc_t *proc = NULL;
    size_t i;
    orte_sstore_base_handle_t ss_handle;

    /*
     * Unpack the data (quick)
     * - jobid
     * - ckpt_state
     * - sstore_handle
     * Unpack the data (long)
     * - jobid
     * - ckpt_state
     * - ckpt_options
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        return;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_state, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        return;
    }

    if( !quick ) {
        if (ORTE_SUCCESS != (ret = orte_sstore.unpack_handle(sender, buffer, &ss_handle)) ) {
            ORTE_ERROR_LOG(ret);
            return;
        }

        options = OBJ_NEW(opal_crs_base_ckpt_options_t);
        if( ORTE_SUCCESS != (ret = orte_snapc_base_unpack_options(buffer, options)) ) {
            ORTE_ERROR_LOG(ret);
            return;
        }
        /* In this case we want to use the current_options that are cached
         * so that we do not have to send them every time.
         */
        opal_crs_base_copy_options(options, global_snapshot.options);

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(loc_migrating), &count, OPAL_BOOL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if( loc_migrating ) {
            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &loc_num_procs, &count, OPAL_SIZE))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            for( i = 0; i < loc_num_procs; ++i ) {
                count = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc, &count, ORTE_NAME))) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
                /* JJH: Update local info as needed */
            }
        }
    }

    if( ORTE_SUCCESS != (ret = global_coord_job_state_update(jobid,
                                                             job_ckpt_state,
                                                             ss_handle,
                                                             global_snapshot.options) ) ) {
        ORTE_ERROR_LOG(ret);
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}

static int snapc_full_establish_snapshot_dir(bool empty_metadata)
{
    char **value = NULL;
    int idx = 0;

    /*********************
     * Contact the Stable Storage Framework to setup the storage directory
     *********************/
    INC_SEQ_NUM();
    orte_sstore.request_checkpoint_handle(&(global_snapshot.ss_handle),
                                          orte_snapc_base_snapshot_seq_number,
                                          current_global_jobid);
    if( currently_migrating ) {
        orte_sstore.set_attr(global_snapshot.ss_handle,
                             SSTORE_METADATA_GLOBAL_MIGRATING,
                             "1");
    }
    orte_sstore.register_handle(global_snapshot.ss_handle);

    /*
     * Save the AMCA parameter used into the metadata file
     */
    if( 0 > (idx = mca_base_var_find("opal", "mca", "base", "param_file_prefix")) ) {
        opal_show_help("help-orte-restart.txt", "amca_param_not_found", true);
    }
    if( 0 < idx ) {
        mca_base_var_get_value (idx, &value, NULL, NULL);

        if (*value) {
            orte_sstore.set_attr(global_snapshot.ss_handle,
                                 SSTORE_METADATA_GLOBAL_AMCA_PARAM,
                                 *value);

            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) AMCA Parameter Preserved: %s",
                                 *value));
        }
    }

    return ORTE_SUCCESS;
}

static int snapc_full_global_checkpoint(opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Checkpoint of job %s has been requested\n",
                         ORTE_JOBID_PRINT(current_global_jobid)));

    /* opal_output(0, "================> JJH Checkpoint Started"); */

    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_REQUEST;

    /*********************
     * Generate the global snapshot directory, and unique global snapshot handle
     *********************/
    if( ORTE_SUCCESS != (ret = snapc_full_establish_snapshot_dir(false))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /***********************************
     * Do an update handshake with the orte_checkpoint command
     ***********************************/
    updated_job_to_running = false;
    if( is_orte_checkpoint_connected &&
        ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                            global_snapshot.ss_handle,
                                                                            current_job_ckpt_state) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /**********************
     * Notify the Local Snapshot Coordinators of the checkpoint request
     **********************/
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notifying the Local Coordinators\n"));

    if( ORTE_SUCCESS != (ret = snapc_full_global_notify_checkpoint(current_global_jobid, options)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int  snapc_full_global_notify_checkpoint(orte_jobid_t jobid,
                                                opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    int ckpt_state;

    ckpt_state = ORTE_SNAPC_CKPT_STATE_PENDING;

    /*
     * Copy over the options
     */
    opal_crs_base_copy_options(options, global_snapshot.options);

    /*
     * Update the global structure
     */
    for(item  = opal_list_get_first(&global_snapshot.local_snapshots);
        item != opal_list_get_end(&global_snapshot.local_snapshots);
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        orted_snapshot->state   = ckpt_state;
    }

    /*
     * Update the job state, and broadcast to all local daemons
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(jobid,
                                                                        ckpt_state,
                                                                        global_snapshot.ss_handle,
                                                                        false, options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

/**********************************
 * Job/Proc State Set/Get Routines
 **********************************/
static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     int    ckpt_state,
                                                     orte_sstore_base_handle_t handle,
                                                     bool quick,
                                                     opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command;
    opal_buffer_t *buffer = NULL;
    char * state_str = NULL;
    orte_proc_t *proc = NULL;
    opal_list_item_t *item = NULL;
    size_t num_procs;
    orte_grpcomm_signature_t *sig;

    /*
     * Update all Local Coordinators (broadcast operation)
     */
    buffer = OBJ_NEW(opal_buffer_t);

    if( quick ) {
        command = ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD;
    } else {
        command = ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &ckpt_state, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( quick ) {
        goto process_msg;
    }

    if (ORTE_SUCCESS != (ret = orte_sstore.pack_handle(NULL, buffer, handle))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if(ORTE_SUCCESS != (ret = orte_snapc_base_pack_options(buffer, options))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(currently_migrating), 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( currently_migrating ) {
        num_procs = opal_list_get_size(migrating_procs);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &num_procs, 1, OPAL_SIZE))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        for (item  = opal_list_get_first(migrating_procs);
             item != opal_list_get_end(migrating_procs);
             item  = opal_list_get_next(item)) {
            proc = (orte_proc_t*)item;
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(proc->name), 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }

 process_msg:
    orte_snapc_ckpt_state_str(&state_str, ckpt_state);
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notify Local Coordinators of job %s state change to %d (%s)\n",
                         ORTE_JOBID_PRINT(jobid), (int)ckpt_state, state_str ));
    free(state_str);
    state_str = NULL;

    /* goes to all daemons */
    sig = OBJ_NEW(orte_grpcomm_signature_t);
    sig->signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    sig->signature[0].jobid = ORTE_PROC_MY_NAME->jobid;
    sig->signature[0].vpid = ORTE_VPID_WILDCARD;
    if (ORTE_SUCCESS != (ret = orte_grpcomm.xcast(sig, ORTE_RML_TAG_SNAPC_FULL, buffer))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * We will also receive the job update, and process in the RML callback
     */

 cleanup:
    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    OBJ_RELEASE(buffer);
    OBJ_RELEASE(sig);

    return exit_status;
}

int global_coord_job_state_update(orte_jobid_t jobid,
                                  int    job_ckpt_state,
                                  orte_sstore_base_handle_t ss_handle,
                                  opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    char * state_str = NULL;

    orte_snapc_ckpt_state_str(&state_str, job_ckpt_state);
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Job update command: jobid %s -> state %d (%s)\n",
                         ORTE_JOBID_PRINT(jobid), (int)job_ckpt_state, state_str ));
    free(state_str);
    state_str = NULL;

    /************************
     * Update the orte_checkpoint command
     ************************/
    current_job_ckpt_state = job_ckpt_state;
    if( is_orte_checkpoint_connected &&
        ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                            global_snapshot.ss_handle,
                                                                            current_job_ckpt_state)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Global Coordinator: If also a Local coordinator then act locally before globally
     */
    if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE) ) {
        if( ORTE_SUCCESS != (ret = local_coord_job_state_update(jobid,
                                                                job_ckpt_state,
                                                                ss_handle,
                                                                options)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Process the cmd
     */
    if(ORTE_SNAPC_CKPT_STATE_ESTABLISHED == job_ckpt_state ) {
        /*
         * If the processes recovered before the checkpoint was established,
         * then we need to cleanup here instead of in the recovery block
         */
        if( cleanup_on_establish ) {
            if( ORTE_SUCCESS != (ret = orte_snapc_full_global_reset_coord()) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }
    else if(ORTE_SNAPC_CKPT_STATE_ERROR     == job_ckpt_state ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Error: Checkpoint failed!");
    }
    /*
     * This should not happen, since this state is always handled locally
     */
    else if(ORTE_SNAPC_CKPT_STATE_STOPPED == job_ckpt_state ) {
        ;
    }
    /*
     * This should not happen, since we do not handle this case
     */
    else if(ORTE_SNAPC_CKPT_STATE_REQUEST == job_ckpt_state ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "ERROR: Internal Checkpoint request not implemented.");
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
    }

 cleanup:
    if( NULL != state_str) {
        free(state_str);
        state_str = NULL;
    }

    return exit_status;
}

static int write_out_global_metadata(void)
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Updating Metadata"));

    /*
     * Check for an error
     * JJH CLEANUP: Check might be good, but mostly unnecessary
     * JJH: Do we want to pass this along to the SStore? Probably
     */
    for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
        orted_item  = opal_list_get_next(orted_item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

        if( ORTE_SNAPC_CKPT_STATE_ERROR == orted_snapshot->state ) {
            return ORTE_ERROR;
        }
    }

    /*
     * Sync the stable storage
     */
    orte_sstore.sync(global_snapshot.ss_handle);

    SNAPC_FULL_SET_TIMER(SNAPC_FULL_TIMER_SS_SYNC);

    return ORTE_SUCCESS;
}

static orte_snapc_full_orted_snapshot_t *find_orted_snapshot(orte_process_name_t *name )
{
    int ret;

    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    orte_ns_cmp_bitmask_t mask;

    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, name, &orted_snapshot->process_name)) {
            return orted_snapshot;
        }
    }

    /*
     * Refresh the job structure, and try again
     */
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) find_orted(%s) failed. Refreshing and trying again...",
                         ORTE_NAME_PRINT(name) ));

    if( ORTE_SUCCESS != (ret = global_refresh_job_structs()) ) {
        ORTE_ERROR_LOG(ret);
        return NULL;
    }

    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, name, &orted_snapshot->process_name)) {
            return orted_snapshot;
        }
    }

    return NULL;
}

static int snapc_full_global_get_min_state(void)
{
    int min_state = ORTE_SNAPC_CKPT_MAX;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    char * state_str_a = NULL;
    char * state_str_b = NULL;

    current_total_orteds = 0;

    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        /* Ignore orteds with no processes */
        if( 0 >= opal_list_get_size(&(orted_snapshot->super.local_snapshots)) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) ... %s Skipping - (no children)",
                                 ORTE_NAME_PRINT(&orted_snapshot->process_name) ));
            continue;
        }

        current_total_orteds++;

        if( NULL != state_str_a ) {
            free(state_str_a);
            state_str_a = NULL;
        }
        if( NULL != state_str_b ) {
            free(state_str_b);
            state_str_b = NULL;
        }

        orte_snapc_ckpt_state_str(&state_str_a, orted_snapshot->state);
        orte_snapc_ckpt_state_str(&state_str_b, min_state);

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) ... %s Checking [%d %s] vs [%d %s]",
                             ORTE_NAME_PRINT(&orted_snapshot->process_name),
                             (int)orted_snapshot->state, state_str_a,
                             min_state, state_str_b ));

        if( (int)min_state > (int)orted_snapshot->state ) {
            min_state = orted_snapshot->state;

            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) ... %s Update  --> Min State [%d %s]",
                                 ORTE_NAME_PRINT(&orted_snapshot->process_name),
                                 (int)min_state, state_str_a ));
        }
    }

    if( NULL != state_str_b ) {
        free(state_str_b);
        state_str_b = NULL;
    }
    orte_snapc_ckpt_state_str(&state_str_b, min_state);
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) ... Min State [%d %s]",
                         (int)min_state, state_str_b ));

    if( NULL != state_str_a ) {
        free(state_str_a);
        state_str_a = NULL;
    }
    if( NULL != state_str_b ) {
        free(state_str_b);
        state_str_b = NULL;
    }

    return min_state;
}

static int orte_snapc_full_global_reset_coord(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    opal_list_item_t* aitem = NULL;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;


    /********************************
     * Terminate the job if requested
     * At this point the application should have already exited, but do this
     * just to make doubly sure that the job is terminated.
     *********************************/
    if( global_snapshot.options->term ) {
        SNAPC_FULL_DISPLAY_ALL_TIMERS();
        orte_plm.terminate_job(current_global_jobid);
    } else {
        SNAPC_FULL_DISPLAY_ALL_TIMERS();
    }

    /*
     * Just cleanup, do not need to send out another message
     */
    opal_crs_base_clear_options(global_snapshot.options);

    /*
     * Reset global data structures
     */
    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        orted_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

        for(aitem  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
            aitem != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
            aitem  = opal_list_get_next(aitem) ) {
            app_snapshot = (orte_snapc_base_local_snapshot_t*)aitem;

            app_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;
        }
    }

    /************************
     * Set up the Command Line listener again
     *************************/
    is_orte_checkpoint_connected = false;
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener() ) ){
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
    cleanup_on_establish = false;

    report_progress_cur_loc_finished = 0;
    report_progress_last_reported_loc_finished = 0;

    return exit_status;
}

/************************
 * Timing
 ************************/
static void snapc_full_set_time(int idx)
{
    if(idx < SNAPC_FULL_TIMER_MAX ) {
        if( timer_start[idx] <= 0.0 ) {
            timer_start[idx] = snapc_full_get_time();
        }
    }
}

static void snapc_full_display_all_timers(void)
{
    double diff = 0.0;
    char * label = NULL;

    opal_output(0, "Snapshot Coordination Timing: ******************** Summary Begin\n");

    /********** Startup time **********/
    label = strdup("Running");
    diff = timer_start[SNAPC_FULL_TIMER_RUNNING]   - timer_start[SNAPC_FULL_TIMER_START];
    snapc_full_display_indv_timer_core(diff, label);
    free(label);

    /********** Time to finish locally **********/
    label = strdup("Finish Locally");
    diff = timer_start[SNAPC_FULL_TIMER_FIN_LOCAL] - timer_start[SNAPC_FULL_TIMER_RUNNING];
    snapc_full_display_indv_timer_core(diff, label);
    free(label);

    if( timer_start[SNAPC_FULL_TIMER_SS_SYNC] <= timer_start[SNAPC_FULL_TIMER_RECOVERED] ) {
        /********** SStore Sync **********/
        label = strdup("SStore Sync");
        diff = timer_start[SNAPC_FULL_TIMER_SS_SYNC]   - timer_start[SNAPC_FULL_TIMER_FIN_LOCAL];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);

        /********** Establish Ckpt **********/
        label = strdup("Establish");
        diff = timer_start[SNAPC_FULL_TIMER_ESTABLISH]   - timer_start[SNAPC_FULL_TIMER_SS_SYNC];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);

        /********** Recover **********/
        label = strdup("Continue/Recover");
        diff = timer_start[SNAPC_FULL_TIMER_RECOVERED] - timer_start[SNAPC_FULL_TIMER_ESTABLISH];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);
    } else { /* Established after procs recovered */
        /********** SStore Sync **********/
        label = strdup("SStore Sync*");
        diff = timer_start[SNAPC_FULL_TIMER_SS_SYNC]   - timer_start[SNAPC_FULL_TIMER_RECOVERED];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);

        /********** Establish Ckpt **********/
        label = strdup("Establish*");
        diff = timer_start[SNAPC_FULL_TIMER_ESTABLISH]   - timer_start[SNAPC_FULL_TIMER_SS_SYNC];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);

        /********** Recover **********/
        label = strdup("Continue/Recover*");
        diff = timer_start[SNAPC_FULL_TIMER_RECOVERED] - timer_start[SNAPC_FULL_TIMER_FIN_LOCAL];
        snapc_full_display_indv_timer_core(diff, label);
        free(label);
    }

    opal_output(0, "Snapshot Coordination Timing: ******************** Summary End\n");
}

static void snapc_full_display_recovered_timers(void)
{
    double diff = 0.0;
    char * label = NULL;

    opal_output(0, "Snapshot Coordination Timing: ******************** Summary Begin\n");

    /********** Recover **********/
    label = strdup("Recover");
    diff = timer_start[SNAPC_FULL_TIMER_RECOVERED] - timer_start[SNAPC_FULL_TIMER_START];
    snapc_full_display_indv_timer_core(diff, label);
    free(label);

    opal_output(0, "Snapshot Coordination Timing: ******************** Summary End\n");
}

static void snapc_full_clear_timers(void)
{
    int i;
    for(i = 0; i < SNAPC_FULL_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
    }
}

static double snapc_full_get_time(void)
{
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void snapc_full_display_indv_timer_core(double diff, char *str)
{
    double total = 0;
    double perc  = 0;

    if( timer_start[SNAPC_FULL_TIMER_SS_SYNC] <= timer_start[SNAPC_FULL_TIMER_RECOVERED] ) {
        total = timer_start[SNAPC_FULL_TIMER_RECOVERED] - timer_start[SNAPC_FULL_TIMER_START];
    } else {
        total = timer_start[SNAPC_FULL_TIMER_ESTABLISH] - timer_start[SNAPC_FULL_TIMER_START];
    }
    perc = (diff/total) * 100;

    opal_output(0,
                "snapc_full: timing: %-20s = %10.2f s\t%10.2f s\t%6.2f\n",
                str,
                diff,
                total,
                perc);
    return;
}

static void snapc_full_report_progress(orte_snapc_full_orted_snapshot_t *orted_snapshot, int total, int min_state)
{
    orte_snapc_full_orted_snapshot_t *loc_orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    double perc_done;

    if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL != orted_snapshot->state ) {
        return;
    }

    report_progress_cur_loc_finished++;
    perc_done = (total-report_progress_cur_loc_finished)/(total*1.0);
    perc_done = (perc_done-1)*(-100.0);

    if( perc_done >= (report_progress_last_reported_loc_finished + orte_snapc_full_progress_meter) ||
        report_progress_last_reported_loc_finished == 0.0 ) {
        report_progress_last_reported_loc_finished = perc_done;
        opal_output(0, "snapc_full: progress:   %10.2f %c Locally Finished\n",
                    perc_done, '%');
    }

    if( perc_done > 95.0 ) {
        opal_output(0, "snapc_full: progress:   Waiting on the following daemons (%10.2f %c):", perc_done, '%');

        for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
            item != opal_list_get_end(&(global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            loc_orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

            if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL != loc_orted_snapshot->state ) {
                opal_output(0, "snapc_full: progress:        Daemon %s",
                            ORTE_NAME_PRINT(&loc_orted_snapshot->process_name));
            }
        }
    }

    return;
}
