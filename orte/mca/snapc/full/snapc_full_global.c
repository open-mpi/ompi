/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
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

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

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

static bool snapc_recv_issued = false;
static bool snapc_cmdline_recv_issued = false;

static int snapc_full_global_start_listener(void);
static int snapc_full_global_stop_listener(void);
static int snapc_full_global_start_cmdline_listener(void);
static int snapc_full_global_stop_cmdline_listener(void);

static void snapc_full_global_cmd_recv(int status,
                                       orte_process_name_t* sender,
                                       opal_buffer_t* buffer,
                                       orte_rml_tag_t tag,
                                       void* cbdata);
static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer);
static void snapc_full_process_proc_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer);
static void snapc_full_process_vpid_assoc_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer);
static void snapc_full_process_establish_dir_cmd(orte_process_name_t* sender,
                                                 opal_buffer_t* buffer);
static void snapc_full_process_cmdline_request_cmd(orte_process_name_t* sender,
                                                   opal_buffer_t* buffer);

int global_coord_job_state_update(orte_jobid_t jobid,
                                  size_t job_ckpt_state,
                                  char **job_ckpt_snapshot_ref,
                                  char **job_ckpt_snapshot_loc);

static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     size_t ckpt_state, 
                                                     char  *ckpt_snapshot_ref,
                                                     char  *ckpt_snapshot_loc);
static int orte_snapc_full_global_set_vpid_ckpt_info( orte_process_name_t proc,
                                                      size_t ckpt_state, 
                                                      char  *ckpt_snapshot_ref, 
                                                      char  *ckpt_snapshot_loc);

static int  snapc_full_get_vpid_range( orte_jobid_t jobid, 
                                       orte_vpid_t *vpid_start, 
                                       orte_vpid_t *vpid_range);

static int  snapc_full_global_checkpoint(orte_jobid_t jobid, 
                                         bool term, 
                                         char **global_snapshot_handle, 
                                         int *ckpt_status);

static int  snapc_full_global_notify_checkpoint( char *       global_snapshot_handle,
                                                 orte_jobid_t jobid, 
                                                 bool term);

static int snapc_full_global_check_for_done(orte_jobid_t jobid);

static int  snapc_full_global_gather_all_files(void);
static bool snapc_full_global_is_done_yet(void);

static orte_snapc_base_global_snapshot_t global_snapshot;
static orte_process_name_t orte_checkpoint_sender = {0,0};
static bool updated_job_to_running;

static size_t cur_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
static orte_jobid_t cur_job_id = 0;

/************************
 * Function Definitions
 ************************/
int global_coord_init(void) {

    return ORTE_SUCCESS;
}

int global_coord_finalize(void) {

    return ORTE_SUCCESS;
}

int global_coord_setup_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;
    orte_vpid_t vpid_start = 0, vpid_range = 0;
    orte_vpid_t i;

    /*
     * If we have already setup a jobid, warn
     */
    /*
     * If we pass this way twice the first time will have been from:
     *   orte_plm_base_setup_job(): As the global coordinator
     * The second time will have been from:
     *   odls_default_module.c: As the local coordinator.
     * The later case means that we (as the HNP) are acting as both the global and
     * local coordinators.
     * JJH FIX NOTE:
     *   This fix imposes the restriction that only one jobid can be checkpointed
     *   at a time. In the future we will want to lift this restriction.
     */
    if( 0 >= cur_job_id ) {
        /* Global Coordinator pass */
        cur_job_id = jobid;
    }
    else if ( jobid == cur_job_id ) {
        /* Local Coordinator pass -- Will always happen after Global Coordinator Pass */
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) [%d] Setup job %s again as the local coordinator for %s\n",
                             getpid(), ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(cur_job_id)));

        orte_snapc_coord_type |= ORTE_SNAPC_LOCAL_COORD_TYPE;

        return local_coord_setup_job(jobid);
    }
    else {
        /* Already setup things for another job,
         * We do not currently support the ability to checkpoint more than one 
         * jobid
         */
        opal_output(mca_snapc_full_component.super.output_handle,
                    "global [%d]) Setup job (%d) Failed. Already setup job (%d)\n", getpid(), jobid, cur_job_id);
        return ORTE_ERROR;
    }

    /*
     * Start out with a sequence number just below the first
     * This will be incremented when we checkpoint
     */
    orte_snapc_base_snapshot_seq_number = -1;

    /*
     * Get vpid range
     */
    if( ORTE_SUCCESS != (ret = snapc_full_get_vpid_range(jobid, &vpid_start, &vpid_range) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Allocate the snapshot structures
     */
    OBJ_CONSTRUCT(&global_snapshot, orte_snapc_base_global_snapshot_t);
    global_snapshot.component_name = strdup(mca_snapc_full_component.super.base_version.mca_component_name);
    for(i = vpid_start; i < vpid_start + vpid_range; ++i) {
        orte_snapc_full_global_snapshot_t *vpid_snapshot;
        
        vpid_snapshot = OBJ_NEW(orte_snapc_full_global_snapshot_t);

        vpid_snapshot->super.process_name.jobid  = jobid;
        vpid_snapshot->super.process_name.vpid   = i;
        vpid_snapshot->super.term = false;

        opal_list_append(&global_snapshot.snapshots, &(vpid_snapshot->super.crs_snapshot_super.super));
    }

    /*
     * Setup Global Coordinator command processing listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_listener()) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup command line tool checkpoint request listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener()) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If requested pre-establish the global snapshot directory
     */
    if(orte_snapc_base_establish_global_snapshot_dir) {
        char *global_snapshot_handle = NULL;
        char *global_dir = NULL;
        
        INC_SEQ_NUM();
        global_snapshot_handle = strdup( orte_snapc_base_unique_global_snapshot_name( getpid() ) );
        global_dir = orte_snapc_base_get_global_snapshot_directory(global_snapshot_handle);
        orte_snapc_base_global_snapshot_loc = strdup(global_dir);

        global_snapshot.seq_num = orte_snapc_base_snapshot_seq_number;
        global_snapshot.reference_name = strdup(global_snapshot_handle);
        global_snapshot.local_location = opal_dirname(orte_snapc_base_get_global_snapshot_directory(global_snapshot.reference_name));

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Pre-establish the global snapshot directory\n"));

        /* Creates the directory (with metadata files):
         *   /tmp/ompi_global_snapshot_PID.ckpt/seq_num
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_init_global_snapshot_directory(global_snapshot_handle, true))) {
            exit_status = ret;
            goto cleanup;
        }

        free(global_snapshot_handle);
        global_snapshot_handle = NULL;
        
        free(global_dir);
        global_dir = NULL;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) [%d] Setup job %s with vpid [%d, %d]\n",
                         getpid(), ORTE_JOBID_PRINT(jobid), vpid_start, vpid_range));

 cleanup:
    return exit_status;
}

int global_coord_release_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Make sure we are not waiting on a checkpoint to complete
     */

    /*
     * Clean up listeners
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_cmdline_listener()) ) {
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_listener()) ) {
        exit_status = ret;
    }

    OBJ_DESTRUCT(&global_snapshot);
    
    return exit_status;
}

/******************
 * Local functions
 ******************/
static int snapc_full_global_start_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;

    if (snapc_recv_issued && orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive: Start command recv"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_SNAPC_FULL,
                                                      ORTE_RML_PERSISTENT,
                                                      snapc_full_global_cmd_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_recv_issued = true;
    
 cleanup:
    return exit_status;
}

static int snapc_full_global_stop_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;
    
    if (!snapc_recv_issued && orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive stop command recv"));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                   ORTE_RML_TAG_SNAPC_FULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_recv_issued = false;
    
 cleanup:
    return exit_status;
}

static int snapc_full_global_start_cmdline_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;

    if (snapc_cmdline_recv_issued && orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive (Command line): Start command recv"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_CKPT,
                                                      0,
                                                      snapc_full_global_cmd_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_cmdline_recv_issued = true;

 cleanup:
    return exit_status;
}

static int snapc_full_global_stop_cmdline_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;
    
    if (!snapc_cmdline_recv_issued && orte_process_info.hnp) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive (Command Line) stop command"));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                   ORTE_RML_TAG_CKPT))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_cmdline_recv_issued = false;
    
 cleanup:
    return exit_status;
}

void snapc_full_global_cmd_recv(int status,
                                orte_process_name_t* sender,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                void* cbdata)
{
    orte_snapc_full_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive a command message from %s.",
                         ORTE_NAME_PRINT(sender)));

    /*
     * If this is a command line checkpoint request, handle directly
     */
    if( ORTE_RML_TAG_CKPT == tag ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command Line: Start a checkpoint operation"));

        snapc_cmdline_recv_issued = false; /* Not a persistent RML message */
        snapc_full_process_cmdline_request_cmd(sender, buffer);
        return;
    }

    /*
     * Otherwise this is an inter-coordinator command (usually updating state info).
     */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Update Job state command"));

            snapc_full_process_job_update_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_FULL_UPDATE_PROC_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Update Proc state command"));

            snapc_full_process_proc_update_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_FULL_VPID_ASSOC_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Update process/orted associations"));

            snapc_full_process_vpid_assoc_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_FULL_ESTABLISH_DIR_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Establish checkpoint directory"));

            snapc_full_process_establish_dir_cmd(sender, buffer);
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
}

static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_jobid_t jobid;
    size_t job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
    char  *job_ckpt_snapshot_ref = NULL;
    char  *job_ckpt_snapshot_loc = NULL;

    /*
     * If we sent this message to ourself then we will process it elsewhere
     */
    if( sender->jobid == ORTE_PROC_MY_NAME->jobid &&
        sender->vpid  == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command: Reflect the job update command"));
        return;
    }

    /*
     * Unpack the data
     * - jobid
     * - ckpt_state
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_state, &count, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_snapshot_ref, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_snapshot_loc, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = global_coord_job_state_update(jobid,
                                                             job_ckpt_state,
                                                             &job_ckpt_snapshot_ref,
                                                             &job_ckpt_snapshot_loc) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return;
}

int global_coord_job_state_update(orte_jobid_t jobid,
                                  size_t job_ckpt_state,
                                  char **job_ckpt_snapshot_ref,
                                  char **job_ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    bool term_job  = false;

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Job update command: jobid %s -> state %d\n",
                         ORTE_JOBID_PRINT(jobid), (int)job_ckpt_state));

    /************************
     * Update the orte_checkpoint command
     ************************/
    cur_job_ckpt_state = job_ckpt_state;
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender, 
                                                                            global_snapshot.reference_name,
                                                                            global_snapshot.seq_num,
                                                                            cur_job_ckpt_state)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Global Coordinator: If also a Local coordinator then act locally before globally
     */
    if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE) ) {
        if( ORTE_SUCCESS != (ret = local_coord_job_state_update(jobid, job_ckpt_state, job_ckpt_snapshot_ref, job_ckpt_snapshot_loc)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if(ORTE_SNAPC_CKPT_STATE_REQUEST == job_ckpt_state ) {
#if 0
        /*
         * Start the checkpoint, now that we have the jobid
         */
        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(jobid, term, &global_snapshot_handle, &job_ckpt_state) ) ) {
            exit_status = ret;
            goto cleanup;
        }
#else
        opal_output(mca_snapc_full_component.super.output_handle,
                    "ERROR: Internal Checkpoint request not implemented.");
#endif
    }
    /*
     * If we need to transfer files
     */
    else if( ORTE_SNAPC_CKPT_STATE_FILE_XFER == job_ckpt_state ) {
        /**********************
         * Gather all of the files locally
         * Note: We don't need to worry about the return code in as much since the
         *       rest of the functions know what to do with an error scenario.
         **********************/
        if( ORTE_SUCCESS != (ret = snapc_full_global_gather_all_files()) ) {
            exit_status = ret;
            cur_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_ERROR;
        }

        /**********************************
         * Update the job checkpoint state
         **********************************/
        if( ORTE_SNAPC_CKPT_STATE_ERROR != cur_job_ckpt_state ) {
            cur_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_FINISHED;
        }

        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(jobid,
                                                                            cur_job_ckpt_state,
                                                                            global_snapshot.reference_name,
                                                                            orte_snapc_base_global_snapshot_loc) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_SNAPC_CKPT_STATE_FINISHED  == job_ckpt_state ||
             ORTE_SNAPC_CKPT_STATE_ERROR     == job_ckpt_state ) {
        /***********************************
         * Update the vpid checkpoint state
         ***********************************/
        for(item  = opal_list_get_first(&global_snapshot.snapshots);
            item != opal_list_get_end(&global_snapshot.snapshots);
            item  = opal_list_get_next(item) ) {
            orte_snapc_full_global_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

            vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_NONE;

            if( vpid_snapshot->super.term ){
                term_job = true;
            }

            if (ORTE_SUCCESS != (ret = orte_snapc_full_global_set_vpid_ckpt_info(vpid_snapshot->super.process_name,
                                                                                 vpid_snapshot->super.state,
                                                                                 vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                                 vpid_snapshot->super.crs_snapshot_super.local_location) ) ) {
                exit_status = ret;
                goto cleanup;
            }
        }

        /************************
         * Set up the Command Line listener again
         *************************/
        if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener() ) ){
            exit_status = ret;
        }

        /********************************
         * Terminate the job if requested
         * At this point the application should have already exited, but do this
         * just to make doubly sure that the job is terminated.
         *********************************/
        if( term_job ) {
            orte_plm.terminate_job(jobid);
        }
    }

 cleanup:
    return exit_status;
}

static void snapc_full_process_proc_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_process_name_t remote_proc;
    char *remote_ckpt_ref = NULL, *remote_ckpt_loc = NULL;
    size_t remote_ckpt_state;

    /*
     * Unpack the data
     * - process name
     * - ckpt_state
     * - ckpt_ref
     * - ckpt_loc
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_proc, &count, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_state, &count, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_ref, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_loc, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = global_coord_vpid_state_update(remote_proc, remote_ckpt_state, &remote_ckpt_ref, &remote_ckpt_loc)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return;
}

int global_coord_vpid_state_update(orte_process_name_t proc_name,
                                   size_t proc_ckpt_state,
                                   char **proc_ckpt_ref,
                                   char **proc_ckpt_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_global_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Process %s: Changed to state to:\n",
                         ORTE_NAME_PRINT(&proc_name)));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   State:            %d\n",
                         (int)proc_ckpt_state));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   Snapshot Ref:    [%s]\n",
                         *proc_ckpt_ref));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   Remote Location: [%s]\n",
                         *proc_ckpt_loc));

    /*
     * Find this process and update it's information
     */
    for(item  = opal_list_get_first(&global_snapshot.snapshots);
        item != opal_list_get_end(&global_snapshot.snapshots);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

        if(vpid_snapshot->super.process_name.jobid  == proc_name.jobid &&
           vpid_snapshot->super.process_name.vpid   == proc_name.vpid) {

            vpid_snapshot->super.state               = proc_ckpt_state;
            vpid_snapshot->super.crs_snapshot_super.reference_name  = strdup(*proc_ckpt_ref);
            vpid_snapshot->super.crs_snapshot_super.remote_location = strdup(*proc_ckpt_loc);
            
            if(proc_ckpt_state == ORTE_SNAPC_CKPT_STATE_FINISHED ||
               proc_ckpt_state == ORTE_SNAPC_CKPT_STATE_ERROR ) {
                snapc_full_global_check_for_done(vpid_snapshot->super.process_name.jobid);
            }
            break;
        }
    }

    /*
     * Update the global struct
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_vpid_ckpt_info(proc_name,
                                                                         proc_ckpt_state,
                                                                         *proc_ckpt_ref,
                                                                         *proc_ckpt_loc))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Update the job to running
     */
    if( !updated_job_to_running) {
        char * global_dir = NULL;
        global_dir = orte_snapc_base_get_global_snapshot_directory(global_snapshot.reference_name);

        orte_snapc_base_global_snapshot_loc = strdup(global_dir);

        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(proc_name.jobid,
                                                                            ORTE_SNAPC_CKPT_STATE_RUNNING,
                                                                            global_snapshot.reference_name,
                                                                            global_dir) ) ) {
            free(global_dir);
            exit_status = ret;
            goto cleanup;
        }

        free(global_dir);
        updated_job_to_running = true;
    }

 cleanup:
    return exit_status;
}

static void snapc_full_process_vpid_assoc_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer)
{
    int ret;
    orte_std_cntr_t count;
    orte_process_name_t tmp_proc_name;
    size_t num_vpids = 0, i;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_vpids, &count, OPAL_SIZE))) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) vpid_assoc: Failed to unpack num_vpids from peer %s\n",
                    ORTE_NAME_PRINT(sender));
        goto cleanup;
    }

    for(i = 0; i < num_vpids; ++i) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &tmp_proc_name, &count, ORTE_NAME))) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "Global) vpid_assoc: Failed to unpack process name from peer %s\n",
                        ORTE_NAME_PRINT(sender));
            goto cleanup;
        }

        global_coord_vpid_assoc_update(*sender, tmp_proc_name);
    }

 cleanup:
    return;
}

int global_coord_vpid_assoc_update(orte_process_name_t local_coord,
                                   orte_process_name_t proc_name)
{
    orte_snapc_full_global_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&global_snapshot.snapshots);
        item != opal_list_get_end(&global_snapshot.snapshots);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

        if(vpid_snapshot->super.process_name.jobid  == proc_name.jobid &&
           vpid_snapshot->super.process_name.vpid   == proc_name.vpid) {
            vpid_snapshot->local_coord.vpid  = local_coord.vpid;
            vpid_snapshot->local_coord.jobid = local_coord.jobid;
            break;                
        }
    }

    return ORTE_SUCCESS;
}

static void snapc_full_process_establish_dir_cmd(orte_process_name_t* sender,
                                                 opal_buffer_t* exbuf)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_ESTABLISH_DIR_CMD;
    opal_buffer_t buffer;

    /* Send back:
     * - Reference
     * - Local location
     */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(global_snapshot.reference_name), 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(orte_snapc_base_global_snapshot_loc), 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(sender, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);
    return;
}

static void snapc_full_process_cmdline_request_cmd(orte_process_name_t* sender,
                                                   opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_cmd_flag_t command;
    orte_std_cntr_t count = 1;
    bool term = false;
    int ckpt_status = ORTE_SUCCESS;
    char *global_snapshot_handle = NULL;
    orte_jobid_t jobid;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_CMD))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * orte_checkpoint has requested that a checkpoint be taken
     */
    if (ORTE_SNAPC_GLOBAL_INIT_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested a checkpoint (command %d)\n",
                             command));
        /********************
         * Do the basic handshake with the orte_checkpoint command
         ********************/
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_init_cmd(sender, buffer, &term, &jobid)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /* If the command line did not specify a jobid, then use the one we
         * are watching by default
         */
        if( ORTE_JOBID_INVALID == jobid ) {
            jobid = cur_job_id;
        }

        /* Save things */
        orte_checkpoint_sender = *sender;
        
        /*************************
         * Kick off the checkpoint
         *************************/
        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(jobid, term, &global_snapshot_handle, &ckpt_status) ) ) {
            exit_status = ret;
            /* We don't want to terminate here, becase orte_checkpoint may be waiting for
             * us to come back with something, so just send back the empty values, and
             * it will know what to do
             */
        }
        
    }
    else if (ORTE_SNAPC_GLOBAL_TERM_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested to terminate connection (command %d)\n",
                             command));
        /* Something must have happened so we are forced to terminate */
        goto cleanup;
    }
    /*
     * Unknown command
     */
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line sent an unknown command (command %d)\n",
                             command));
        goto cleanup;
    }
    
 cleanup:
    return;
}

/**********************************
 * Job/Proc State Set/Get Routines
 **********************************/
static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     size_t ckpt_state, 
                                                     char  *ckpt_snapshot_ref,
                                                     char  *ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_job_t *jdata = NULL;
    opal_buffer_t buffer;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD;

    /*
     * Update locally - Global Coordinator structures
     */
    /* Get the job data object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Update Local structures */
    jdata->ckpt_state = ckpt_state;
    if( NULL != jdata->ckpt_snapshot_ref) {
        free(jdata->ckpt_snapshot_ref);
        jdata->ckpt_snapshot_ref = NULL;
    }
    jdata->ckpt_snapshot_ref = strdup(ckpt_snapshot_ref);
    if( NULL != jdata->ckpt_snapshot_loc) {
        free(jdata->ckpt_snapshot_loc);
        jdata->ckpt_snapshot_loc = NULL;
    }
    jdata->ckpt_snapshot_loc = strdup(ckpt_snapshot_loc);

    /*
     * Update all Local Coordinators (broadcast operation)
     */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_state, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_snapshot_ref, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_snapshot_loc, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notifying all Local Coordinators of job %s state change to %d\n",
                         ORTE_JOBID_PRINT(jobid), (int)ckpt_state));

    if( ORTE_SUCCESS != (ret = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, &buffer, ORTE_RML_TAG_SNAPC_FULL)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Process the job update - Global Coordinator
     */
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Act locally on job %s state change to %d\n",
                         ORTE_JOBID_PRINT(jobid), (int)ckpt_state));
    if( ORTE_SUCCESS != (ret = global_coord_job_state_update(jobid, ckpt_state, &ckpt_snapshot_ref, &ckpt_snapshot_loc) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);
    return exit_status;
}

static int orte_snapc_full_global_set_vpid_ckpt_info( orte_process_name_t proc,
                                                      size_t ckpt_state, 
                                                      char  *ckpt_snapshot_ref, 
                                                      char  *ckpt_snapshot_loc)
{
    int exit_status = ORTE_SUCCESS;
    orte_job_t *jdata = NULL;
    orte_proc_t **procs = NULL;

    /* get the job data object for this proc */
    if (NULL == (jdata = orte_get_job_data_object(proc.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    /* Get the proc object for this process */
    procs = (orte_proc_t**)jdata->procs->addr;
    if (NULL == procs[proc.vpid] ) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Update Local structures */
    procs[proc.vpid]->ckpt_state = ckpt_state;
    if( NULL != procs[proc.vpid]->ckpt_snapshot_ref) {
        free(procs[proc.vpid]->ckpt_snapshot_ref);
        procs[proc.vpid]->ckpt_snapshot_ref = NULL;
    }
    procs[proc.vpid]->ckpt_snapshot_ref = strdup(ckpt_snapshot_ref);
    if( NULL != procs[proc.vpid]->ckpt_snapshot_loc) {
        free(procs[proc.vpid]->ckpt_snapshot_loc);
        procs[proc.vpid]->ckpt_snapshot_loc = NULL;
    }
    procs[proc.vpid]->ckpt_snapshot_loc = strdup(ckpt_snapshot_loc);

 cleanup:
    return exit_status;
}

static int snapc_full_global_checkpoint(orte_jobid_t jobid,
                                        bool term,
                                        char **global_snapshot_handle,
                                        int *ckpt_status)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Checkpoint of job %s has been requested\n",
                         ORTE_JOBID_PRINT(jobid)));

    /*********************
     * Generate the global snapshot directory, and unique global snapshot handle
     *********************/
    INC_SEQ_NUM();
    *global_snapshot_handle = strdup( orte_snapc_base_unique_global_snapshot_name( getpid() ) );

    global_snapshot.seq_num = orte_snapc_base_snapshot_seq_number;
    global_snapshot.reference_name = strdup(*global_snapshot_handle);
    global_snapshot.local_location = opal_dirname(orte_snapc_base_get_global_snapshot_directory(global_snapshot.reference_name));

    /* Creates the directory (with metadata files):
     *   /tmp/ompi_global_snapshot_PID.ckpt/seq_num
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_init_global_snapshot_directory(*global_snapshot_handle, false))) {
        exit_status = ret;
        goto cleanup;
    }

    /***********************************
     * Do an update handshake with the orte_checkpoint command
     ***********************************/
    updated_job_to_running = false;
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                            global_snapshot.reference_name,
                                                                            global_snapshot.seq_num,
                                                                            ORTE_SNAPC_CKPT_STATE_REQUEST) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Using the checkpoint directory (%s)\n",
                         *global_snapshot_handle));

    /**********************
     * Notify the Local Snapshot Coordinators of the checkpoint request
     **********************/
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notifying the Local Coordinators\n"));

    if( ORTE_SUCCESS != (ret = snapc_full_global_notify_checkpoint(*global_snapshot_handle, 
                                                                   jobid, 
                                                                   term))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:

    *ckpt_status = exit_status;
    return exit_status; 
}

static int snapc_full_global_notify_checkpoint( char * global_snapshot_handle, 
                                                orte_jobid_t jobid, 
                                                bool term)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    char * global_dir = NULL;
    size_t ckpt_state = ORTE_SNAPC_CKPT_STATE_PENDING;

    global_dir = orte_snapc_base_get_global_snapshot_directory(global_snapshot_handle);

    if( term ) {
        ckpt_state = ORTE_SNAPC_CKPT_STATE_PENDING_TERM;
    }

    /*
     * By updating the job segment we tell the Local Coordinator to
     * checkpoint all their apps, so we don't need to do it explicitly here
     * Just update the global structure here...
     */
    for(item  = opal_list_get_first(&global_snapshot.snapshots);
        item != opal_list_get_end(&global_snapshot.snapshots);
        item  = opal_list_get_next(item) ) {
        orte_snapc_full_global_snapshot_t *vpid_snapshot;

        vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

        vpid_snapshot->super.state   = ckpt_state;
        vpid_snapshot->super.term    = term;

        if( NULL != vpid_snapshot->super.crs_snapshot_super.reference_name)
            free(vpid_snapshot->super.crs_snapshot_super.reference_name);
        vpid_snapshot->super.crs_snapshot_super.reference_name = opal_crs_base_unique_snapshot_name(vpid_snapshot->super.process_name.vpid);

        if( NULL != vpid_snapshot->super.crs_snapshot_super.local_location)
            free(vpid_snapshot->super.crs_snapshot_super.local_location);
        asprintf(&(vpid_snapshot->super.crs_snapshot_super.local_location), "%s/%s", global_dir, vpid_snapshot->super.crs_snapshot_super.reference_name);

        if( NULL != vpid_snapshot->super.crs_snapshot_super.remote_location)
            free(vpid_snapshot->super.crs_snapshot_super.remote_location);
        asprintf(&(vpid_snapshot->super.crs_snapshot_super.remote_location), "%s/%s", global_dir, vpid_snapshot->super.crs_snapshot_super.reference_name);

        /* Update the individual process information */
        if (ORTE_SUCCESS != (ret = orte_snapc_full_global_set_vpid_ckpt_info(vpid_snapshot->super.process_name,
                                                                             ORTE_SNAPC_CKPT_STATE_REQUEST,
                                                                             vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                             vpid_snapshot->super.crs_snapshot_super.local_location) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Update the job state, and broadcast to all local daemons
     */
    orte_snapc_base_global_snapshot_loc = strdup(global_dir);
    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(jobid,
                                                                        ckpt_state,
                                                                        global_snapshot_handle,
                                                                        global_dir) ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != global_dir)
        free(global_dir);

    return exit_status;
}

static int snapc_full_global_check_for_done(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;
    char * global_dir = NULL;

    /* If we are not done, then keep waiting */
    if(!snapc_full_global_is_done_yet()) {
        return exit_status;
    }

    /**********************************
     * Update the job checkpoint state
     **********************************/
    global_dir = orte_snapc_base_get_global_snapshot_directory(global_snapshot.reference_name);
    orte_snapc_base_global_snapshot_loc = strdup(global_dir);

    if( ORTE_SNAPC_CKPT_STATE_ERROR != cur_job_ckpt_state ) {
        cur_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_FILE_XFER;
    }

    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(jobid,
                                                                        cur_job_ckpt_state,
                                                                        global_snapshot.reference_name,
                                                                        global_dir) ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != global_dir) 
        free(global_dir);

    return exit_status;
}

static bool snapc_full_global_is_done_yet(void) {
    opal_list_item_t* item = NULL;
    /* Be optimistic, we are talking about Fault Tolerance */
    bool done_yet = true;
    
    for(item  = opal_list_get_first(&global_snapshot.snapshots);
        item != opal_list_get_end(&global_snapshot.snapshots);
        item  = opal_list_get_next(item) ) {
        orte_snapc_full_global_snapshot_t *vpid_snapshot;
        vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;
        
        /* If they are working, then we are not done yet */
        if(ORTE_SNAPC_CKPT_STATE_FINISHED != vpid_snapshot->super.state &&
           ORTE_SNAPC_CKPT_STATE_ERROR    != vpid_snapshot->super.state ) {
            done_yet = false;
            return done_yet;
        }
    }
    
    return done_yet;
}

static int snapc_full_global_gather_all_files(void) {
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    char * local_dir = NULL;
    orte_filem_base_request_t *filem_request = NULL;
    orte_filem_base_process_set_t *p_set = NULL;
    orte_filem_base_file_set_t * f_set = NULL;
    opal_list_t all_filem_requests;

    OBJ_CONSTRUCT(&all_filem_requests, opal_list_t);

    /*
     * If we just want to pretend to do the filem
     */
    if(orte_snapc_full_skip_filem) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }
    /*
     * If it is stored in place, then we do not need to transfer anything
     */
    else if( orte_snapc_base_store_in_place ) {
        for(item  = opal_list_get_first(&global_snapshot.snapshots);
            item != opal_list_get_end(&global_snapshot.snapshots);
            item  = opal_list_get_next(item) ) {
            orte_snapc_full_global_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global) Updating Metadata - Files stored in place, no transfer required:\n"));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   State:            %d\n",
                                 (int)vpid_snapshot->super.state));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Remote Location: [%s]\n",
                                 vpid_snapshot->super.crs_snapshot_super.remote_location));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Local Location:  [%s]\n",
                                 vpid_snapshot->super.crs_snapshot_super.local_location));

            if( ORTE_SNAPC_CKPT_STATE_ERROR == vpid_snapshot->super.state ) {
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            /*
             * Update the metadata file
             */
            if(ORTE_SUCCESS != (ret = orte_snapc_base_add_vpid_metadata(&vpid_snapshot->super.process_name,
                                                                        global_snapshot.reference_name,
                                                                        vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                        vpid_snapshot->super.crs_snapshot_super.local_location))) {
                exit_status = ret;
                goto cleanup;
            }
        }
    }
    /*
     * If *not* stored in place then use FileM to transfer the files and cleanup
     */
    else {

        /*
         * Construct a request for each file/directory to transfer
         *  - start the non-blocking transfer
         */
        for(item  = opal_list_get_first(&global_snapshot.snapshots);
            item != opal_list_get_end(&global_snapshot.snapshots);
            item  = opal_list_get_next(item) ) {
            orte_snapc_full_global_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global) Getting remote directory:\n"));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Status:          (%d)\n",
                                 (int)vpid_snapshot->super.state));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Remote Location: (%s)\n",
                                 vpid_snapshot->super.crs_snapshot_super.remote_location));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Local Location:  (%s)\n",
                                 vpid_snapshot->super.crs_snapshot_super.local_location));

            if( ORTE_SNAPC_CKPT_STATE_ERROR == vpid_snapshot->super.state ) {
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            filem_request = OBJ_NEW(orte_filem_base_request_t);

            /*
             * Construct the process set
             */
            p_set = OBJ_NEW(orte_filem_base_process_set_t);

            p_set->source.jobid = vpid_snapshot->local_coord.jobid;
            p_set->source.vpid  = vpid_snapshot->local_coord.vpid;
            p_set->sink.jobid   = ORTE_PROC_MY_NAME->jobid;
            p_set->sink.vpid    = ORTE_PROC_MY_NAME->vpid;

            opal_list_append(&(filem_request->process_sets), &(p_set->super) );

            /*
             * Construct the file set
             */
            f_set = OBJ_NEW(orte_filem_base_file_set_t);

            local_dir = strdup(vpid_snapshot->super.crs_snapshot_super.local_location);
            f_set->local_target  = opal_dirname(local_dir);
            f_set->remote_target = strdup(vpid_snapshot->super.crs_snapshot_super.remote_location);
            f_set->target_flag   = ORTE_FILEM_TYPE_DIR;

            opal_list_append(&(filem_request->file_sets), &(f_set->super) );

            /*
             * Start the transfer
             */
            opal_list_append(&all_filem_requests, &(filem_request->super));
            if(ORTE_SUCCESS != (ret = orte_filem.get_nb(filem_request) ) ) {
                opal_list_remove_item(&all_filem_requests, &(filem_request->super));
                OBJ_RELEASE(filem_request);
                filem_request = NULL;

                exit_status = ret;
                /* Keep getting all the other files, eventually return an error */
                continue;
            }
        }

        /*
         * Wait for all the transfers to complete
         */
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Getting remote directory: Waiting...\n"));
        if(ORTE_SUCCESS != (ret = orte_filem.wait_all(&all_filem_requests) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Now that the files have been brought local, remove the remote copy
         */
        for(item  = opal_list_get_first( &all_filem_requests);
            item != opal_list_get_end(   &all_filem_requests);
            item  = opal_list_get_next(   item) ) {
            filem_request = (orte_filem_base_request_t *) item;
            if(ORTE_SUCCESS != (ret = orte_filem.rm_nb(filem_request)) ) {
                exit_status = ret;
                /* Keep removing, eventually return an error */
                continue;
            }
        }

        /*
         * Update all of the metadata
         */
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Getting remote directory: Updating Metadata...\n"));
        for(item  = opal_list_get_first(&global_snapshot.snapshots);
            item != opal_list_get_end(&global_snapshot.snapshots);
            item  = opal_list_get_next(item) ) {
            orte_snapc_full_global_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_global_snapshot_t*)item;

            if(ORTE_SUCCESS != (ret = orte_snapc_base_add_vpid_metadata(&vpid_snapshot->super.process_name,
                                                                        global_snapshot.reference_name,
                                                                        vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                        vpid_snapshot->super.crs_snapshot_super.local_location))) {
                exit_status = ret;
                goto cleanup;
            }
        }

        /*
         * Wait for all the removes to complete
         */
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Waiting for removes to complete...\n"));
        if(ORTE_SUCCESS != (ret = orte_filem.wait_all(&all_filem_requests) ) ) {
            exit_status = ret;
            goto cleanup;
        }

    }

    /*
     * Now that we gathered all the files, finish off the metadata file
     */
    orte_snapc_base_finalize_metadata(global_snapshot.reference_name);
    
 cleanup:
    if(NULL != local_dir)
        free(local_dir);

    while (NULL != (item = opal_list_remove_first(&all_filem_requests) ) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&all_filem_requests);

    return exit_status;
}

static int snapc_full_get_vpid_range( orte_jobid_t jobid,
                                      orte_vpid_t *vpid_start,
                                      orte_vpid_t *vpid_range)
{
    int exit_status = ORTE_SUCCESS;
    orte_job_t *jdata;
                
    /* look up job data object */
    if (NULL == (jdata = orte_get_job_data_object(jobid))) {
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    *vpid_start = 0;
    *vpid_range = jdata->num_procs;
    
 cleanup:
    return exit_status;
}

