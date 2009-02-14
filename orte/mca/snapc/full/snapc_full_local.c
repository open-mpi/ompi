/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/runtime/opal_progress.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/basename.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/proc_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static bool snapc_local_recv_issued = false;
static bool snapc_local_proc_recv_issued = false;

static int snapc_full_local_start_listener(void);
static int snapc_full_local_stop_listener(void);
static int snapc_full_local_start_proc_listener(void);
static int snapc_full_local_stop_proc_listener(void);

static void snapc_full_local_cmd_recv(int status,
                                       orte_process_name_t* sender,
                                       opal_buffer_t* buffer,
                                       orte_rml_tag_t tag,
                                       void* cbdata);
static void snapc_full_local_process_job_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer);
static void snapc_full_local_process_app_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer);
static int snapc_full_local_send_vpid_assoc(void);

static int orte_snapc_full_local_set_vpid_ckpt_info( orte_process_name_t proc,
                                                     size_t ckpt_state, 
                                                     char  *ckpt_snapshot_ref, 
                                                     char  *ckpt_snapshot_loc);

static orte_snapc_full_local_snapshot_t *find_vpid_snapshot(orte_process_name_t *name );
static int snapc_full_local_get_vpids(void);

static int snapc_full_local_setup_snapshot_dir(char * snapshot_ref, char * sugg_dir, char **actual_dir);
static int snapc_full_establish_dir(void);

static int snapc_full_local_start_checkpoint_all(size_t ckpt_state);
static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static int snapc_full_local_start_ckpt_handshake_term(orte_snapc_full_local_snapshot_t *vpid_snapshot, bool term);
static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot);
static void snapc_full_local_comm_read_event(int fd, short flags, void *arg);

static opal_list_t snapc_local_vpids;
static orte_jobid_t snapc_local_jobid;
static size_t cur_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;

/************************
 * Function Definitions
 ************************/
int local_coord_init( void )
{
    snapc_local_jobid = -1;

    return ORTE_SUCCESS;
}

int local_coord_finalize( void )
{
    if( snapc_local_jobid >= 0 ) {
        return local_coord_release_job(snapc_local_jobid);
    }

    return ORTE_SUCCESS;
}

int local_coord_setup_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Set the jobid that we are responsible for
     */
    snapc_local_jobid = jobid;
    OBJ_CONSTRUCT(&snapc_local_vpids, opal_list_t);

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Monitor local jobid %s\n",
                         ORTE_JOBID_PRINT(snapc_local_jobid)));

    /*
     * Get the list of vpid's that we care about
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids()) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait for the snapshot directory to be established before registering
     * the callbacks since they use the same tag.
     */
    if(orte_snapc_base_establish_global_snapshot_dir) {
        if( ORTE_SUCCESS != (ret = snapc_full_establish_dir() ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Setup Global Coordinator listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_start_listener() ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup Global Coordinator listener for Application updates
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_start_proc_listener() ) ) {
        exit_status = ret;
        goto cleanup;
    }


    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Finished setting up job\n"));

 cleanup:
    return exit_status;
}

int local_coord_release_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t* item = NULL;
    bool is_done = true;

    /*
     * Wait around until we hear back from the checkpoint requests that
     * we have outstanding.
     */
    do {
        is_done = true;

        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            orte_snapc_full_local_snapshot_t *vpid_snapshot;
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;
            
            if(ORTE_SNAPC_CKPT_STATE_NONE     != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_ERROR    != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_FINISHED != vpid_snapshot->super.state ) {
                is_done = false;
                break;
            }
        }
        if( !is_done )
            opal_progress();
    } while(!is_done);

    OBJ_DESTRUCT(&snapc_local_vpids);

    /*
     * Stop Global Coordinator listeners
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_stop_proc_listener() ) ) {
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_local_stop_listener() ) ) {
        exit_status = ret;
    }

    return exit_status;
}

/******************
 * Local functions
 ******************/
static int snapc_full_local_start_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;

    /*
     * Global Coordinator: Do not register a Local listener
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        return ORTE_SUCCESS;
    }

    if (snapc_local_recv_issued ) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive: Start command recv"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_SNAPC_FULL,
                                                      ORTE_RML_PERSISTENT,
                                                      snapc_full_local_cmd_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_local_recv_issued = true;
    
 cleanup:
    return exit_status;
}

static int snapc_full_local_stop_listener(void)
{
    int rc, exit_status = ORTE_SUCCESS;

    /*
     * Global Coordinator: Does not register a Local listener
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        return ORTE_SUCCESS;
    }

    if (!snapc_local_recv_issued ) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive stop command recv"));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                   ORTE_RML_TAG_SNAPC_FULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_local_recv_issued = false;
    
 cleanup:
    return exit_status;
}

static int snapc_full_local_start_proc_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;

    if (snapc_local_proc_recv_issued) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive (Command line): Start command recv"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_SNAPC,
                                                      ORTE_RML_PERSISTENT,
                                                      snapc_full_local_cmd_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_local_proc_recv_issued = true;

 cleanup:
    return exit_status;
}

static int snapc_full_local_stop_proc_listener(void)
{
    int exit_status = ORTE_SUCCESS;
    int rc;
    
    if (!snapc_local_proc_recv_issued ) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive (Command Line) stop command"));
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                   ORTE_RML_TAG_SNAPC))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

    snapc_local_proc_recv_issued = false;
    
 cleanup:
    return exit_status;
}

void snapc_full_local_cmd_recv(int status,
                                orte_process_name_t* sender,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                void* cbdata)
{
    orte_snapc_full_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive a command message."));

    /*
     * This is the local process contacting us with its updated pid information
     */
    if( ORTE_RML_TAG_SNAPC == tag ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Application: Update pid operation"));
        snapc_full_local_process_app_update_cmd(sender, buffer);
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
                                 "Local) Command: Update Job state command"));

            snapc_full_local_process_job_update_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_FULL_UPDATE_PROC_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Command: Update Job state command"));
            /* Nothing to do */
            break;

        case ORTE_SNAPC_FULL_VPID_ASSOC_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Command: Update process/orted associations"));

            /* Nothing to do */
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
}

static void snapc_full_local_process_job_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_jobid_t jobid;
    size_t job_ckpt_state;
    char *job_ckpt_ref = NULL;
    char *job_ckpt_loc = NULL;
    orte_std_cntr_t count;

    /*
     * Unpack the data
     * - jobid
     * - ckpt_state
     * - ckpt_reference
     * - ckpt_location
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
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_ref, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_loc, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = local_coord_job_state_update(jobid, job_ckpt_state, &job_ckpt_ref, &job_ckpt_loc)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    return;
}

int local_coord_job_state_update(orte_jobid_t jobid,
                                 size_t job_ckpt_state,
                                 char **job_ckpt_ref,
                                 char **job_ckpt_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot;
    opal_list_item_t* item = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local) Job State:        %d\n",
                         (int)job_ckpt_state));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local) Snapshot Ref:    (%s)\n",
                         *job_ckpt_ref));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local) Remote Location: (%s)\n",
                         *job_ckpt_loc));

    /*
     * Update the vpid structure if we need to.
     * Really only need to if we don't have valid information (PID) 
     * for the application.
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids() ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If we have been asked to checkpoint do so
     */
    if( ORTE_SNAPC_CKPT_STATE_PENDING      == job_ckpt_state ||
        ORTE_SNAPC_CKPT_STATE_PENDING_TERM == job_ckpt_state ) {
        /*
         * For each of the processes we are tasked with, start their checkpoints
         */
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

            if( ORTE_SNAPC_CKPT_STATE_PENDING_TERM == job_ckpt_state ) {
                vpid_snapshot->super.term = true;
            }
            else {
                vpid_snapshot->super.term = false;
            }

            /*
             * Update it's local information
             */
            if( NULL != vpid_snapshot->super.crs_snapshot_super.reference_name ) { 
                free(vpid_snapshot->super.crs_snapshot_super.reference_name);
                vpid_snapshot->super.crs_snapshot_super.reference_name = NULL;
            }
            vpid_snapshot->super.crs_snapshot_super.reference_name = opal_crs_base_unique_snapshot_name(vpid_snapshot->super.process_name.vpid);
            
            /* global_directory/local_snapshot_vpid/... */
            if( NULL != vpid_snapshot->super.crs_snapshot_super.local_location ) {
                free(vpid_snapshot->super.crs_snapshot_super.local_location);
                vpid_snapshot->super.crs_snapshot_super.local_location = NULL;
            }

            if( orte_snapc_base_store_in_place ) {
                asprintf(&(vpid_snapshot->super.crs_snapshot_super.local_location), 
                         "%s/%s", 
                         *job_ckpt_loc,
                         vpid_snapshot->super.crs_snapshot_super.reference_name);
            }
            else {
                /* Use the OPAL CRS base snapshot dir
                 * JJH: Do we want to do something more interesting?
                 */
                asprintf(&(vpid_snapshot->super.crs_snapshot_super.local_location), 
                         "%s/%s",
                         opal_crs_base_snapshot_dir,
                         vpid_snapshot->super.crs_snapshot_super.reference_name);
            }
            
            if( NULL != vpid_snapshot->super.crs_snapshot_super.remote_location ) {
                free(vpid_snapshot->super.crs_snapshot_super.remote_location);
                vpid_snapshot->super.crs_snapshot_super.remote_location = NULL;
            }

            asprintf(&(vpid_snapshot->super.crs_snapshot_super.remote_location), 
                     "%s/%s", 
                     *job_ckpt_loc,
                     vpid_snapshot->super.crs_snapshot_super.reference_name);

            /*
             * Update the Global Coordinator
             */
            if( ORTE_SUCCESS != (ret = orte_snapc_full_local_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                                  job_ckpt_state,
                                                                                  vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                                  vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
                exit_status = ret;
                goto cleanup;
            }
        }

        cur_job_ckpt_state = job_ckpt_state;

        /*
         * Start checkpointing all local processes
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_start_checkpoint_all(job_ckpt_state) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_SNAPC_CKPT_STATE_FILE_XFER == job_ckpt_state ||
             ORTE_SNAPC_CKPT_STATE_FINISHED  == job_ckpt_state ) {

        if( cur_job_ckpt_state == ORTE_SNAPC_CKPT_STATE_FILE_XFER ) {
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Local) Already released processes on file transfer. Mark job as finished\n"));
            cur_job_ckpt_state = job_ckpt_state;
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }

        /*
         * Release all checkpointed processes now that the checkpoint is complete
         * If the request was to checkpoint then terminate this command will tell
         * the application to do so upon release.
         */
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

            OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                 "Local) Job Ckpt finished tell process %s\n",
                                 ORTE_NAME_PRINT(&vpid_snapshot->super.process_name)));

            if( ORTE_SUCCESS != (ret = snapc_full_local_end_ckpt_handshake(vpid_snapshot) ) ) {
                opal_output(mca_snapc_full_component.super.output_handle,
                            "Local) Error: Unable to finish the handshake with peer %s. %d\n", 
                            ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }
        }

        cur_job_ckpt_state = job_ckpt_state;
    } else {
        cur_job_ckpt_state = job_ckpt_state;
    }
    
 cleanup:
    return exit_status;
}

static void snapc_full_local_process_app_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;
    orte_snapc_cmd_flag_t command;
    orte_process_name_t proc;
    pid_t proc_pid = 0;
    orte_std_cntr_t count;

    /*
     * Verify the command
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SNAPC_LOCAL_UPDATE_CMD != command ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Warning: Expected an application command (%d) but received (%d)\n",
                             ORTE_SNAPC_LOCAL_UPDATE_CMD, command));
        return;
    }

    /*
     * Unpack the data
     * - process name
     * - PID
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc, &count, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc_pid, &count, OPAL_PID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( NULL == (vpid_snapshot = find_vpid_snapshot(&proc)) ) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* JJH: Maybe we should save the old and the newly restarted pid? */
    vpid_snapshot->super.process_pid = proc_pid;
    
 cleanup:
    return;
}

static int snapc_full_local_send_vpid_assoc(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
    opal_buffer_t buffer;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_VPID_ASSOC_CMD;
    size_t num_vpids = 0;

    /*
     * Global Coordinator: Operate locally
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        for(item  = opal_list_get_first(&snapc_local_vpids);
            item != opal_list_get_end(&snapc_local_vpids);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;
            global_coord_vpid_assoc_update(*ORTE_PROC_MY_NAME, vpid_snapshot->super.process_name);
        }
        return ORTE_SUCCESS;
    }

    /*
     * Local Coordinator: Send Global Coordinator the information
     */
    num_vpids = opal_list_get_size(&snapc_local_vpids);
    if( num_vpids <= 0 ) {
        return exit_status;
    }

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &num_vpids, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(vpid_snapshot->super.process_name), 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

static int snapc_full_establish_dir(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_ESTABLISH_DIR_CMD;
    opal_buffer_t buffer;
    char * ckpt_snapshot_ref = NULL;
    char * ckpt_snapshot_loc = NULL;
    orte_std_cntr_t count;

    /*
     * Global Coordinator: Operate locally
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        opal_output(0, "Error: Not supported!\n");
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Contact the HNP for global snapshot directory information to establish\n"));

    /* Notify HNP of request for information */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OBJ_DESTRUCT(&buffer);

    /* Wait for the HNP to release us */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Wait for response to global snapshot directory information request\n"));
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer(ORTE_PROC_MY_HNP,
                                                    &buffer,
                                                    ORTE_RML_TAG_SNAPC_FULL,
                                                    ORTE_RML_NON_PERSISTENT) ) ) {
        OBJ_DESTRUCT(&buffer);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Unpack the data
     * - command
     * - ckpt_reference
     * - ckpt_location
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &ckpt_snapshot_ref, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &ckpt_snapshot_loc, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( NULL != ckpt_snapshot_loc &&
        (0 != strncmp(ckpt_snapshot_loc, "", strlen(""))) ) {
        orte_snapc_base_global_snapshot_loc = strdup(ckpt_snapshot_loc);
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) The global snapshot directory has been established at [%s]\n",
                         orte_snapc_base_global_snapshot_loc));

 cleanup:
    OBJ_DESTRUCT(&buffer);
    if( NULL != ckpt_snapshot_ref ) {
        free(ckpt_snapshot_ref);
        ckpt_snapshot_ref = NULL;
    }
    if( NULL != ckpt_snapshot_loc ) {
        free(ckpt_snapshot_loc);
        ckpt_snapshot_loc = NULL;
    }

    return exit_status;
}

static int snapc_full_local_setup_snapshot_dir(char * snapshot_ref, char * sugg_dir, char **actual_dir)
{
    int ret, exit_status = ORTE_SUCCESS;
    mode_t my_mode = S_IRWXU;

    /* See if we can use the suggested directory */
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(sugg_dir, my_mode) ) ) {
        /* Can't use that directory, try the default directory from OPAL CRS */
        *actual_dir = strdup(opal_crs_base_get_snapshot_directory(snapshot_ref));

        if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(*actual_dir, my_mode) ) ) {
            /* Can't use that either, so let's give up */
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        /* We are able to use that directory */
        *actual_dir = strdup(sugg_dir);
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_get_vpids(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_item_t *item = NULL;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;
    orte_odls_child_t *child = NULL;
    size_t list_len = 0;
    bool new_child = false;

    /*
     * If the list is populated, and has updated pid information then
     * there is nothing to update.
     */
    list_len = opal_list_get_size(&snapc_local_vpids);
    if( list_len > 0 ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)opal_list_get_first(&snapc_local_vpids);
        if( 0 < vpid_snapshot->super.process_pid ) {
            return ORTE_SUCCESS;
        }
    }

    /*
     * Otherwise update or populate the list
     */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;

        /* if the list is empty or this child is not in the list then add it */
        if( 0    >= list_len ||
            NULL == (vpid_snapshot = find_vpid_snapshot(child->name)) ) {
            vpid_snapshot = OBJ_NEW(orte_snapc_full_local_snapshot_t);
            opal_list_append(&snapc_local_vpids, &(vpid_snapshot->super.crs_snapshot_super.super));
            new_child = true;
        }
        else {
            new_child = false;
        }

        vpid_snapshot->super.process_pid        = child->pid;
        vpid_snapshot->super.process_name.jobid = child->name->jobid;
        vpid_snapshot->super.process_name.vpid  = child->name->vpid;
    }

    /*
     * Send list to global coordinator
     */
    if( new_child ) {
        if( ORTE_SUCCESS != (ret = snapc_full_local_send_vpid_assoc() ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

static orte_snapc_full_local_snapshot_t *find_vpid_snapshot(orte_process_name_t *name )
{
    opal_list_item_t* item = NULL;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;

    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( name->jobid == vpid_snapshot->super.process_name.jobid &&
            name->vpid  == vpid_snapshot->super.process_name.vpid ) {
            return vpid_snapshot;
        }
    }

    return NULL;
}

static int orte_snapc_full_local_set_vpid_ckpt_info( orte_process_name_t proc,
                                                     size_t ckpt_state, 
                                                     char  *ckpt_snapshot_ref, 
                                                     char  *ckpt_snapshot_loc)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_UPDATE_PROC_STATE_CMD;

    /*
     * Global Coordinator: Operate locally
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        if( ORTE_SUCCESS != (ret = global_coord_vpid_state_update(proc, ckpt_state, &ckpt_snapshot_ref, &ckpt_snapshot_loc)) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local) Process %s: Changed state to:\n",
                         ORTE_NAME_PRINT(&proc)));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local)   State:            %d\n",
                         (int)ckpt_state));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local)   Snapshot Ref:    [%s]\n",
                         ckpt_snapshot_ref));
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local)   Location:        [%s]\n",
                         ckpt_snapshot_loc));

    /*
     * Local Coordinator: Send Global Coordinator the information
     */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc, 1, ORTE_NAME))) {
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

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

/************************
 * Start the checkpoint
 ************************/
static int snapc_full_local_start_checkpoint_all(size_t ckpt_state)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot;
    opal_list_item_t* item = NULL;
    char * actual_local_dir = NULL;
    bool ckpt_n_term = false;
    char *tmp_pid = NULL;

    /*
     * Cannot let opal-checkpoint be passed the --term flag
     * since the HNP needs to talk to the app to get
     * information for FileM. HNP will issue the termination.
     * JJH: Eventually release the contraint that the app needs to 
     *      be alive for FileM to properly work.
     *      However if we are storing in place, then we don't use
     *      the FileM framework and can terminate the application
     *      from this command.
     */
    if ( !orte_snapc_base_store_in_place ) {
        ckpt_n_term = false;
    }
    else if( ORTE_SNAPC_CKPT_STATE_PENDING_TERM == ckpt_state ) {
        ckpt_n_term = true;
    }
    else {
        ckpt_n_term = false;
    }

    /*
     * Pass 1: Setup snapshot directory
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        /*
         * Set up the snapshot directory per suggestion from 
         * the Global Snapshot Coordinator
         * If we can't create the suggested local directory, do what we can and update
         * local directory reference in the GPR
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_setup_snapshot_dir(vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                       vpid_snapshot->super.crs_snapshot_super.local_location,
                                                                       &actual_local_dir) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Local) Using directory (%s)\n",
                             vpid_snapshot->super.crs_snapshot_super.local_location));

        /* Dummy check */
        if( vpid_snapshot->super.process_pid == 0 ) {
            ret = snapc_full_local_get_vpids();
            if( ORTE_SUCCESS != ret || vpid_snapshot->super.process_pid == 0 ) {
                opal_output( mca_snapc_full_component.super.output_handle,
                             "local) Cannot checkpoint an invalid pid (%d)\n", 
                             vpid_snapshot->super.process_pid);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
    }

    /*
     * Pass 2: Start process of opening communication channels
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        /*
         * Create named pipe references for this process
         */
        if( NULL == vpid_snapshot->comm_pipe_w ||
            NULL == vpid_snapshot->comm_pipe_r ) {
            if( NULL != tmp_pid ) {
                free(tmp_pid);
                tmp_pid = NULL;
            }
            asprintf(&tmp_pid, "%d", vpid_snapshot->super.process_pid);
            asprintf(&(vpid_snapshot->comm_pipe_w), "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
            asprintf(&(vpid_snapshot->comm_pipe_r), "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);
        }

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Local) Signal process (%d) with signal %d\n",
                             (int) vpid_snapshot->super.process_pid,
                             opal_cr_entry_point_signal));

        /*
         * Signal the application
         */
        if( 0 != (ret = kill(vpid_snapshot->super.process_pid, opal_cr_entry_point_signal) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Failed to signal process %d with signal %d. %d\n", 
                        (int) vpid_snapshot->super.process_pid,
                        opal_cr_entry_point_signal,
                        ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Pass 3: Wait for channels to open up
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_open_comm(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }

        /*
         * Update so that folks know that we are working on it
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_full_local_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                             ORTE_SNAPC_CKPT_STATE_RUNNING,
                                                                             vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                             vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Pass 3: Start Handshake, send term argument
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake_term(vpid_snapshot, ckpt_n_term) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    /*
     * Pass 4: Start Handshake, send snapshot reference/location arguments
     */
    for(item  = opal_list_get_first(&snapc_local_vpids);
        item != opal_list_get_end(&snapc_local_vpids);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_local_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n", 
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != tmp_pid ) {
        free(tmp_pid);
        tmp_pid = NULL;
    }

    if( ORTE_SUCCESS != exit_status ) {
        ckpt_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    return exit_status;
}

static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int usleep_time = 1000;
    int s_time = 0, max_wait_time;

    max_wait_time = 20 * (1000000/usleep_time); /* wait time before giving up on the checkpoint */

    /*
     * Wait for the named pipes to be created
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Waiting for process %s's pipes (%s) (%s)\n",
                         ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                         vpid_snapshot->comm_pipe_w,
                         vpid_snapshot->comm_pipe_r));
    for( s_time = 0; s_time < max_wait_time; ++s_time) {
        /*
         * See if the named pipe exists yet for the PID in question
         */
        if( 0 > (ret = access(vpid_snapshot->comm_pipe_r, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 ) {
                OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                     "Local) File does not exist yet: <%s> rtn = %d (waited %d/%d usec)\n",
                                     vpid_snapshot->comm_pipe_r, ret, s_time, max_wait_time));
            }
            usleep(usleep_time);
            opal_event_loop(OPAL_EVLOOP_NONBLOCK);
            continue;
        }
        else if( 0 > (ret = access(vpid_snapshot->comm_pipe_w, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 ) {
                OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                     "Local) File does not exist yet: <%s> rtn = %d (waited %d/%d usec)\n",
                                     vpid_snapshot->comm_pipe_w, ret, s_time, max_wait_time));
            }
            usleep(usleep_time);
            opal_event_loop(OPAL_EVLOOP_NONBLOCK);
            continue;
        }
        else {
            break;
        }
    }
    if( s_time == max_wait_time ) { 
        /* The file doesn't exist, 
         * This means that the process didn't open up a named pipe for us
         * to access their checkpoint notification routine. Therefore,
         * the application either:
         *  - Doesn't exist
         *  - Isn't checkpointable
         * In either case there is nothing we can do.
         */
        orte_show_help("help-opal-checkpoint.txt", "pid_does_not_exist", true,
                       vpid_snapshot->super.process_pid,
                       vpid_snapshot->comm_pipe_r,
                       vpid_snapshot->comm_pipe_w);

        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Open Pipes...
     *  - prog_named_write_pipe:
     *    prog makes this file and opens Read Only
     *    this app. opens it Write Only
     *  - prog_named_read_pipe:
     *    prog makes this file and opens Write Only
     *    this app. opens it Read Only
     */
    vpid_snapshot->comm_pipe_w_fd = open(vpid_snapshot->comm_pipe_w, O_WRONLY);
    if(vpid_snapshot->comm_pipe_w_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to open name pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_w, vpid_snapshot->comm_pipe_w_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    vpid_snapshot->comm_pipe_r_fd = open(vpid_snapshot->comm_pipe_r, O_RDWR);
    if(vpid_snapshot->comm_pipe_r_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to open name pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_r, vpid_snapshot->comm_pipe_r_fd);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_start_ckpt_handshake_term(orte_snapc_full_local_snapshot_t *vpid_snapshot, bool term)
{
    int ret, exit_status = ORTE_SUCCESS;
    int term_rep;

    /*
     * Start the handshake: Send term argument
     */
    term_rep = (int)term;

    if( term ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Tell app to TERMINATE after completion of checkpoint. [%s (%d)]\n",
                             (term ? "True" : "False"), term_rep));
    }

    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &term_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write term (%d) to named pipe (%s), %d\n", 
                    term, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *local_dir = NULL;
    int len, value;
    ssize_t tmp_size = 0;

    /*
     * Wait for the appliation to respond
     */
    if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &value, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to read length from named pipe (%s). %d\n", 
                    vpid_snapshot->comm_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Check the response to make sure we can checkpoint this process */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:in_progress", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if( OPAL_CHECKPOINT_CMD_NULL == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_null", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if ( OPAL_CHECKPOINT_CMD_ERROR == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_error", 
                       true,
                       vpid_snapshot->super.process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_event_set(&(vpid_snapshot->comm_pipe_r_eh),
                   vpid_snapshot->comm_pipe_r_fd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   snapc_full_local_comm_read_event,
                   vpid_snapshot);
    vpid_snapshot->is_eh_active = true;
    opal_event_add(&(vpid_snapshot->comm_pipe_r_eh), NULL);

    /*
     * Send: Snapshot Name
     */
    len = strlen(vpid_snapshot->super.crs_snapshot_super.reference_name) + 1;
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot name len (%d) to named pipe (%s). %d\n", 
                    len, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(vpid_snapshot->comm_pipe_w_fd, (vpid_snapshot->super.crs_snapshot_super.reference_name), (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot name (%s) to named pipe (%s). %d\n", 
                    vpid_snapshot->super.crs_snapshot_super.reference_name, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Send: Snapshot Location
     */
    local_dir = strdup(vpid_snapshot->super.crs_snapshot_super.local_location);
    local_dir = opal_dirname(local_dir);
    len = strlen(local_dir) + 1;
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot location len (%d) to named pipe (%s). %d\n", 
                    len, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = write(vpid_snapshot->comm_pipe_w_fd, (local_dir), (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write snapshot location (%s) to named pipe (%s). %d\n", 
                    local_dir, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != local_dir ) {
        free(local_dir);
        local_dir = NULL;
    }

    return exit_status;
}

static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_local_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;

    /*
     * Make sure the pipe is open, so we do not try to do this twice
     */
    if( 0 > vpid_snapshot->comm_pipe_w_fd ) {
        return exit_status;
    }

    if( vpid_snapshot->super.term ) {
        last_cmd = 999;
    } else {
        last_cmd = 0;
    }

    /*
     * Finish the handshake.
     */
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to release process %s (%d)\n", 
                    ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    /*
     * Close all pipes
     */
    close(vpid_snapshot->comm_pipe_w_fd);
    close(vpid_snapshot->comm_pipe_r_fd);
    vpid_snapshot->comm_pipe_w_fd = -1;
    vpid_snapshot->comm_pipe_r_fd = -1;

    return exit_status;
}

static void snapc_full_local_comm_read_event(int fd, short flags, void *arg)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_local_snapshot_t *vpid_snapshot = NULL;
    size_t loc_state = ORTE_SNAPC_CKPT_STATE_FINISHED;
    int ckpt_state;

    vpid_snapshot = (orte_snapc_full_local_snapshot_t *)arg;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Read Event: Process %s done...\n",
                         ORTE_NAME_PRINT(&vpid_snapshot->super.process_name)));

    /*
     * Get the final state of the checkpoint from the checkpointing process
     */
    if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &ckpt_state, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to read state from named pipe (%s). %d\n",
                    vpid_snapshot->comm_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    if( ckpt_state == OPAL_CRS_ERROR ) {
        loc_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    /*
     * Now that the checkpoint is finished
     * Update our status information
     */
    vpid_snapshot->super.state = loc_state;
    if( ORTE_SUCCESS != (ret = orte_snapc_full_local_set_vpid_ckpt_info( vpid_snapshot->super.process_name,
                                                                         loc_state,
                                                                         vpid_snapshot->super.crs_snapshot_super.reference_name,
                                                                         vpid_snapshot->super.crs_snapshot_super.local_location ) ) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    /*
     * Disable events
     */
    opal_event_del(&(vpid_snapshot->comm_pipe_r_eh));
    vpid_snapshot->is_eh_active = false;

    return;
}
