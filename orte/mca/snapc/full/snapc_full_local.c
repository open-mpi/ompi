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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/runtime/opal_progress.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/basename.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static orte_jobid_t current_local_jobid = ORTE_JOBID_INVALID;
static orte_snapc_base_global_snapshot_t local_global_snapshot;

static int current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;

static bool currently_migrating = false;
static bool flushed_modex = false;
static bool sstore_local_sync_finished = false;
static bool sstore_local_procs_finished = false;

static int local_define_pipe_names(orte_snapc_full_app_snapshot_t *vpid_snapshot);

static bool snapc_local_hnp_recv_issued = false;
static int  snapc_full_local_start_hnp_listener(void);
static int  snapc_full_local_stop_hnp_listener(void);
static void snapc_full_local_hnp_cmd_recv(int status,
                                          orte_process_name_t* sender,
                                          opal_buffer_t* buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata);

static bool snapc_local_app_recv_issued = false;
static int  snapc_full_local_start_app_listener(void);
static int  snapc_full_local_stop_app_listener(void);
static void snapc_full_local_app_cmd_recv(int status,
                                       orte_process_name_t* sender,
                                       opal_buffer_t* buffer,
                                       orte_rml_tag_t tag,
                                       void* cbdata);

static orte_snapc_full_app_snapshot_t *find_vpid_snapshot(orte_process_name_t *name );
static int snapc_full_local_get_vpids(void);
static int snapc_full_local_refresh_vpids(void);

#if OPAL_ENABLE_CRDEBUG == 1
static int snapc_full_local_send_restart_proc_info(void);
#endif

static void snapc_full_local_process_job_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer,
                                                    bool quick);

static int local_coord_job_state_update_finished_local(void);
static int local_coord_job_state_update_finished_local_vpid(orte_snapc_full_app_snapshot_t *vpid_snapshot);

#if 0
static int snapc_full_establish_dir(void);
#endif
static int snapc_full_get_min_state(void);

static int snapc_full_local_update_coord(int state, bool quick);

static int snapc_full_local_start_checkpoint_all(int ckpt_state,
                                                 opal_crs_base_ckpt_options_t *options);
static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_app_snapshot_t *vpid_snapshot);
static int snapc_full_local_start_ckpt_handshake_opts(orte_snapc_full_app_snapshot_t *vpid_snapshot,
                                                      opal_crs_base_ckpt_options_t *options);
static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_app_snapshot_t *vpid_snapshot);
static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_app_snapshot_t *vpid_snapshot);
static void snapc_full_local_comm_read_event(int fd, short flags, void *arg);

static int orte_snapc_full_local_reset_coord(void);

/************************
 * Function Definitions
 ************************/
int local_coord_init( void )
{
    current_local_jobid = ORTE_JOBID_INVALID;
    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;

    return ORTE_SUCCESS;
}

int local_coord_finalize( void )
{
    if( ORTE_JOBID_INVALID != current_local_jobid ) {
        return local_coord_release_job(current_local_jobid);
    }

    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;

    return ORTE_SUCCESS;
}

int local_coord_setup_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;

    /*
     * Set the jobid that we are responsible for
     */
    if( jobid == current_local_jobid ) {
        /* If we pass this way twice, we must be restarting.
         * so just refresh the vpid structure
         */
        if( currently_migrating ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Restarting Job %s from Migration...",
                                 ORTE_JOBID_PRINT(jobid)));
            if( ORTE_SUCCESS != (ret = snapc_full_local_refresh_vpids() ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
        else {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Restarting Job %s...",
                                 ORTE_JOBID_PRINT(jobid)));

            for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
                item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
                item  = opal_list_get_next(item) ) {
                vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;
                opal_list_remove_item(&(local_global_snapshot.local_snapshots), item);
            }

            if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids() ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
                item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
                item  = opal_list_get_next(item) ) {
                vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;
                OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                     "Local) Restarting Job %s: Daemon %s \t Process %s",
                                     ORTE_JOBID_PRINT(jobid),
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&vpid_snapshot->super.process_name)));
            }
        }

        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }
    else if( ORTE_JOBID_INVALID != current_local_jobid ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Setup of job %s Failed! Already setup job %s\n",
                    ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(current_local_jobid));
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    current_local_jobid = jobid;
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Setting up jobid %s\n",
                         ORTE_JOBID_PRINT(current_local_jobid)));

    /*
     * Get the list of vpid's that we care about
     */
    OBJ_CONSTRUCT(&local_global_snapshot, orte_snapc_base_global_snapshot_t);

    if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait for the snapshot directory to be established before registering
     * the callbacks since they use the same tag.
     */
#if 0
    if(orte_snapc_base_establish_global_snapshot_dir) {
        if( ORTE_SUCCESS != (ret = snapc_full_establish_dir() ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
#endif

    /*
     * Setup Global Coordinator listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_start_hnp_listener() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup Global Coordinator listener for Application updates
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_start_app_listener() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }


    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Finished setup of job %s",
                         ORTE_JOBID_PRINT(current_local_jobid) ));

 cleanup:
    return exit_status;
}

int local_coord_release_job(orte_jobid_t jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot;
    opal_list_item_t* item = NULL;
    bool is_done = true;

    /*
     * Wait around until we hear back from the checkpoint requests that
     * we have outstanding.
     */
    do {
        is_done = true;

        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

            if(ORTE_SNAPC_CKPT_STATE_NONE      != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_ERROR     != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_ESTABLISHED  != vpid_snapshot->super.state &&
               ORTE_SNAPC_CKPT_STATE_RECOVERED != vpid_snapshot->super.state ) {
                is_done = false;
                break;
            }
            else {
                opal_list_remove_item(&(local_global_snapshot.local_snapshots), item);
            }
        }
        if( !is_done ) {
            opal_progress();
        }
    } while(!is_done);

    OBJ_DESTRUCT(&local_global_snapshot);

    /*
     * Stop Global Coordinator listeners
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_stop_app_listener() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_local_stop_hnp_listener() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    return exit_status;
}

/******************
 * Local functions
 ******************/

/******************
 * Setup Listeners
 ******************/
static int snapc_full_local_start_hnp_listener(void)
{
    /*
     * Global Coordinator: Do not register a Local listener
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        return ORTE_SUCCESS;
    }

    if (snapc_local_hnp_recv_issued ) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Startup Coordinator Channel"));

    /*
     * Coordinator command listener
     */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC_FULL,
                            ORTE_RML_PERSISTENT, snapc_full_local_hnp_cmd_recv, NULL);

    snapc_local_hnp_recv_issued = true;

    return ORTE_SUCCESS;
}

static int snapc_full_local_stop_hnp_listener(void)
{
    /*
     * Global Coordinator: Does not register a Local listener
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        return ORTE_SUCCESS;
    }

    if (!snapc_local_hnp_recv_issued ) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Shutdown Coordinator Channel"));

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC_FULL);

    snapc_local_hnp_recv_issued = false;
    return ORTE_SUCCESS;
}

static int snapc_full_local_start_app_listener(void)
{
    if (snapc_local_app_recv_issued) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Startup Application State Channel"));

    /*
     * Coordinator command listener
     */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC,
                            ORTE_RML_PERSISTENT, snapc_full_local_app_cmd_recv,
                            NULL);

    snapc_local_app_recv_issued = true;
    return ORTE_SUCCESS;
}

static int snapc_full_local_stop_app_listener(void)
{
    if (!snapc_local_app_recv_issued ) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Shutdown Application State Channel"));

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SNAPC);

    snapc_local_app_recv_issued = false;
    return ORTE_SUCCESS;
}

/******************
 * Listener Handlers
 ******************/
void snapc_full_local_app_cmd_recv(int status,
                                   orte_process_name_t* sender,
                                   opal_buffer_t* buffer,
                                   orte_rml_tag_t tag,
                                   void* cbdata)
{
    int ret;
    opal_list_item_t* item = NULL;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    orte_snapc_cmd_flag_t command;
    orte_process_name_t proc;
    pid_t proc_pid = 0;
    orte_std_cntr_t count;
    int cr_state;
    bool is_done;
#if OPAL_ENABLE_CRDEBUG == 1
    bool all_done = false;
    bool crdebug_enabled = false;
#endif

    if( ORTE_RML_TAG_SNAPC != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    /*
     * Verify the command
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    if( ORTE_SNAPC_LOCAL_UPDATE_CMD != command &&
        ORTE_SNAPC_LOCAL_FINISH_CMD != command ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Warning: Expected an application command (%d) but received (%d)\n",
                             ORTE_SNAPC_LOCAL_UPDATE_CMD, command));
        goto cleanup;
    }

    if( ORTE_SNAPC_LOCAL_UPDATE_CMD == command ) {
        /*
         * This is the local process contacting us with its updated pid information
         */
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Application: Update pid operation."));

        /*
         * Unpack the data
         * - process name
         * - PID
         */
        count = 1;
        /* JJH CLEANUP: Do we really need this, it is equal to sender */
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc, &count, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc_pid, &count, OPAL_PID))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
#if OPAL_ENABLE_CRDEBUG == 1
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &crdebug_enabled, &count, OPAL_BOOL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
#endif

        if( NULL == (vpid_snapshot = find_vpid_snapshot(&proc)) ) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Updated PID: %s : %d -> %d",
                             ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), vpid_snapshot->process_pid, proc_pid));

        /* JJH: Maybe we should save the old and the newly restarted pid? */
        vpid_snapshot->process_pid = proc_pid;
        vpid_snapshot->finished = true;

#if OPAL_ENABLE_CRDEBUG == 1
        /*
         * Once we have received all updates we should send them to the Global coord
         */
        if( crdebug_enabled ) {
            all_done = true;
            for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
                item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
                item  = opal_list_get_next(item) ) {
                vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;
                if( !vpid_snapshot->finished ) {
                    all_done = false;
                    break;
                }
            }
            if( all_done ) {
                /* If C/R Debugging then send hostlist */
                if( ORTE_SUCCESS != (ret = snapc_full_local_send_restart_proc_info() ) ) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
            }
        }
#endif

        /* Note: We should not update the ORTE structure since, if the CRS uses
         * an intermediary restart mechanism (e.g., BLCR's cr_restart) that
         * forks a child, then this process cannot call waitpit() on it.
         */
    }
    else if( ORTE_SNAPC_LOCAL_FINISH_CMD == command ) {
        /*
         * Unpack the data
         * - cr_state
         */
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &cr_state, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if( NULL == (vpid_snapshot = find_vpid_snapshot(sender)) ) {
            opal_output(0, "Local) Failed to find process %s",
                        ORTE_NAME_PRINT(sender));
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Process %s Finished Recovery (%d)",
                             ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                             cr_state));

        vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_RECOVERED;

        /*
         * Check if we are done
         */
        is_done = true;
        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

            if( ORTE_SNAPC_CKPT_STATE_RECOVERED != vpid_snapshot->super.state ) {
                is_done = false;
                break;
            }
        }

        if( is_done ) {
            /*
             * Tell the Global Coordinator that all of our processes are finished
             */
            OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                 "Local) Job Ckpt finished - Confirmed! Tell the Global Coord\n"));

            if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(ORTE_SNAPC_CKPT_STATE_RECOVERED, true) ) ) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            /*
             * If we are not finished sync'ing then delay cleanup
             */
            if( !sstore_local_sync_finished ) {
                sstore_local_procs_finished = true;
                OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                     "Local) Job Ckpt finished - Confirmed! Not finished Syncing...\n"));
            } else {
                /*
                 * Cleanup
                 */
                if( ORTE_SUCCESS != (ret = orte_snapc_full_local_reset_coord()) ) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
            }
        }
    }

 cleanup:
    return;
}

#if OPAL_ENABLE_CRDEBUG == 1
static int snapc_full_local_send_restart_proc_info(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
    opal_buffer_t *buffer = NULL;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_RESTART_PROC_INFO;
    size_t num_vpids = 0;

    /*
     * Global Coordinator: Operate locally
     */
    if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;
            global_coord_restart_proc_info(vpid_snapshot->process_pid, orte_process_info.nodename);
        }
        /* stdout may be buffered by the C library so it needs to be flushed so
         * that the debugger can read the process info.
         */
        fflush(stdout);
        return ORTE_SUCCESS;
    }

    buffer = OBJ_NEW(opal_buffer_t);
    /*
     * Local Coordinator: Send Global Coordinator the information
     * [ hostname, num_pids, {pids} ]
     */
    num_vpids = opal_list_get_size(&(local_global_snapshot.local_snapshots));
    if( num_vpids <= 0 ) {
        return exit_status;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(orte_process_info.nodename), 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &num_vpids, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(vpid_snapshot->process_pid), 1, OPAL_PID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

    }

    if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buffer, ORTE_RML_TAG_SNAPC_FULL,
                                                       orte_rml_send_callback, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    return ORTE_SUCCESS;

 cleanup:
    OBJ_RELEASE(buffer);

    return exit_status;
}
#endif

void snapc_full_local_hnp_cmd_recv(int status,
                                   orte_process_name_t* sender,
                                   opal_buffer_t* buffer,
                                   orte_rml_tag_t tag,
                                   void* cbdata)
{
    int ret;
    orte_snapc_full_cmd_flag_t command;
    orte_std_cntr_t count;

    if( ORTE_RML_TAG_SNAPC_FULL != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    /*
     * This is a Global Coordinator message.
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) Receive a command message."));

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    switch (command) {
        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Command: Update Job state command (quick)"));

            snapc_full_local_process_job_update_cmd(sender, buffer, true);
            break;

        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Command: Update Job state command"));

            snapc_full_local_process_job_update_cmd(sender, buffer, false);
            break;

        case ORTE_SNAPC_FULL_RESTART_PROC_INFO:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Command: Update hostname/pid associations"));
            /* Nothing to do */
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }

 cleanup:
    return;
}

static void snapc_full_local_process_job_update_cmd(orte_process_name_t* sender,
                                                    opal_buffer_t* buffer,
                                                    bool quick)
{
    int ret;
    orte_jobid_t jobid;
    int job_ckpt_state;
    orte_std_cntr_t count;
    opal_crs_base_ckpt_options_t *options = NULL;
    bool loc_migrating = false;
    size_t loc_num_procs = 0;
    orte_process_name_t proc_name;
    size_t i;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
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
            goto cleanup;
        }
        /* In this case we want to use the current_local_options that are cached
         * so that we do not have to send them every time.
         */
        opal_crs_base_copy_options(options, local_global_snapshot.options);

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(loc_migrating), &count, OPAL_BOOL))) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if( loc_migrating ) {
            currently_migrating = true;

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &loc_num_procs, &count, OPAL_SIZE))) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }

            for( i = 0; i < loc_num_procs; ++i ) {
                count = 1;
                if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &proc_name, &count, ORTE_NAME))) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }

                /*
                 * See if we are watching this process
                 */
                for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
                    item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
                    item  = opal_list_get_next(item) ) {
                    vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

                    if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                                    &(vpid_snapshot->super.process_name),
                                                                    &proc_name) ) {
                        vpid_snapshot->migrating = true;
                        break;
                    }
                }
            }
        }
    }

    if( ORTE_SUCCESS != (ret = local_coord_job_state_update(jobid,
                                                            job_ckpt_state,
                                                            ss_handle,
                                                            local_global_snapshot.options)) ) {
        ORTE_ERROR_LOG(ret);
        return;
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}


int local_coord_job_state_update(orte_jobid_t jobid,
                                 int    job_ckpt_state,
                                 orte_sstore_base_handle_t ss_handle,
                                 opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
    char * state_str = NULL;

    /* Save Options */
    opal_crs_base_copy_options(options, local_global_snapshot.options);

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local) Job %s: Changed to state to:\n",
                         ORTE_JOBID_PRINT(jobid)));
    orte_snapc_ckpt_state_str(&state_str, job_ckpt_state);
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Local)    Job State:        %d (%s)\n",
                         (int)job_ckpt_state, state_str ));
    free(state_str);
    state_str = NULL;

    /*
     * Update the vpid structure if we need to.
     * Really only need to if we don't have valid information (PID)
     * for the application.
     */
    if( ORTE_SUCCESS != (ret = snapc_full_local_get_vpids() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    current_job_ckpt_state = job_ckpt_state;

    /*
     * If we have been asked to checkpoint do so
     */
    if( ORTE_SNAPC_CKPT_STATE_PENDING      == job_ckpt_state ) {
        /*
         * Register with the SStore
         */
        local_global_snapshot.ss_handle = ss_handle;
        orte_sstore.register_handle(local_global_snapshot.ss_handle);

        /*
         * For each of the processes we are tasked with, start their checkpoints
         */
        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

            vpid_snapshot->super.state = job_ckpt_state;
            vpid_snapshot->finished = false;
        }

        /*
         * Start checkpointing all local processes
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_start_checkpoint_all(job_ckpt_state, options) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_SNAPC_CKPT_STATE_MIGRATING  == job_ckpt_state ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Migrating: Display a list of processes migrating"));

        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;
            /*
             * If this process migrated away, then remove it from our list.
             */
            if( vpid_snapshot->migrating ) {
                OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                     "Local) Migrating:    %s",
                                     ORTE_NAME_PRINT(&vpid_snapshot->super.process_name) ));
            }
        }
    }
    /*
     * Release all checkpointed processes now that the checkpoint is complete
     * If the request was to checkpoint then terminate this command will tell
     * the application to do so upon release.
     */
    else if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL  == job_ckpt_state ) {
        OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                             "Local) Locally finished, release all processes\n"));
        if( ORTE_SUCCESS != (ret = local_coord_job_state_update_finished_local() ) ) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }
    /*
     * Once we get the FINISHED state then the checkpoint is all done, and we
     * reset our state to NONE.
     */
    else if( ORTE_SNAPC_CKPT_STATE_ESTABLISHED  == job_ckpt_state ) {
        /*
         * Wait to cleanup until all have reported
         */
    }
    else {
        ;
    }

 cleanup:
    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    return exit_status;
}

static int local_coord_job_state_update_finished_local(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Local) Job Ckpt finished tell all processes\n"));

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = local_coord_job_state_update_finished_local_vpid(vpid_snapshot) ) ) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        if( vpid_snapshot->migrating ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Removing Migrated Process:    %s",
                                 ORTE_NAME_PRINT(&vpid_snapshot->super.process_name) ));
            opal_list_remove_item(&(local_global_snapshot.local_snapshots), item);
        }
    }
 cleanup:
    return exit_status;
}

static int local_coord_job_state_update_finished_local_vpid(orte_snapc_full_app_snapshot_t *vpid_snapshot)
{
    int ret;

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Local)   Tell process %s (Ckpt Finished) %s\n",
                         ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                         (vpid_snapshot->migrating ? "- Migrating, Skip" : "") ));

    /*
     * If this process is migrating, it has already been told
     */
    if( vpid_snapshot->migrating ) {
        return ORTE_SUCCESS;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_local_end_ckpt_handshake(vpid_snapshot) ) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to finish the handshake with peer %s. %d\n",
                    ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}


/************************
 * Start the checkpoint
 ************************/
static int snapc_full_local_start_checkpoint_all(int ckpt_state,
                                                 opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot;
    opal_list_item_t* item = NULL;
    size_t num_stopped = 0;
    int waitpid_status = 0;

    /*
     * Pass 1: make sure all vpids are setup correctly
     * This is a sanity check. Most of the time it will not be necessary.
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 1: Sanity check"));

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        /* Dummy check */
        if( vpid_snapshot->process_pid == 0 ) {
            ret = snapc_full_local_get_vpids();
            if( ORTE_SUCCESS != ret || vpid_snapshot->process_pid == 0 ) {
                opal_output( mca_snapc_full_component.super.output_handle,
                             "local) Cannot checkpoint an invalid pid (%d)\n",
                             vpid_snapshot->process_pid);
                ORTE_ERROR_LOG(ORTE_ERROR);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
            break;
        }
    }

    /*
     * Pass 2: Start process of opening communication channels
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 2: Signal Procs"));
    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        /*
         * Create named pipe references for this process
         */
        local_define_pipe_names(vpid_snapshot);

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Local) Signal process (%d) with signal %d\n",
                             (int) vpid_snapshot->process_pid,
                             opal_cr_entry_point_signal));

        /*
         * Signal the application
         */
        if( 0 != (ret = kill(vpid_snapshot->process_pid, opal_cr_entry_point_signal) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Failed to signal process %d with signal %d. %d\n",
                        (int) vpid_snapshot->process_pid,
                        opal_cr_entry_point_signal,
                        ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Pass 3: Wait for channels to open up
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 3: Open pipes"));
    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_open_comm(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n",
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_RUNNING;
    }

    /*
     * Pass 4: Start Handshake, send option argument set and sstore handle
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 4: Start handshake"));
    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake_opts(vpid_snapshot, options) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n",
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * Pass 5: Start checkpoint
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 5: Start checkpoints"));
    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if( ORTE_SUCCESS != (ret = snapc_full_local_start_ckpt_handshake(vpid_snapshot) ) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "local) Error: Unable to initiate the handshake with peer %s. %d\n",
                        ORTE_NAME_PRINT(&vpid_snapshot->super.process_name), ret);
            ORTE_ERROR_LOG(ORTE_ERROR);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * Progress Update to Global Coordinator
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Local) start() Pass 6: Tell Global Coord that we are running now"));
    if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(ORTE_SNAPC_CKPT_STATE_RUNNING, true) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If stopping then wait for all processes to stop
     */
    if( options->stop ) {
        while( num_stopped < opal_list_get_size(&(local_global_snapshot.local_snapshots)) ) {
            opal_progress();
            sleep(1);

            for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
                item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
                item  = opal_list_get_next(item) ) {
                vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

                ret = waitpid(vpid_snapshot->process_pid, &waitpid_status, WNOHANG|WUNTRACED);

                if( (ret > 0) && WIFSTOPPED(waitpid_status) && (SIGSTOP == WSTOPSIG(waitpid_status)) ) {
                    ++num_stopped;
                    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                         "Local) Child (%d) is stopped [total = %d]",
                                         vpid_snapshot->process_pid, (int)num_stopped ));
                }
                else if( ret < 0 ) {
                    if( 0 < mca_snapc_full_component.super.verbose ) {
                        orte_show_help("help-orte-snapc-full.txt", "waitpid_stop_fail", true,
                                       vpid_snapshot->process_pid, ret,
                                       ORTE_NAME_PRINT(&vpid_snapshot->super.process_name));
                    }
                    goto skip_wait;
                }
            }
        }

    skip_wait:
        for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
            item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

            vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_STOPPED;
        }

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "Local) All Children have now been stopped [total = %d]",
                             (int)num_stopped ));

        /*
         * Finish the local snapshot
         */
        orte_sstore.sync(local_global_snapshot.ss_handle);

        /*
         * Progress Update to Global Coordinator
         */
        if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(ORTE_SNAPC_CKPT_STATE_STOPPED, false) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    if( ORTE_SUCCESS != exit_status ) {
        ckpt_state = ORTE_SNAPC_CKPT_STATE_ERROR;
    }

    return exit_status;
}

static int local_define_pipe_names(orte_snapc_full_app_snapshot_t *vpid_snapshot)
{
    if( NULL != vpid_snapshot->comm_pipe_r ) {
        free(vpid_snapshot->comm_pipe_r);
        vpid_snapshot->comm_pipe_r = NULL;
    }

    if( NULL != vpid_snapshot->comm_pipe_w ) {
        free(vpid_snapshot->comm_pipe_w);
        vpid_snapshot->comm_pipe_w = NULL;
    }

    asprintf(&(vpid_snapshot->comm_pipe_w),
             "%s/%s.%d_%d",
             opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R,
             vpid_snapshot->process_pid,
             vpid_snapshot->unique_pipe_id);

    asprintf(&(vpid_snapshot->comm_pipe_r),
             "%s/%s.%d_%d",
             opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W,
             vpid_snapshot->process_pid,
             vpid_snapshot->unique_pipe_id);

    (vpid_snapshot->unique_pipe_id)++;

    return ORTE_SUCCESS;
}

static int snapc_full_local_update_coord(int state, bool quick)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t *buffer = NULL;
    orte_snapc_full_cmd_flag_t command;

    /*
     * Local Coordinator: Send Global Coordinator state information
     */
    buffer = OBJ_NEW(opal_buffer_t);

    if( quick ) {
        command = ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_QUICK_CMD;
    } else {
        command = ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_CMD;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &state, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* Optionally send only an abbreviated message to improve scalability */
    /* JJH: Though there is currently no additional information sent in a long
     *      message versus a small message, keep this logic so that in the
     *      future it can be easily reused without substantially modifying
     *      the component.
     */
    if( quick ) {
        goto send_data;
    }

 send_data:
    if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buffer,
                                                       ORTE_RML_TAG_SNAPC_FULL,
                                                       orte_rml_send_callback, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    return ORTE_SUCCESS;

 cleanup:
    OBJ_RELEASE(buffer);

    return exit_status;
}

static int snapc_full_local_start_ckpt_open_comm(orte_snapc_full_app_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int usleep_time = 1000;
    int s_time = 0, max_wait_time;

    /* wait time before giving up on the checkpoint */
    max_wait_time = orte_snapc_full_max_wait_time * (1000000/usleep_time);

    /*
     * Wait for the named pipes to be created
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Waiting for process %s's pipes (%s) (%s)\n",
                         ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                         vpid_snapshot->comm_pipe_w,
                         vpid_snapshot->comm_pipe_r));
    for( s_time = 0; s_time < max_wait_time || max_wait_time <= 0; ++s_time) {
        /*
         * See if the named pipe exists yet for the PID in question
         */
        if( 0 > (ret = access(vpid_snapshot->comm_pipe_r, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 && max_wait_time > 0 ) {
                OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                     "Local) WARNING: Read file does not exist yet: <%s> rtn = %d (waited %d/%d sec)\n",
                                     vpid_snapshot->comm_pipe_r, ret,
                                     s_time/usleep_time, max_wait_time/usleep_time));
            }
            usleep(usleep_time);
            continue;
        }
        else if( 0 > (ret = access(vpid_snapshot->comm_pipe_w, F_OK) )) {
            /* File doesn't exist yet, keep waiting */
            if( s_time >= max_wait_time - 5 && max_wait_time > 0 ) {
                OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                                     "Local) WARNING: Write file does not exist yet: <%s> rtn = %d (waited %d/%d sec)\n",
                                     vpid_snapshot->comm_pipe_w, ret,
                                     s_time/usleep_time, max_wait_time/usleep_time));
            }
            usleep(usleep_time);
            continue;
        }
        else {
            break;
        }

        if( max_wait_time > 0 &&
            (s_time == (max_wait_time/2) ||
             s_time == (max_wait_time/4) ||
             s_time == (3*max_wait_time/4) ) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "WARNING: Pid (%d) not responding [%d / %d]",
                                 vpid_snapshot->process_pid, s_time, max_wait_time));
        }
    }

    if( max_wait_time > 0 && s_time == max_wait_time ) {
        /* The file doesn't exist,
         * This means that the process didn't open up a named pipe for us
         * to access their checkpoint notification routine. Therefore,
         * the application either:
         *  - Doesn't exist
         *  - Isn't checkpointable
         * In either case there is nothing we can do.
         */
        orte_show_help("help-opal-checkpoint.txt", "pid_does_not_exist", true,
                       vpid_snapshot->process_pid,
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

    vpid_snapshot->comm_pipe_r_fd = open(vpid_snapshot->comm_pipe_r, O_RDONLY);
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

static int snapc_full_local_start_ckpt_handshake_opts(orte_snapc_full_app_snapshot_t *vpid_snapshot,
                                                      opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    int opt_rep;

    /*
     * Start the handshake:
     * - Send the migrating options [All, this proc]
     * - Send term argument
     * - Send stop argument
     */
    if( vpid_snapshot->migrating ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Tell app to MIGRATE. [%s (%d)]\n",
                             (vpid_snapshot->migrating ? "True" : "False"),
                             (int)(currently_migrating) ));
    }
    if( options->term ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Tell app to TERMINATE after completion of checkpoint. [%s]\n",
                             (options->term ? "True" : "False") ));
    }
    if( options->stop ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Tell app to STOP after completion of checkpoint. [%s]\n",
                             (options->stop ? "True" : "False") ));
    }


    opt_rep = (int)(currently_migrating);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write migrating (%d) to named pipe (%s), %d\n",
                    vpid_snapshot->migrating, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(vpid_snapshot->migrating);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write migrating (%d) to named pipe (%s), %d\n",
                    vpid_snapshot->migrating, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(options->term);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write term (%d) to named pipe (%s), %d\n",
                    options->term, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(options->stop);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write stop (%d) to named pipe (%s), %d\n",
                    options->stop, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(options->inc_prep_only);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write inc_prep_only (%d) to named pipe (%s), %d\n",
                    options->stop, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(options->inc_recover_only);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write inc_recover_only (%d) to named pipe (%s), %d\n",
                    options->stop, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

#if OPAL_ENABLE_CRDEBUG == 1
    opt_rep = (int)(options->attach_debugger);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write attach_debugger (%d) to named pipe (%s), %d\n",
                    options->attach_debugger, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opt_rep = (int)(options->detach_debugger);
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "local) Error: Unable to write detach_debugger (%d) to named pipe (%s), %d\n",
                    options->detach_debugger, vpid_snapshot->comm_pipe_w, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
#endif

    /*
     * Send the SStore handle
     */
    if( sizeof(orte_sstore_base_handle_t) != (ret = write(vpid_snapshot->comm_pipe_w_fd,
                                                          &(local_global_snapshot.ss_handle), sizeof(orte_sstore_base_handle_t) )) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write sstore handle (%d) to named pipe (%s). %d\n",
                    (int)(local_global_snapshot.ss_handle), vpid_snapshot->comm_pipe_w, ret);
        ORTE_ERROR_LOG(OPAL_ERROR);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_start_ckpt_handshake(orte_snapc_full_app_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int value;

    /*
     * Wait for the appliation to respond
     */
    if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &value, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to read length from named pipe (%s). %d\n",
                    vpid_snapshot->comm_pipe_r, ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Check the response to make sure we can checkpoint this process */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:in_progress",
                       true,
                       vpid_snapshot->process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if( OPAL_CHECKPOINT_CMD_NULL == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_null",
                       true,
                       vpid_snapshot->process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else if ( OPAL_CHECKPOINT_CMD_ERROR == value ) {
        orte_show_help("help-opal-checkpoint.txt",
                       "ckpt:req_error",
                       true,
                       vpid_snapshot->process_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_event_set(orte_event_base, &(vpid_snapshot->comm_pipe_r_eh),
                   vpid_snapshot->comm_pipe_r_fd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   snapc_full_local_comm_read_event,
                   vpid_snapshot);
    vpid_snapshot->is_eh_active = true;
    opal_event_add(&(vpid_snapshot->comm_pipe_r_eh), NULL);

    /*
     * Let the application know that it can proceed
     */
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &value, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to write to named pipe (%s). %d\n",
                    vpid_snapshot->comm_pipe_w, ret);
        ORTE_ERROR_LOG(OPAL_ERROR);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_local_end_ckpt_handshake(orte_snapc_full_app_snapshot_t *vpid_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;

    /*
     * Make sure the pipe is open, so we do not try to do this twice
     */
    if( 0 > vpid_snapshot->comm_pipe_w_fd ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) end_handshake: Process %s closed pipe. Skipping. (%d)\n",
                             ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                             vpid_snapshot->comm_pipe_w_fd));
        return exit_status;
    }

    /*
     * Finish the handshake.
     */
    if( sizeof(int) != (ret = write(vpid_snapshot->comm_pipe_w_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Local) Error: Unable to release process %s (%d)\n",
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
    int ret;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    int ckpt_state;
    int loc_min_state;
    char * state_str = NULL;

    vpid_snapshot = (orte_snapc_full_app_snapshot_t *)arg;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) Read Event: Process %s done checkpointing...\n",
                         ORTE_NAME_PRINT(&vpid_snapshot->super.process_name)));

    /*
     * Get the final state of the checkpoint from the checkpointing process
     */
    if( !vpid_snapshot->migrating ) {
        if( sizeof(int) != (ret = read(vpid_snapshot->comm_pipe_r_fd, &ckpt_state, sizeof(int))) ) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "Local) Error: Unable to read state from named pipe (%s). %d\n",
                        vpid_snapshot->comm_pipe_r, ret);
            ORTE_ERROR_LOG(ORTE_ERROR);
            goto cleanup;
        }

        /*
         * If only doing INC Prep phase, then jump out
         */
        if( local_global_snapshot.options->inc_prep_only &&
            OPAL_CRS_RUNNING == ckpt_state ) {
            /*
             * If all local procs are done, then tell the Global coord
             */
            vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_INC_PREPED;
            loc_min_state = snapc_full_get_min_state();
            if( loc_min_state > current_job_ckpt_state &&
                ORTE_SNAPC_CKPT_STATE_INC_PREPED == loc_min_state ) {
                if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(ORTE_SNAPC_CKPT_STATE_INC_PREPED, false) ) ) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
            }

            /* Just return */
            return;
        }
    }

    /*
     * Now that the checkpoint is finished
     * Update our status information
     */
    if( ckpt_state == OPAL_CRS_ERROR ) {
        vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_ERROR;
    } else {
        vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL;
    }

    /*
     * Flush GrpComm modex info if migrating
     */
    if( currently_migrating && !flushed_modex ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Read Event: Flush the modex cached data\n"));

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

        flushed_modex = true;
    }

    /*
     * If error, then exit early
     */
    if( ORTE_SNAPC_CKPT_STATE_ERROR == vpid_snapshot->super.state ) {
        /* JJH - The error path needs some more work */
        if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(ORTE_SNAPC_CKPT_STATE_ERROR, true) ) ) {
            ORTE_ERROR_LOG(ret);
        }
        goto cleanup;
    }

    /*
     * If all processes have finished locally, notify Global Coordinator
     */
    loc_min_state = snapc_full_get_min_state();
    if( loc_min_state > current_job_ckpt_state &&
        ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL == loc_min_state ) {

        orte_snapc_ckpt_state_str(&state_str, loc_min_state);
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) Daemon State Changed: %d (%s)",
                             (int)loc_min_state, state_str ));
        free(state_str);
        state_str = NULL;

        /*
         * Notify the Global Coordinator
         */
        current_job_ckpt_state = loc_min_state;
        if( ORTE_SNAPC_GLOBAL_COORD_TYPE != (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
            if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(loc_min_state, false) ) ) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }
        }

        /*
         * Sync the SStore
         * If we stopped the process then we already did this
         */
        if( !local_global_snapshot.options->stop ) {
            orte_sstore.sync(local_global_snapshot.ss_handle);
            sstore_local_sync_finished = true;
            /*
             * If the processes finished before we finished sync'ing
             * then we need to cleanup.
             */
            if( sstore_local_procs_finished ) {
                if( ORTE_SUCCESS != (ret = orte_snapc_full_local_reset_coord()) ) {
                    ORTE_ERROR_LOG(ret);
                    goto cleanup;
                }
            }
        }

        /* If this process is also the global coord, then we have to update -after- we sync locally */
        if( ORTE_SNAPC_GLOBAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_GLOBAL_COORD_TYPE)) {
            if( ORTE_SUCCESS != (ret = snapc_full_local_update_coord(loc_min_state, false) ) ) {
                ORTE_ERROR_LOG(ret);
                goto cleanup;
            }
        }
    }

 cleanup:
    /*
     * Disable events
     */
    opal_event_del(&(vpid_snapshot->comm_pipe_r_eh));
    vpid_snapshot->is_eh_active = false;

    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    return;
}

static int snapc_full_get_min_state(void)
{
    int min_state = ORTE_SNAPC_CKPT_MAX;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;
    char * state_str_a = NULL;
    char * state_str_b = NULL;

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        if( NULL != state_str_a ) {
            free(state_str_a);
        }
        if( NULL != state_str_b ) {
            free(state_str_b);
        }

        orte_snapc_ckpt_state_str(&state_str_a, vpid_snapshot->super.state);
        orte_snapc_ckpt_state_str(&state_str_b, min_state);

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Local) ... %s Checking [%d %s] vs [%d %s]",
                             ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                             (int)vpid_snapshot->super.state, state_str_a,
                             (int)min_state, state_str_b ));
        if( min_state > vpid_snapshot->super.state ) {
            min_state = vpid_snapshot->super.state;
        }
    }

    if( NULL != state_str_b ) {
        free(state_str_b);
        state_str_b = NULL;
    }
    orte_snapc_ckpt_state_str(&state_str_b, min_state);
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Local) ... Min State [%d %s]",
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

static int snapc_full_local_get_vpids(void)
{
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    int i;
    orte_proc_t *child = NULL;
    size_t list_len = 0;

    /*
     * If the list is populated, and has updated pid information then
     * there is nothing to update.
     */
    list_len = opal_list_get_size(&(local_global_snapshot.local_snapshots));
    if( list_len > 0 ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)opal_list_get_first(&(local_global_snapshot.local_snapshots));
        if( 0 < vpid_snapshot->process_pid ) {
            return ORTE_SUCCESS;
        }
    }

    /*
     * Otherwise update or populate the list
     */
    for (i=0; i < orte_local_children->size; i++) {
	    if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
		    continue;
	    }

        /* if the list is empty or this child is not in the list then add it */
        if( 0    >= list_len ||
            NULL == (vpid_snapshot = find_vpid_snapshot(&child->name)) ) {
            vpid_snapshot = OBJ_NEW(orte_snapc_full_app_snapshot_t);
            opal_list_append(&(local_global_snapshot.local_snapshots), &(vpid_snapshot->super.super));
        }

        /* Only update if the PID is -not- already set */
        if( 0 >= vpid_snapshot->process_pid ) {
            vpid_snapshot->process_pid              = child->pid;
            vpid_snapshot->super.process_name.jobid = child->name.jobid;
            vpid_snapshot->super.process_name.vpid  = child->name.vpid;
        }
    }

    return ORTE_SUCCESS;
}

static int snapc_full_local_refresh_vpids(void)
{
    opal_list_item_t *v_item = NULL;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    int i;
    orte_proc_t *child = NULL;
    bool found = false;

    /*
     * First make sure that all of the vpids in the list are still our
     * children (they may have moved)
     */
    for(v_item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        v_item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        v_item  = opal_list_get_next(v_item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)v_item;

        found = false;
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }

            if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                           &child->name,
                                                           &(vpid_snapshot->super.process_name) )) {
                found = true;
                break;
            }
        }
        if( !found ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Refresh List: Remove Process %s (%5d) from Daemon %s",
                                 ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                                 vpid_snapshot->process_pid,
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) ));
            opal_list_remove_item(&(local_global_snapshot.local_snapshots), v_item);
        }
    }

    /*
     * Next pass to find new processes that we are not already tracking
     * (processes migrated to us).
     */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }

        /* if the list is empty or this child is not in the list then add it */
        if( NULL == (vpid_snapshot = find_vpid_snapshot(&child->name)) ) {
            vpid_snapshot = OBJ_NEW(orte_snapc_full_app_snapshot_t);

            vpid_snapshot->process_pid              = child->pid;
            vpid_snapshot->super.process_name.jobid = child->name.jobid;
            vpid_snapshot->super.process_name.vpid  = child->name.vpid;
            /*vpid_snapshot->migrating = true;*/

            opal_list_append(&(local_global_snapshot.local_snapshots), &(vpid_snapshot->super.super));

            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Local) Refresh List: Add Process %s (%5d) to Daemon %s",
                                 ORTE_NAME_PRINT(&vpid_snapshot->super.process_name),
                                 vpid_snapshot->process_pid,
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) ));
        }
        /* Only update if the PID is -not- already set */
        else if( 0 >= vpid_snapshot->process_pid ) {
            vpid_snapshot->process_pid              = child->pid;
            vpid_snapshot->super.process_name.jobid = child->name.jobid;
            vpid_snapshot->super.process_name.vpid  = child->name.vpid;
        }
    }

    return ORTE_SUCCESS;
}

static orte_snapc_full_app_snapshot_t *find_vpid_snapshot(orte_process_name_t *name )
{
    opal_list_item_t* item = NULL;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    orte_ns_cmp_bitmask_t mask;

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        mask = ORTE_NS_CMP_ALL;

        if (OPAL_EQUAL ==
                orte_util_compare_name_fields(mask, name, &vpid_snapshot->super.process_name)) {
            return vpid_snapshot;
        }
    }

    return NULL;
}

static int orte_snapc_full_local_reset_coord(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_app_snapshot_t *vpid_snapshot = NULL;
    opal_list_item_t* item = NULL;

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Local) Job Ckpt finished - Cleanup\n"));

    for(item  = opal_list_get_first(&(local_global_snapshot.local_snapshots));
        item != opal_list_get_end(&(local_global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_snapc_full_app_snapshot_t*)item;

        /* If we forgot to close the pipes to the application, then do so
         * now. It is rare that this would have happened, so more of a
         * sanity check as part of cleanup.
         */
        if( vpid_snapshot->comm_pipe_w_fd > 0  ) {
            if( ORTE_SUCCESS != (ret = local_coord_job_state_update_finished_local_vpid(vpid_snapshot) ) ) {
                ORTE_ERROR_LOG(ORTE_ERROR);
                goto cleanup;
            }
        }

        vpid_snapshot->super.state = ORTE_SNAPC_CKPT_STATE_NONE;
    }

    /*
     * Clear globally cached options
     */
    opal_crs_base_clear_options(local_global_snapshot.options);

    currently_migrating = false;
    flushed_modex = false;

    sstore_local_sync_finished  = false;
    sstore_local_procs_finished = false;

 cleanup:
    return exit_status;
}
