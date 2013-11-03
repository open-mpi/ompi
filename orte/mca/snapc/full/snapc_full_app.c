/*
 * Copyright (c) 2004-2012 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include <errno.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"
#include "opal/mca/event/event.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static void snapc_full_app_signal_handler (int signo);
static int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp);
static int app_notify_resp_stage_1(opal_cr_ckpt_cmd_state_t resp);
static int app_notify_resp_stage_2(int cr_state );
static int app_notify_resp_stage_3(int cr_state, bool skip_fin_msg);
static int app_define_pipe_names(void);
static int snapc_full_app_notify_reopen_files(void);
static int snapc_full_app_ckpt_handshake_start(opal_cr_ckpt_cmd_state_t resp);
static int snapc_full_app_ckpt_handshake_end(int cr_state);

static int snapc_full_app_ft_event_update_process_info(orte_process_name_t proc, pid_t pid);
static int snapc_full_app_finished_msg(int cr_state);

static int app_notify_resp_inc_prep_only(int cr_state);

static char *app_comm_pipe_r = NULL;
static char *app_comm_pipe_w = NULL;
static int   app_comm_pipe_r_fd = -1;
static int   app_comm_pipe_w_fd = -1;

static opal_crs_base_snapshot_t *local_snapshot = NULL;

static bool app_notif_processed = false;

static bool currently_migrating = false;
static bool currently_all_migrating = false;

static bool currently_checkpointing = false;
static int  current_unique_id = 0;

static int current_cr_state = OPAL_CRS_NONE;

static orte_sstore_base_handle_t current_ss_handle = ORTE_SSTORE_HANDLE_INVALID, last_ss_handle = ORTE_SSTORE_HANDLE_INVALID;
static opal_crs_base_ckpt_options_t *current_options = NULL;

/************************
 * Function Definitions
 ************************/

int app_coord_init()
{
    int ret, exit_status  = ORTE_SUCCESS;
    opal_cr_notify_callback_fn_t prev_notify_func;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_REQUEST_OP_CMD;
    orte_snapc_base_request_op_event_t op_event = ORTE_SNAPC_OP_INIT;
    opal_buffer_t buffer;
    orte_grpcomm_collective_t *coll;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "App) Initalized for Application %s\n", 
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /*
     * Register the INC notification callback
     */
    opal_cr_reg_notify_callback(snapc_full_app_notify_response, &prev_notify_func);

    /*
     * Set the pipe names
     */
    current_unique_id = 0;
    app_define_pipe_names();

    /*
     * Setup a signal handler to catch and start the proper thread
     * to handle the checkpoint
     */
    if( SIG_ERR == signal(opal_cr_entry_point_signal, snapc_full_app_signal_handler) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) init: Error: Failed to register signal %d\n",
                    opal_cr_entry_point_signal);
        ORTE_ERROR_LOG(OPAL_ERROR);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "app) Named Pipes (%s) (%s), Signal (%d)", 
                         app_comm_pipe_r, app_comm_pipe_w, opal_cr_entry_point_signal));

    /*
     * All processes must sync here, so the Global coordinator can know that
     * it is safe to checkpoint now.
     * Rank 0: Sends confirmation message to the Global Coordinator
     */
    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Startup Barrier..."));
    }

    coll = OBJ_NEW(orte_grpcomm_collective_t);
    coll->id = orte_process_info.peer_init_barrier;
    if( ORTE_SUCCESS != (ret = orte_grpcomm.barrier(coll)) ) {
	    ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    coll->active = true;
    ORTE_WAIT_FOR_COMPLETION(coll->active);
    OBJ_RELEASE(coll);

    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Startup Barrier: Send INIT to HNP...!"));

        OBJ_CONSTRUCT(&buffer, opal_buffer_t);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(op_event), 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        OBJ_DESTRUCT(&buffer);
    }

    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Startup Barrier: Done!"));
    }

 cleanup:
    return exit_status;
}

int app_coord_finalize()
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_REQUEST_OP_CMD;
    orte_snapc_base_request_op_event_t op_event = ORTE_SNAPC_OP_FIN;
    opal_buffer_t buffer;
    orte_std_cntr_t count;
    orte_grpcomm_collective_t *coll;

    /*
     * All processes must sync here, so the Global coordinator can know that
     * it is no longer safe to checkpoint.
     * Rank 0: Sends confirmation message to the Global Coordinator
     */
    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Shutdown Barrier..."));
    }

    coll = OBJ_NEW(orte_grpcomm_collective_t);
    coll->id = orte_process_info.peer_init_barrier;
    if( ORTE_SUCCESS != (ret = orte_grpcomm.barrier(coll)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    coll->active = true;
    ORTE_WAIT_FOR_COMPLETION(coll->active);

    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Shutdown Barrier: Send FIN to HNP...!"));

        /* Tell HNP that we are finalizing */
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(op_event), 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        OBJ_DESTRUCT(&buffer);

        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Shutdown Barrier: Waiting on FIN_ACK...!"));

        /* Wait for HNP to tell us that it is ok to finish finalization.
         * We could have been checkpointing just as we entered finalize, so we
         * need to wait until the checkpoint is finished before finishing.
         */
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);
        if (0 > (ret = orte_rml.recv_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &op_event, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Shutdown Barrier: Waiting on barrier...!"));
    }

    coll->id = orte_process_info.peer_fini_barrier;
    if( ORTE_SUCCESS != (ret = orte_grpcomm.barrier(coll)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((3, mca_snapc_full_component.super.output_handle,
                             "app) Shutdown Barrier, Done!"));
    }

 cleanup:
    /* cleanup */
    OBJ_RELEASE(coll);

    /*
     * Cleanup named pipes
     */
    if( NULL != app_comm_pipe_r) {
        free(app_comm_pipe_r);
        app_comm_pipe_r = NULL;
    }

    if( NULL != app_comm_pipe_w) {
        free(app_comm_pipe_w);
        app_comm_pipe_w = NULL;
    }

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/
static void snapc_full_app_signal_handler (int signo)
{
    if( opal_cr_entry_point_signal != signo ) {
        OPAL_OUTPUT_VERBOSE((1, mca_snapc_full_component.super.output_handle,
                             "App) signal_handler: Received unknown signal %d",
                             signo));
        /* Not our signal */
        return;
    }
    if( currently_checkpointing ) {
        opal_output(0, "snapc:full:(app) Error: Received a signal to checkpoint, but Already checkpointing. Ignoring request!");
    }
    else {
        currently_checkpointing = true;
        /*
         * Signal thread to start checkpoint handshake
         */
        opal_cr_checkpoint_request   = OPAL_CR_STATUS_REQUESTED;

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) signal_handler: Receive Checkpoint Request."));
    }
}

/*
 * Respond to an asynchronous checkpoint request
 */
int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp)
{
    static int cr_state;
    int app_pid;
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Clear the options set
     */
    if( NULL == current_options ) {
        current_options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    }

    if( opal_cr_currently_stalled ) {
        goto STAGE_1;
    }

    /* Default: use the fast way */
    orte_cr_continue_like_restart = false;
    orte_cr_flush_restart_files   = true;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Stage 1..."));
    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_1(resp) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto ckpt_cleanup;
    }

    cr_state = OPAL_CRS_RUNNING;
    current_cr_state = cr_state;

#if OPAL_ENABLE_CRDEBUG == 1
    if( current_options->attach_debugger ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: C/R Debug: Wait for debugger..."));
        MPIR_debug_with_checkpoint = true;
    }
    if( current_options->detach_debugger ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: C/R Debug: Do not wait for debugger..."));
        MPIR_debug_with_checkpoint = false;
    }
#endif

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Start checkpoint..."));
 STAGE_1:
    opal_cr_currently_stalled = false;

    app_pid = getpid();
    if( orte_snapc_full_skip_app ) {
        OPAL_OUTPUT_VERBOSE((2, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Skipping App. (%d)\n",
                             app_pid));
        ret = ORTE_SUCCESS;
        cr_state = OPAL_CRS_CONTINUE;
    }
    else {
        /*
         * INC: Prepare stack using the registered coordination routine
         */
        if(OPAL_SUCCESS != (ret = opal_cr_inc_core_prep() ) ) {
            if( OPAL_EXISTS == ret ) {
                OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                     "App) notify_response: Stalling the checkpoint progress until state is stable again (PID = %d)\n",
                                     app_pid));
                opal_cr_currently_stalled = true;
                return exit_status;
            }
            else {
                opal_output(mca_snapc_full_component.super.output_handle,
                            "App) notify_response: Error: checkpoint notification failed. %d\n", ret);
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto ckpt_cleanup;
            }
        }

        /*
         * If this is a quiesce_start operation then we can stop here after calling
         * the INC prep. Need to keep the connection open for the quiesce_end()
         * operation though.
         */
        if( current_options->inc_prep_only ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_response: INC Prep Only..."));
            return app_notify_resp_inc_prep_only(cr_state);
        } else {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_response: Normal operation..."));
        }

        /*
         * INC: Take the checkpoint
         *
         * If migrating, only checkpoint if you are the target process
         * otherwise just continue.
         */
        if( currently_all_migrating ) {
            orte_cr_continue_like_restart = true;
            orte_cr_flush_restart_files   = false;
        }
        if( !currently_migrating && currently_all_migrating ) {
            OPAL_OUTPUT_VERBOSE((2, mca_snapc_full_component.super.output_handle,
                                 "App) notify_response: Skipping App. (%d) - This process is not migrating \n",
                                 app_pid));
            ret = ORTE_SUCCESS;
            cr_state = OPAL_CRS_CONTINUE;
        }
        else {
            ret = opal_cr_inc_core_ckpt(app_pid, local_snapshot, current_options, &cr_state);
        }
        current_cr_state = cr_state;

        /*
         * Tell Local Coordinator that we are done with local checkpoint
         * (only if not restarting, on restart we are not attached to the Local
         *  Coordinator. )
         */
        if( OPAL_CRS_RESTART != cr_state ) {
            OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                 "App) notify_response: Stage 2..."));
            if( ORTE_SUCCESS != (ret = app_notify_resp_stage_2(cr_state) ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto ckpt_cleanup;
            }
        }

        /*
         * INC: Recover stack using the registered coordination routine
         */
        if( !currently_all_migrating ) {
            if( OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(cr_state)) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto ckpt_cleanup;
            }
        }
        /*
         * If this is a migrating target process, then do not recover the stack, but terminate.
         * All non-migrating processes will wait in the recovery until the target processes are
         * restarted on the target nodes.
         */
        else {
            /*
             * If we are one of the processes migrating, then terminate after checkpointing
             */
            if( currently_migrating ) {
                if( OPAL_CRS_RESTART != cr_state ) {
                    current_options->term = true;
                }
                else {
                    if( OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(cr_state)) ) {
                        ORTE_ERROR_LOG(ret);
                        exit_status = ret;
                        goto ckpt_cleanup;
                    }
                }
            }
            /*
             * If we are not one of the processes migrating, then wait for release.
             * Need to act like we are restarting during recovery, since the migrating processes
             * will expect this logic.
             */
            else {
                if( OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(OPAL_CRS_RESTART)) ) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    goto ckpt_cleanup;
                }
            }
        }
    }

    /* Don't stall any longer */
    opal_cr_stall_check = false;

    if(OPAL_CRS_RESTART == cr_state) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Restarting... (%s : %d)\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app_pid));
        
        current_options->term = false;
        /* Do not respond to the non-existent command line tool */
        goto ckpt_cleanup;
    }
    else if(cr_state == OPAL_CRS_CONTINUE) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Continuing...(%s : %d)\n",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app_pid));
        ; /* Don't need to do anything here */
    }
    else if(cr_state == OPAL_CRS_TERM ) {
        ; /* Don't need to do anything here */
    }
    else {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Unknown cr_state(%d) [%d]",
                             cr_state, app_pid));
    }

 ckpt_cleanup:
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Stage 3..."));
    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_3(cr_state, false) )) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto ckpt_cleanup;
    }

    if( current_options->term ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: User has asked to terminate the application"));
        /* Wait here for termination.
         * If we call 'exit' then the job will fail in an ugly way, instead just
         * wait for the Global coordinator to terminate us.
         */
        while(1) {
            opal_progress();
            sleep(1);
        }
    }

    if( NULL != current_options ) {
        OBJ_RELEASE(current_options);
        current_options = NULL;
    }

    currently_checkpointing = false;

    return exit_status;
}

static int app_notify_resp_stage_1(opal_cr_ckpt_cmd_state_t resp)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_CR_CLEAR_TIMERS();
    opal_cr_timing_my_rank = ORTE_PROC_MY_NAME->vpid;
    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY0);

    /*
     * Open communication channels
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Open Communication Channels."));
    if (ORTE_SUCCESS != (ret = snapc_full_app_notify_reopen_files())) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Initial Handshake
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Initial Handshake."));
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_start(resp) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY1);

    /*
     * Register with SStore
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Register with SStore..."));
    if( OPAL_SUCCESS != (ret = orte_sstore.register_handle(current_ss_handle)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    local_snapshot = OBJ_NEW(opal_crs_base_snapshot_t);

    if( !currently_migrating && currently_all_migrating ) {
        orte_sstore.set_attr(current_ss_handle,
                             SSTORE_METADATA_LOCAL_SKIP_CKPT,
                             "1");
    }

    orte_sstore.get_attr(current_ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_LOC,
                         &(local_snapshot->snapshot_directory));
    orte_sstore.get_attr(current_ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_META,
                         &(local_snapshot->metadata_filename));

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY2);

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Start checkpoint... (%d)", (int)current_ss_handle));

 cleanup:
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Are we migrating [%5s]. Am I migrating [%5s]",
                         (currently_all_migrating ? "True" : "False"),
                         (currently_migrating ? "True" : "False") ));

    return exit_status;
}

static int app_notify_resp_inc_prep_only(int cr_state)
{
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Tell the local coordinator that we are done with the INC prep
     */
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &cr_state, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to write cr_state to named pipe (%s).\n",
                    app_comm_pipe_w);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    app_notif_processed = true;

 cleanup:
    return exit_status;
}

static int app_notify_resp_stage_2(int cr_state )
{
    int ret;

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY3);

    /*
     * Sync SStore
     * If we stopped the process, then we already did this
     */
    if( !(current_options->stop) ) {
        if( currently_migrating || !currently_all_migrating ) {
            orte_sstore.set_attr(current_ss_handle,
                                 SSTORE_METADATA_LOCAL_CRS_COMP,
                                 local_snapshot->component_name);
        }

        orte_sstore.sync(current_ss_handle);
    }
    last_ss_handle = current_ss_handle;
    current_ss_handle = 0;

    /*
     * Final Handshake
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Waiting for final handshake."));
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_end(cr_state ) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Final Handshake complete."));

    return ORTE_SUCCESS;
}

static int app_define_pipe_names(void)
{
    if( NULL != app_comm_pipe_r ) {
        free(app_comm_pipe_r);
        app_comm_pipe_r = NULL;
    }

    if( NULL != app_comm_pipe_w ) {
        free(app_comm_pipe_w);
        app_comm_pipe_w = NULL;
    }

    asprintf(&app_comm_pipe_r, "%s/%s.%d_%d",
             opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R,
             (int)getpid(), current_unique_id);
    asprintf(&app_comm_pipe_w, "%s/%s.%d_%d",
             opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W,
             (int)getpid(), current_unique_id);

    ++current_unique_id;

    return ORTE_SUCCESS;
}

static int app_notify_resp_stage_3(int cr_state, bool skip_fin_msg)
{
    /*
     * Send a message to the local daemon letting it know that we are done
     */
    if( !skip_fin_msg ) {
        snapc_full_app_finished_msg(cr_state);
    }

    /*
     * Close and cleanup pipes
     */
    if( 0 <= app_comm_pipe_r_fd ) {
        close(app_comm_pipe_r_fd);
        app_comm_pipe_r_fd = -1;
    }
    if( 0 <= app_comm_pipe_w_fd ) {
        close(app_comm_pipe_w_fd);
        app_comm_pipe_w_fd = -1;
    }

    remove(app_comm_pipe_r);
    remove(app_comm_pipe_w);

    app_comm_pipe_r_fd = -1;
    app_comm_pipe_w_fd = -1;

    if( OPAL_CRS_RESTART == cr_state ) {
        current_unique_id = 0;
    }

    app_define_pipe_names();

    /* Prepare to wait for another checkpoint action */
    opal_cr_checkpointing_state = OPAL_CR_STATUS_NONE;
    opal_cr_currently_stalled   = false;

    currently_all_migrating = false;
    currently_migrating     = false;

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY4);
    if(OPAL_CRS_RESTART != cr_state) {
        OPAL_CR_DISPLAY_ALL_TIMERS();
    }

    return ORTE_SUCCESS;
}

static int snapc_full_app_finished_msg(int cr_state) {
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_LOCAL_FINISH_CMD;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &cr_state, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SNAPC, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

static int snapc_full_app_notify_reopen_files(void)
{
    int ret = OPAL_ERR_NOT_IMPLEMENTED;

#ifndef HAVE_MKFIFO
    return ret;
#else
    /*
     * Open up the read pipe
     */
    if( (ret = mkfifo(app_comm_pipe_r, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                 app_comm_pipe_r, ret));
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "App) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_r, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_r_fd = open(app_comm_pipe_r, O_RDWR);
    if(app_comm_pipe_r_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) init: Error: open failed to open the named pipe (%s). %d\n",
                    app_comm_pipe_r, app_comm_pipe_r_fd);
        return ORTE_ERROR;
    }

    /*
     * Open up the write pipe
     */
    if( (ret = mkfifo(app_comm_pipe_w, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                 app_comm_pipe_w, ret));
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "App) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_w, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_w_fd = open(app_comm_pipe_w, O_WRONLY);
    if(app_comm_pipe_w_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_reopen_files: Error: open failed to open the named pipe (%s). (%d)\n",
                    app_comm_pipe_w, app_comm_pipe_w_fd);
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
#endif  /* HAVE_MKFIFO */
}

static int snapc_full_app_ckpt_handshake_start(opal_cr_ckpt_cmd_state_t resp)
{
    int ret, exit_status = ORTE_SUCCESS;
    int tmp_resp, opt_rep;

    /*
     * Get the initial handshake command:
     * - Migrating option [all, me]
     * - Term argument
     * - Stop argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the all_migrating option from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    currently_all_migrating = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the migrating option from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    currently_migrating = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'term' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->term = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'stop' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->stop = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'inc_prep_only' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->inc_prep_only = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'inc_recover_only' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->inc_recover_only = OPAL_INT_TO_BOOL(opt_rep);

#if OPAL_ENABLE_CRDEBUG == 1
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'attach_debugger' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->attach_debugger = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'detach_debugger' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    current_options->detach_debugger = OPAL_INT_TO_BOOL(opt_rep);
#endif

    /*
     * Get SStore Handle
     */
    if( sizeof(orte_sstore_base_handle_t) != (ret = read(app_comm_pipe_r_fd, &current_ss_handle, sizeof(orte_sstore_base_handle_t))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the sstore handle from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) %s Received Options... Responding with %d\n", 
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)resp));

    /*
     * Write back the response to the request (message printed below)
     */
    tmp_resp = (int)resp;
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &tmp_resp, sizeof(int)) ) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: %d: Error: Unable to write to pipe (%s) ret = %d [Line %d]\n",
                    tmp_resp, app_comm_pipe_w, ret, __LINE__);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * Respond that the checkpoint is currently in progress
     */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Checkpoint in progress, cannot start (%d)",
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    /*
     * Respond that the application is unable to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_NULL == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Non-checkpointable application, cannot start (%d)", 
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    /*
     * Respond that some error has occurred such that the application is 
     * not able to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_ERROR == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Error generated, cannot start (%d)", 
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * Respond signalng that we wish to respond to this request
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Starting checkpoint request (%d)", 
                         getpid()));

    /*
     * Get the sentinel value indicating that we can start now
     * JJH: Check for an error here indicating that even though this process is
     *      OK to checkpoint others might not be in which case we should cleanup
     *      properly.
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_app_ckpt_handshake_end(int cr_state)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;
    int err;

    /*
     * Return the final checkpoint state to the local coordinator
     */
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &cr_state, sizeof(int))) ) {
        err = errno;
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to write cr_state to named pipe (%s). %d/%d/%s\n",
                    app_comm_pipe_w, ret, err, strerror(err));
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( currently_all_migrating && currently_migrating ) {
        app_notify_resp_stage_3(cr_state, true);
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) handshake_end: Waiting for termination (%d)",
                             getpid()));
        /* Wait here for termination, do not terminate ourselves.
         * JJH: We cannot terminate ourselves without killing the job...
         */
        while(1) {
            opal_progress();
            sleep(1);
        }
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "App) handshake_end: Waiting for release (%d)",
                         getpid()));

    /*
     * Wait for the local coordinator to release us
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'last_cmd' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "App) handshake_end: Released... (%d)",
                         getpid()));

 cleanup:
    return exit_status;
}

int app_coord_ft_event(int state) {
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "App) In ft_event(%d)", state));

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
        /*
         * Record the job session directory
         * This way we will recreate it on restart so that any components that
         * have old references to it (like btl/sm) can reference their files
         * (to close the fd's to them) on restart. We will remove it before we
         * create the new session directory.
         */
        orte_sstore.set_attr(orte_sstore_handle_current,
                             SSTORE_METADATA_LOCAL_MKDIR,
                             orte_process_info.job_session_dir);

        /*
         * If stopping then sync early
         */
        if( current_options->stop ) {
            orte_sstore.set_attr(current_ss_handle,
                                 SSTORE_METADATA_LOCAL_CRS_COMP,
                                 opal_crs_base_selected_component.base_version.mca_component_name);

            orte_sstore.sync(current_ss_handle);
        }
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
#if OPAL_ENABLE_CRDEBUG == 1
        /*
         * Send PID to HNP/daemon if debugging as an indicator that we have
         * finished the checkpoint operation.
         */
        if( ORTE_SUCCESS != (ret = snapc_full_app_ft_event_update_process_info(orte_process_info.my_name, getpid())) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
#endif
        ; /* Nothing */
    }
    /******** Restart Pre-Recovery ********/
    else if (OPAL_CRS_RESTART_PRE == state ) {
        ; /* Nothing */
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "App) Initalized for Application %s (Restart) (%5d)\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), getpid()));

        /*
         * Send new PID to HNP/daemon
         * The checkpointer could have used a proxy program to boot us
         * so the pid that the orted got from fork() may not be the
         * PID of this application.
         * - Note: BLCR does this because it tries to preseve the PID
         *         of the program across checkpointes
         */
        if( ORTE_SUCCESS != (ret = snapc_full_app_ft_event_update_process_info(orte_process_info.my_name, getpid())) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        /*
         * Since this process is interacting with an 'old' daemon, we must make
         * sure to sync twice.
         * JJH: This assumes that we only move whole nodes, this may be wrong
         * JJH: when interacting with partial migration
         */
        if( currently_all_migrating && !currently_migrating ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) ft_event(RESTART): Not a migrating process, so re-sync"));
            orte_routed_base_register_sync(false);
        }

        /*
         * JJH: Optionally the non-migrating processes can wait here in stage_2
         * JJH: This will delay the initial checkpoint, but potentially speed up
         * JJH: restart.
         */
    }
    /******** Termination ********/
    else if (OPAL_CRS_TERM == state ) {
        ; /* Nothing */
    }
    /******** Error State ********/
    else {
        ; /* Nothing */
    }

 cleanup:
    return exit_status;
}

static int snapc_full_app_ft_event_update_process_info(orte_process_name_t proc, pid_t proc_pid)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_LOCAL_UPDATE_CMD;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* JJH CLEANUP: Do we really need this, it is equal to sender */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc_pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

#if OPAL_ENABLE_CRDEBUG == 1
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &MPIR_debug_with_checkpoint, 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
#endif

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SNAPC, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

int app_coord_request_op(orte_snapc_base_request_op_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_REQUEST_OP_CMD;
    opal_buffer_t buffer;
    orte_std_cntr_t count;
    int op_event, op_state;
    char *seq_str = NULL, *tmp_str = NULL;
    int cr_state = OPAL_CRS_CONTINUE;
    int app_pid, i;

    /*
     * Quiesce_end recovers the library before talking to the Global coord.
     */
    if( ORTE_SNAPC_OP_QUIESCE_END == datum->event) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_end: Recovering the stack..."));

        /*
         * INC: Recover the stack
         */
        if( NULL == local_snapshot->component_name ) {
            local_snapshot->component_name = strdup("");
        }
        if( ORTE_SUCCESS != (ret = app_notify_resp_stage_2(cr_state) ) ) {
            exit_status = ret;
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if(OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(cr_state) ) ) {
            exit_status = ret;
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        if( ORTE_SUCCESS != (ret = app_notify_resp_stage_3(cr_state, false) )) {
            exit_status = ret;
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        currently_checkpointing = false;
        app_notif_processed = false;

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_end: Recovered."));
    }
    else if( ORTE_SNAPC_OP_QUIESCE_CHECKPOINT == datum->event) {
        app_pid = getpid();
        cr_state = OPAL_CRS_RUNNING;
        if( OPAL_SUCCESS != (ret = opal_cr_inc_core_ckpt(app_pid, local_snapshot, current_options, &cr_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
        }

        if( OPAL_CRS_RESTART != cr_state ) {
            orte_sstore.sync(current_ss_handle);
        }

        orte_sstore.get_attr(current_ss_handle,
                             SSTORE_METADATA_GLOBAL_SNAP_SEQ,
                             &seq_str);
        if( NULL != seq_str ) {
            datum->seq_num = atoi(seq_str);
        } else {
            datum->seq_num = -1;
        }

        orte_sstore.get_attr(current_ss_handle,
                             SSTORE_METADATA_GLOBAL_SNAP_REF,
                             &(datum->global_handle));
        if( NULL == datum->global_handle ) {
            datum->global_handle = strdup("Unknown");
        }

        return exit_status;
    }

    /*
     * Leader: Send the info to the head node
     */
    if( datum->leader == (int)ORTE_PROC_MY_NAME->vpid ) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Request_op: Sending request (%3d)...",
                             datum->event));
        /*
         * Send request to HNP
         */
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(datum->event), 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        if( ORTE_SNAPC_OP_RESTART == datum->event) {
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(datum->seq_num), 1, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                OBJ_DESTRUCT(&buffer);
                goto cleanup;
            }
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(datum->global_handle), 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                OBJ_DESTRUCT(&buffer);
                goto cleanup;
            }
        }
        else if( ORTE_SNAPC_OP_MIGRATE == datum->event) {
            /*
             * Check information
             *  Rank  | Hostname  | cr_off_node  | Meaning
             * -------+-----------+--------------+---------
             *  self  | home/same | false        | Do not move this process
             *        |           | true         | ERROR
             *        | NULL      | false        | Move wherever
             *        |           | true         | Move off of this node
             *        | other     | false/true   | Move to the 'other' node
             * -------+-----------+--------------+---------
             *  peer  | home/same | false        | Move 'peer' to me
             *        |           | true         | ERROR
             *        | NULL      | false        | Move wherever (Default: Move 'peer' to me)
             *        |           | true         | Move with peer to some other node
             *        | other     | false/true   | Move with peer to 'other' node
             * -------+-----------+--------------+---------
             * If 'rank' is set to a peer other than self, and the peer sets
             * conflicting 'hostname' or 'cr_off_node' preferences, then that
             * is an error. In which case the migration should fail.
             */
            currently_all_migrating = true;

            /*
             * Send information
             */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(datum->mig_num), 1, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                OBJ_DESTRUCT(&buffer);
                goto cleanup;
            }

            for( i = 0; i < datum->mig_num; ++i ) {
                OPAL_OUTPUT_VERBOSE((30, mca_snapc_full_component.super.output_handle,
                                     "App) Migration %3d/%3d: Sending Rank %3d - Requested <%s> (%3d) %c\n",
                                     datum->mig_num, i,
                                     (datum->mig_vpids)[i],
                                     (datum->mig_host_pref)[i],
                                     (datum->mig_vpid_pref)[i],
                                     (OPAL_INT_TO_BOOL((datum->mig_off_node)[i]) ? 'T' : 'F')
                                     ));

                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &((datum->mig_vpids)[i]), 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    OBJ_DESTRUCT(&buffer);
                    goto cleanup;
                }
                tmp_str = strdup((datum->mig_host_pref)[i]);
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &tmp_str, 1, OPAL_STRING))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    OBJ_DESTRUCT(&buffer);
                    goto cleanup;
                }
                if( NULL != tmp_str ) {
                    free(tmp_str);
                    tmp_str = NULL;
                }

                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &((datum->mig_vpid_pref)[i]), 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    OBJ_DESTRUCT(&buffer);
                    goto cleanup;
                }
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &((datum->mig_off_node)[i]), 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    OBJ_DESTRUCT(&buffer);
                    goto cleanup;
                }
            }
        }

        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            goto cleanup;
        }

        OBJ_DESTRUCT(&buffer);
    }

    /*
     * Wait for the response
     */
    if( ORTE_SNAPC_OP_CHECKPOINT == datum->event) {
        if( datum->leader == (int)ORTE_PROC_MY_NAME->vpid ) {
            /*
             * Wait for local completion (need to check to see if we are restarting)
             */
            while(OPAL_CRS_CONTINUE != current_cr_state &&
                  OPAL_CRS_RESTART  != current_cr_state &&
                  OPAL_CRS_ERROR    != current_cr_state ) {
                opal_progress();
                OPAL_CR_TEST_CHECKPOINT_READY();
            }

            /* Do not wait for a response if we are restarting (it will never arrive) */
            if( OPAL_CRS_RESTART == current_cr_state ) {
                orte_sstore.get_attr(current_ss_handle,
                                     SSTORE_METADATA_GLOBAL_SNAP_SEQ,
                                     &seq_str);
                if( NULL != seq_str ) {
                    datum->seq_num = atoi(seq_str);
                } else {
                    datum->seq_num = -1;
                }

                orte_sstore.get_attr(current_ss_handle,
                                     SSTORE_METADATA_GLOBAL_SNAP_REF,
                                     &(datum->global_handle));
                if( NULL == datum->global_handle ) {
                    datum->global_handle = strdup("Unknown");
                }

                current_cr_state = OPAL_CRS_NONE;

                exit_status = ORTE_SUCCESS;
                goto cleanup;
            }

            OBJ_CONSTRUCT(&buffer, opal_buffer_t);

            /*
             * Wait for a response regarding completion
             */
            if (0 > (ret = orte_rml.recv_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                OBJ_DESTRUCT(&buffer);
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &op_event, &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &op_state, &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            OBJ_DESTRUCT(&buffer);

            orte_sstore.get_attr(last_ss_handle,
                                 SSTORE_METADATA_GLOBAL_SNAP_SEQ,
                                 &seq_str);
            datum->seq_num = atoi(seq_str);

            orte_sstore.get_attr(last_ss_handle,
                                 SSTORE_METADATA_GLOBAL_SNAP_REF,
                                 &(datum->global_handle));
        }
    }
    /*
     * Restart will terminate this process, so just wait...
     */
    else if( ORTE_SNAPC_OP_RESTART == datum->event) {
        while( 1 ) {
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY();
            sleep(1);
        }
    }
    /*
     * Leader waits for response
     */
    else if( ORTE_SNAPC_OP_MIGRATE == datum->event) {
        if( datum->leader == (int)ORTE_PROC_MY_NAME->vpid ) {
            while( currently_all_migrating ) {
                opal_progress();
                OPAL_CR_TEST_CHECKPOINT_READY();
                sleep(1);
            }

            OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                 "App) Request_op: Leader waiting for Migrate release (%3d)...",
                                 datum->event));

            OBJ_CONSTRUCT(&buffer, opal_buffer_t);

            /*
             * Wait for a response regarding completion
             */
            if (0 > (ret = orte_rml.recv_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                OBJ_DESTRUCT(&buffer);
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &op_event, &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            count = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&buffer, &op_state, &count, OPAL_INT))) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            OBJ_DESTRUCT(&buffer);

            OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                 "App) Request_op: Leader continuing from Migration (%3d)...",
                                 datum->event));
        }
    }
    /*
     * Everyone waits here for completion of Quiesce start
     */
    else if( ORTE_SNAPC_OP_QUIESCE_START == datum->event) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_start: Waiting for release..."));

        while( !app_notif_processed ) {
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY();
        }

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_start: Released"));
    }
    /*
     * No waiting for Quiesce end (barrier occurs in protocol)
     */
    else if( ORTE_SNAPC_OP_QUIESCE_END == datum->event) {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_end: Waiting for release..."));

        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) Quiesce_end: Released"));
    }


 cleanup:
    if( NULL != seq_str ) {
        free(seq_str);
        seq_str = NULL;
    }

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    return exit_status;
}
