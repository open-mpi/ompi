/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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

#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/orte_constants.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static void snapc_full_app_signal_handler (int signo);
static int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp);
static int snapc_full_app_notify_reopen_files(void);
static int snapc_full_app_ckpt_handshake_start(int *app_term, opal_cr_ckpt_cmd_state_t resp);
static int snapc_full_app_ckpt_handshake_end(int cr_state);

static char *app_comm_pipe_r = NULL;
static char *app_comm_pipe_w = NULL;
static int   app_comm_pipe_r_fd = -1;
static int   app_comm_pipe_w_fd = -1;

static opal_crs_base_snapshot_t *local_snapshot = NULL;

/************************
 * Function Definitions
 ************************/

int app_coord_init() {
    int exit_status  = ORTE_SUCCESS;
    opal_cr_notify_callback_fn_t prev_notify_func;
    char *tmp_pid = NULL;

    /*
     * Setup GPR callback that indicates if we should checkpoint ourselves
     * Currently this is relayed via the Local Snapshot Coordinator
     */

    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "app) Initalized for Application (%d.%d)\n", 
                        orte_process_info.my_name->jobid,
                        orte_process_info.my_name->vpid);

    /*
     * Register the notification callback
     */
    opal_cr_reg_notify_callback(snapc_full_app_notify_response, &prev_notify_func);
    opal_cr_entry_point_finalize();

    /* String representation of the PID */
    asprintf(&tmp_pid, "%d", getpid());

    asprintf(&app_comm_pipe_r, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
    asprintf(&app_comm_pipe_w, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);

    opal_output_verbose(15, opal_cr_output,
                        "opal_cr: init: Named Pipes (%s) (%s)", 
                        app_comm_pipe_r, app_comm_pipe_w);

    /*
     * Setup a signal handler to catch and start the proper thread
     * to handle the checkpoint
     */
    if( SIG_ERR == signal(opal_cr_entry_point_signal, snapc_full_app_signal_handler) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

 cleanup:
    if( NULL != tmp_pid) {
        free(tmp_pid);
        tmp_pid = NULL;
    }

    return exit_status;
}

int app_coord_finalize() {

    /*
     * Cleanup GPR callbacks
     */

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
        /* Not our signal */
        return;
    }
    /*
     * Signal thread to start checkpoint handshake
     */
    opal_cr_checkpoint_request   = OPAL_CR_STATUS_REQUESTED;

    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) signal_handler: Receive Checkpoint Request.");
}

/*
 * Respond to an asynchronous checkpoint request
 */
int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp)
{
    static int app_term = 0;
    static int cr_state;
    int app_pid;
    int ret, exit_status = ORTE_SUCCESS;

    if( opal_cr_currently_stalled ) {
        goto STAGE_1;
    }

    /*
     * Open communication channels
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) notify_response: Open Communication Channels.");
    if (ORTE_SUCCESS != (ret = snapc_full_app_notify_reopen_files())) {
        exit_status = ret;
        goto ckpt_cleanup;
    }

    /*
     * Initial Handshake
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) notify_response: Initial Handshake.");
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_start(&app_term, resp) ) ) {
        exit_status = ret;
        goto ckpt_cleanup;
    }

    /*
     * Begin checkpoint
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) notify_response: Start checkpoint...");

 STAGE_1:
    opal_cr_currently_stalled = false;

    app_pid = getpid();
    ret = opal_cr_inc_core(app_pid, local_snapshot, app_term, &cr_state);
    if( OPAL_EXISTS == ret ) {
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Stalling the checkpoint progress until state is stable again (PID = %d)\n",
                            getpid());
        opal_cr_currently_stalled = true;
        return exit_status;
    }
    else if(ORTE_SUCCESS != ret) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: checkpoint notification failed. %d\n", ret);
        goto ckpt_cleanup;
    }
    
    /* Don't stall any longer */
    opal_cr_stall_check = false;

    if(OPAL_CRS_RESTART == cr_state) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Restarting...(%d)\n",
                            getpid());
        
        app_term = false;
        /* Do not respond to the non-existent command line tool */
        goto ckpt_cleanup;
    }
    else if(cr_state == OPAL_CRS_CONTINUE) {
        ;  /* Don't need to do anything here */
    }
    else if(cr_state == OPAL_CRS_TERM ) {
        ; /* Don't need to do anything here */
    }
    else {
        opal_output_verbose(5, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Unknown cr_state(%d) [%d]",
                            cr_state, getpid());
    }
    
    /*
     * Final Handshake
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) notify_response: Final Handshake.");
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_end(cr_state ) ) ) {
        exit_status = ret;
        goto ckpt_cleanup;
    }

 ckpt_cleanup:
    close(app_comm_pipe_w_fd);
    close(app_comm_pipe_r_fd);
    remove(app_comm_pipe_r);
    remove(app_comm_pipe_w);
    
    if(app_term) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: User has asked to terminate the application");
        exit(ORTE_SUCCESS);
    }
        
    /* Prepare to wait for another checkpoint action */
    opal_cr_checkpointing      = OPAL_CR_STATUS_NONE;
    opal_cr_currently_stalled = false;

    return exit_status;
}

static int snapc_full_app_notify_reopen_files(void)
{
    int ret = OPAL_ERR_NOT_IMPLEMENTED;

#ifndef HAVE_MKFIFO
    return ret;
#else
#ifdef __WINDOWS__
    return ret;
#else
    /*
     * Open up the read pipe
     */
    if( (ret = mkfifo(app_comm_pipe_r, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                                "app) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                app_comm_pipe_r, ret);
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "app) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_r, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_r_fd = open(app_comm_pipe_r, O_RDWR);
    if(app_comm_pipe_r_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) init: Error: open failed to open the named pipe (%s). %d\n",
                    app_comm_pipe_r, app_comm_pipe_r_fd);
        return ORTE_ERROR;
    }

    /*
     * Open up the write pipe
     */
    if( (ret = mkfifo(app_comm_pipe_w, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                                "app) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                app_comm_pipe_w, ret);
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "app) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_w, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_w_fd = open(app_comm_pipe_w, O_WRONLY);
    if(app_comm_pipe_w_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_reopen_files: Error: open failed to open the named pipe (%s). (%d)\n",
                    app_comm_pipe_w, app_comm_pipe_w_fd);
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
#endif  /* __WINDOWS__ */
#endif  /* HAVE_MKFIFO */
}

static int snapc_full_app_ckpt_handshake_start(int *app_term, opal_cr_ckpt_cmd_state_t resp)
{
    int ret, exit_status = ORTE_SUCCESS;
    int len = 0, tmp_resp;
    char *tmp_str = NULL;
    ssize_t tmp_size = 0;

    /*
     * Get the initial handshake command: Term argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, app_term, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the term from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }

    tmp_resp = (int)resp;
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &tmp_resp, sizeof(int)) ) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: %d: Error: Unable to write to pipe (%s) ret = %d [Line %d]\n",
                    tmp_resp, app_comm_pipe_w, ret, __LINE__);
        goto cleanup;
    }
    
    /*
     * Respond that the checkpoint is currently in progress
     */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == resp ) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Checkpoint in progress, cannot start (%d)",
                            getpid());
        goto cleanup;
    }
    /*
     * Respond that the application is unable to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_NULL == resp ) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Non-checkpointable application, cannot start (%d)", 
                            getpid());
        goto cleanup;
    }
    /*
     * Respond that some error has occurred such that the application is 
     * not able to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_ERROR == resp ) {
        opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                            "app) notify_response: Error generated, cannot start (%d)", 
                            getpid());
        goto cleanup;
    }

    /*
     * Respond signalng that we wish to respond to this request
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "app) notify_response: Starting checkpoint request (%d)", 
                        getpid());

    /*
     * Get Snapshot Handle argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the snapshot_handle len from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    tmp_size = sizeof(char) * len;
    tmp_str  = (char *) malloc(sizeof(char) * len);
    if( tmp_size != (ret = read(app_comm_pipe_r_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the snapshot_handle from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    local_snapshot = OBJ_NEW(opal_crs_base_snapshot_t);

    if( 1 < strlen(tmp_str) ) {
        if( NULL != local_snapshot->reference_name)
            free( local_snapshot->reference_name );
        local_snapshot->reference_name = strdup(tmp_str);
        
        if( NULL != local_snapshot->local_location )
            free( local_snapshot->local_location );
        local_snapshot->local_location = opal_crs_base_get_snapshot_directory(local_snapshot->reference_name);
        
        if( NULL != local_snapshot->remote_location )
            free( local_snapshot->remote_location );
        local_snapshot->remote_location = strdup(local_snapshot->local_location);
    }
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    
    /*
     * Get Snapshot location argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the snapshot_location len from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    tmp_str = (char *) malloc(sizeof(char) * len);
    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = read(app_comm_pipe_r_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the snapshot_location from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    if( 1 < strlen(tmp_str) ) {
        if( NULL != local_snapshot->local_location)
            free( local_snapshot->local_location );
        asprintf(&(local_snapshot->local_location), "%s/%s", tmp_str, local_snapshot->reference_name);
        
        if( NULL != local_snapshot->remote_location)
            free( local_snapshot->remote_location );
        local_snapshot->remote_location = strdup(local_snapshot->local_location);
    }
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

 cleanup:
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    return exit_status;
}

static int snapc_full_app_ckpt_handshake_end(int cr_state)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;

    /*
     * Return the final checkpoint state to the local coordinator
     */
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &cr_state, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to write cr_state to named pipe (%s). %d\n",
                    app_comm_pipe_w, ret);
        goto cleanup;
    }

    /*
     * Wait for the local coordinator to release us
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "app) notify_response: Error: Unable to read the term from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }

 cleanup:
    return exit_status;
}
