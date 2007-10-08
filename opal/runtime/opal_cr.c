/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file 
 * 
 * OPAL Layer Checkpoint/Restart Runtime functions
 *
 */

#include "opal_config.h"

#include <errno.h>
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

#include "opal/class/opal_object.h"
#include "opal/util/opal_environ.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/if.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/opal_environ.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"

#include "opal/mca/base/base.h"
#include "opal/runtime/opal_cr.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/paffinity/base/base.h"

#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/mca/crs/base/base.h"
#include "opal/threads/condition.h"

/******************
 * Global Var Decls
 ******************/
bool opal_cr_allow_opal_only   = false;
bool opal_cr_stall_check       = false;
bool opal_cr_currently_stalled = false;
int  opal_cr_output;

/******************
 * Local Functions & Var Decls
 ******************/
static int cr_notify_response(opal_cr_ckpt_cmd_state_t resp);
static int extract_env_vars(int prev_pid);
static int cr_entry_point_notify_reopen_files(int *prog_read_fd, int *prog_write_fd);
static void opal_cr_entry_point_signal_handler (int signo);

static opal_cr_coord_callback_fn_t  cur_coord_callback = NULL;
static opal_cr_notify_callback_fn_t cur_notify_callback = NULL;

static char *prog_named_pipe_r = NULL;
static char *prog_named_pipe_w = NULL;

/******************
 * Interface Functions & Vars
 ******************/
char * opal_cr_pipe_dir   = NULL;
int    opal_cr_entry_point_signal     = 0;
bool   opal_cr_is_enabled = true;
bool   opal_cr_is_tool    = false;
/* Current checkpoint state */
int    opal_cr_checkpointing      = OPAL_CR_STATUS_NONE;
/* Current checkpoint request channel state */
int    opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;



int opal_cr_set_enabled(bool en)
{
    opal_cr_is_enabled = en;
    return OPAL_SUCCESS;
}

int opal_cr_initalized = 0;

int opal_cr_init(void )
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_cr_coord_callback_fn_t prev_coord_func;
    int val;

    if( ++opal_cr_initalized != 1 ) {
        if( opal_cr_initalized < 1 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    /*
     * Some startup MCA parameters
     */
    ret = mca_base_param_reg_int_name("opal_cr", "verbose",
                                      "Verbose output level for the runtime OPAL Checkpoint/Restart functionality",
                                      false, false,
                                      0,
                                      &val);
    if(0 != val) {
        opal_cr_output = opal_output_open(NULL);
    } else {
        opal_cr_output = -1;
    }
    opal_output_set_verbosity(opal_cr_output, val);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Verbose Level: %d",
                        val);

    mca_base_param_reg_int_name("ft", "cr_enabled",
                                "Enable fault tolerance for this program",
                                false, false,
                                0, &val);
    if(0 != val) {
        opal_cr_set_enabled(true);
    }
    else {
        opal_cr_set_enabled(false);
    }

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: FT Enabled: %d",
                        val);

    /*
     * Whether or not to allow OPAL only checkpointing.
     * By default we rely on ORTE to provide this functionality for us, but
     * if the application is OPAL only then we need to fallback to the signal
     * method which is activated by setting this MCA parameter to 'true'.
     */
    mca_base_param_reg_int_name("opal_cr", "allow_opal_only",
                                "Enable OPAL Only checkpointing [Default: Disabled]",
                                true, false,
                                0, &val);
    if(0 != val) {
        opal_cr_allow_opal_only = true;
    }
    else {
        opal_cr_allow_opal_only = false;
    }

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: OPAL CR Allow OPAL Only: %d",
                        val);
    mca_base_param_reg_int_name("opal_cr", "is_tool",
                                "Is this a tool program, meaning does it require a fully operational OPAL or just enough to exec.",
                                false, false,
                                false,
                                &val);
    if(!val) 
        opal_cr_is_tool = false;
    else
        opal_cr_is_tool = true;

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Is a tool program: %d",
                        val);
#ifndef __WINDOWS__
    mca_base_param_reg_int_name("opal_cr", "signal",
                                "Checkpoint/Restart signal used to initialize an OPAL Only checkpoint of a program",
                                false, false,
                                SIGUSR1,
                                &opal_cr_entry_point_signal);
#else
    opal_output( 0, "This feature is disabled on Windows" );
    return 0;
#endif  /* __WINDOWS__ */

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Checkpoint Signal: %d",
                        opal_cr_entry_point_signal);

    mca_base_param_reg_string_name("opal_cr", "tmp_dir",
                                   "Temporary directory to place rendezvous files for a checkpoint",
                                   false, false,
                                   "/tmp",
                                   &opal_cr_pipe_dir);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Temp Directory: %s",
                        opal_cr_pipe_dir);

    if( !opal_cr_is_tool ) {
        /* Register the OPAL interlevel coordination callback */
        opal_cr_reg_coord_callback(opal_cr_coord, &prev_coord_func);

        opal_cr_stall_check = false;
        opal_cr_currently_stalled = false;

        if( OPAL_SUCCESS != (ret = opal_cr_entry_point_init()) ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    } /* End opal_cr_is_tool = true */

    /* 
     * If fault tolerance was not compiled in then
     * we need to make sure that the listener thread is active to tell
     * the tools that this is not a checkpointable job.
     * We don't need the CRS framework to be initalized.
     */
#if OPAL_ENABLE_FT    == 1
    /*
     * Open the checkpoint / restart service components
     */
    if (OPAL_SUCCESS != (ret = opal_crs_base_open())) {
        opal_output(opal_cr_output,
                    "opal_cr: init: opal_crs_base_open Failed to open. (%d)\n", ret);
        goto cleanup;
    }
    
    if (OPAL_SUCCESS != (ret = opal_crs_base_select())) {
        opal_output(opal_cr_output,
                    "opal_cr: init: opal_crs_base_select Failed. (%d)\n", ret);
        goto cleanup;
    }
#endif

 cleanup:
    return exit_status;
}

int opal_cr_finalize(void)
{
    int ret, exit_status = OPAL_SUCCESS;

    if( --opal_cr_initalized != 0 ) {
        if( opal_cr_initalized < 0 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    if( !opal_cr_is_tool ) {
        if( OPAL_SUCCESS != (ret = opal_cr_entry_point_finalize()) ) {
            exit_status = ret;
        }

        /* Nothing to do for just process notifications */
        opal_cr_checkpointing      = OPAL_CR_STATUS_TERM;
        opal_cr_checkpoint_request = OPAL_CR_STATUS_TERM;
    }

#if OPAL_ENABLE_FT    == 1
    /*
     * Close the checkpoint / restart service components
     */
    opal_crs_base_close();
#endif

    return exit_status;
}

/*
 * Check if a checkpoint request needs to be operated upon
 */
void opal_cr_test_if_checkpoint_ready(void)
{
    int ret;

    if( opal_cr_currently_stalled) {
        opal_output_verbose(20, opal_cr_output,
                            "opal_cr:opal_test_if_ready: JUMPING to Post Stall stage");
        goto STAGE_1;
    }

    /*
     * If there is no checkpoint request to act on 
     * then just return
     */
    if(OPAL_CR_STATUS_REQUESTED != opal_cr_checkpoint_request ) {
        return;
    }

    /*
     * If we are currently checkpointing:
     *  - If a request is pending then cancel it
     *  - o.w., skip it.
     */
    if(OPAL_CR_STATUS_RUNNING == opal_cr_checkpointing ) {
        if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_IN_PROGRESS) ) ) {
            opal_output(opal_cr_output,
                        "Error: opal_cr: test_if_checkpoint_ready: Respond [In Progress] Failed. (%d)",
                        ret);
        }
        opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;
        return;
    }

    /*
     * If no CRS module is loaded return an error
     */
    if (NULL == opal_crs.crs_checkpoint ) {
         if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_NULL) ) ) {
             opal_output(opal_cr_output,
                         "Error: opal_cr: test_if_checkpoint_ready: Respond [Not Able/NULL] Failed. (%d)",
                         ret);
         }
         opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;
         return;
    }
   
    /* 
     * Start the checkpoint
     */
    opal_cr_checkpointing      = OPAL_CR_STATUS_RUNNING;
    opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;

 STAGE_1:
    if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_START) ) ) {
        opal_output(opal_cr_output,
                    "Error: opal_cr: test_if_checkpoint_ready: Respond [Start Ckpt] Failed. (%d)",
                    ret);
    }

    return;
}

/*******************************
 * Notification Routines
 *******************************/
int opal_cr_inc_core(pid_t pid, opal_crs_base_snapshot_t *snapshot, bool term, int *state)
{
    int ret, exit_status = OPAL_SUCCESS;
    int prev_pid = 0;

    prev_pid = getpid();

    /*
     * Use the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = cur_coord_callback(OPAL_CRS_CHECKPOINT)) ) {
        if ( OPAL_EXISTS != ret ) {
            opal_output(opal_cr_output, 
                        "opal_cr: inc_core: Error: cur_coord_callback(%d) failed! %d\n",
                        OPAL_CRS_CHECKPOINT, ret);
        }
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Take the checkpoint
     */
    if(OPAL_SUCCESS != (ret = opal_crs.crs_checkpoint(pid, snapshot, (opal_crs_state_type_t *)state))) {
        opal_output(opal_cr_output,
                    "opal_cr: inc_core: Error: The checkpoint failed. %d\n", ret);
        exit_status = ret;
        /* Don't return here since we want to restart the OPAL level stuff */
    }

    if(*state == OPAL_CRS_CONTINUE) {
        if(term)
            *state = OPAL_CRS_TERM;
    }
    else {
        term = false;
    }

    /*
     * If restarting read environment stuff that opal-restart left us.
     */
    if(*state == OPAL_CRS_RESTART) {
        extract_env_vars(prev_pid);
    }

    /*
     * Use the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = cur_coord_callback(*state)) ) {
        if ( OPAL_EXISTS != ret ) {
            opal_output(opal_cr_output,
                        "opal_cr: inc_core: Error: cur_coord_callback(%d) failed! %d\n",
                        *state, ret);
        }
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    return exit_status;
}


/*******************************
 * Coordination Routines
 *******************************/
/**
 * Current Coordination callback routines
 */
int opal_cr_coord(int state) 
{
    if(OPAL_CRS_CHECKPOINT == state) {
        /* Do Checkpoint Phase work */
    }
    else if (OPAL_CRS_CONTINUE == state ) {
        /* Do Continue Phase work */
    }
    else if (OPAL_CRS_RESTART == state ) {
        /* Do Restart Phase work */

        /*
         * Flush if() functionality, since it caches system specific info.
         */
        opal_iffinalize();
        /* Since opal_ifinit() is not exposed, the necessary
         * functions will call it when needed. Just make sure we
         * finalized this code so we don't get old socket addrs.
         */
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Do Continue Phase work in prep to terminate the application */
    }
    else {
        /* We must have been in an error state from the checkpoint
         * recreate everything, as in the Continue Phase
         */
    }

    /*
     * Here we are returning to either:
     *  - opal_notify()
     *    If we have an OPAL only opplication.
     *  - [orte | ompi]_notify()
     *    If we have an ORTE or OPAL application.
     */
    return OPAL_SUCCESS;
}

int opal_cr_reg_notify_callback(opal_cr_notify_callback_fn_t  new_func,
                                opal_cr_notify_callback_fn_t *prev_func)
{
    /*
     * Preserve the previous callback
     */
    if( NULL != cur_notify_callback) {
        *prev_func = cur_notify_callback;
    }
    else {
        *prev_func = NULL;
    }

    /*
     * Update the callbacks
     */
    cur_notify_callback     = new_func;

    return OPAL_SUCCESS;
}

int opal_cr_reg_coord_callback(opal_cr_coord_callback_fn_t  new_func,
                               opal_cr_coord_callback_fn_t *prev_func)
{
    /*
     * Preserve the previous callback
     */
    if( NULL != cur_coord_callback) {
        *prev_func = cur_coord_callback;
    }
    else {
        *prev_func = NULL;
    }

    /*
     * Update the callbacks
     */
    cur_coord_callback     = new_func;

    return OPAL_SUCCESS;
}

/*
 * Extract environment variables from a saved file
 * and place them in the environment.
 */
static int extract_env_vars(int prev_pid)
{
    int exit_status = OPAL_SUCCESS;
    char *file_name = NULL;
    FILE *env_data = NULL;
    int len = 128;
    char * tmp_str = NULL;

    if( 0 > prev_pid ) {
        opal_output(opal_cr_output,
                    "opal_cr: extract_env_vars: Invalid PID (%d)\n",
                    prev_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * JJH: Hardcode /tmp here, really only need an agreed upon file to 
     * transfer the environment variables.
     */
    asprintf(&file_name, "/tmp/%s-%d", OPAL_CR_BASE_ENV_NAME, prev_pid);

    if (NULL == (env_data = fopen(file_name, "r")) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Extract an env var */
    while(!feof(env_data) ) {
        char **t_set = NULL;
        len = 128;

        tmp_str = (char *) malloc(sizeof(char) * len);
        if( NULL == fgets(tmp_str, len, env_data) ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
        len = strlen(tmp_str);
        if(tmp_str[len - 1] == '\n')
            tmp_str[len - 1] = '\0';

        if( NULL == (t_set = opal_argv_split(tmp_str, '=')) ) {
            break;
        }
        
        opal_setenv(t_set[0], t_set[1], true, &environ);

        free(tmp_str);
        tmp_str = NULL;
    }

    
 cleanup:
    if( NULL != env_data ) {
        fclose(env_data);
    }
    unlink(file_name);

    if( NULL != file_name ){
        free(file_name);
    }

    if( NULL != tmp_str ){
        free(tmp_str);
    }
    
    return exit_status;
}

/*****************************************
 * OPAL CR Entry Point Functionality
*****************************************/
int opal_cr_entry_point_init(void)
{
    int exit_status = OPAL_SUCCESS;
    char *tmp_pid = NULL;
    opal_cr_notify_callback_fn_t prev_notify_func;

    if( !opal_cr_allow_opal_only ) {
        return OPAL_SUCCESS;
    }

    opal_cr_reg_notify_callback(cr_notify_response, &prev_notify_func);

    /* String representation of the PID */
    asprintf(&tmp_pid, "%d", getpid());

    asprintf(&prog_named_pipe_r, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
    asprintf(&prog_named_pipe_w, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);

    opal_output_verbose(15, opal_cr_output,
                        "opal_cr: init: Named Pipes (%s) (%s)", 
                        prog_named_pipe_r, prog_named_pipe_w);

    /*
     * Setup a signal handler to catch and start the proper thread
     * to handle the checkpoint
     */
    if( SIG_ERR == signal(opal_cr_entry_point_signal, opal_cr_entry_point_signal_handler) ) {
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

int opal_cr_entry_point_finalize(void)
{
    if( !opal_cr_allow_opal_only ) {
        return OPAL_SUCCESS;
    }

    if( NULL != prog_named_pipe_r) {
        free(prog_named_pipe_r);
        prog_named_pipe_r = NULL;
    }

    if( NULL != prog_named_pipe_w) {
        free(prog_named_pipe_w);
        prog_named_pipe_w = NULL;
    }

    return OPAL_SUCCESS;
}

/*
 * C/R Signal Handler.
 * Once a signal is received then the notification thread is notified
 * so it can communicate with the checkpoint command to take the approprate 
 * action.
 */
static void opal_cr_entry_point_signal_handler (int signo)
{
    if( opal_cr_entry_point_signal != signo ) {
        /* Not our signal */
        return;
    }
    /*
     * Signal thread to start checkpoint handshake
     */
    opal_cr_checkpoint_request   = OPAL_CR_STATUS_REQUESTED;
}

/*
 * Respond to an asynchronous checkpoint request
 */
int cr_notify_response(opal_cr_ckpt_cmd_state_t resp)
{
    static int app_term = 0, app_pid = 0;
    static opal_crs_base_snapshot_t *snapshot = NULL;
    static int prog_named_read_pipe_fd, prog_named_write_pipe_fd;
    static int len = 0;
    static int cr_state;
    int ret, exit_status = OPAL_SUCCESS;
    int tmp_resp;
    char *tmp_str = NULL;
    ssize_t tmp_size = 0;
    /* Commands from the command line tool */
    unsigned char app_cmd;

    if( opal_cr_currently_stalled ) {
        goto STAGE_1;
    }

    /*
     * Open a named pipe for our application
     */
    if (OPAL_SUCCESS != (ret = cr_entry_point_notify_reopen_files(&prog_named_read_pipe_fd, &prog_named_write_pipe_fd))) {
        goto ckpt_cleanup;
    }

    /*
     * Get the initial handshake command
     */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the first handshake from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }

    tmp_resp = (int)resp;
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &tmp_resp, sizeof(int)) ) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: %d: Error: Unable to write to pipe (%s) ret = %d [Line %d]\n",
                    tmp_resp, prog_named_pipe_w, ret, __LINE__);
        goto ckpt_cleanup;
    }
    
    /*
     * Respond that the checkpoint is currently in progress
     */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == resp ) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: cr_notify_response: Checkpoint in progress, cannot start (%d)",
                            getpid());
        goto ckpt_cleanup;
    }
    /*
     * Respond that the application is unable to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_NULL == resp ) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: cr_notify_response: Non-checkpointable application, cannot start (%d)", 
                            getpid());
        goto ckpt_cleanup;
    }
    /*
     * Respond that some error has occurred such that the application is 
     * not able to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_ERROR == resp ) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: cr_notify_response: Error generated, cannot start (%d)", 
                            getpid());
        goto ckpt_cleanup;
    }

    /*
     * Respond signalng that we wish to respond to this request
     */
    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: cr_notify_response: Starting checkpoint request (%d)", 
                        getpid());

    /*
     * Wait for a notify command from command line tool
     */
    if( sizeof(app_cmd) != (ret = read(prog_named_read_pipe_fd, &app_cmd, sizeof(app_cmd))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the requested command from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    /* get PID argument */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &app_pid, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the pid from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    /* get term argument */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &app_term, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the term from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    /* get Snapshot Handle argument */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the snapshot_handle len from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    tmp_size = sizeof(char) * len;
    tmp_str  = (char *) malloc(sizeof(char) * len);
    if( tmp_size != (ret = read(prog_named_read_pipe_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the snapshot_handle from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    snapshot = OBJ_NEW(opal_crs_base_snapshot_t);

    if( 1 < strlen(tmp_str) ) {
        if( NULL != snapshot->reference_name)
            free( snapshot->reference_name );
        snapshot->reference_name = strdup(tmp_str);
        
        if( NULL != snapshot->local_location )
            free( snapshot->local_location );
        snapshot->local_location = opal_crs_base_get_snapshot_directory(snapshot->reference_name);
        
        if( NULL != snapshot->remote_location )
            free( snapshot->remote_location );
        snapshot->remote_location = strdup(snapshot->local_location);
        
        free(tmp_str);
        tmp_str = NULL;
    }
    
    /* get Snapshot location argument */
    if( sizeof(int) != (ret = read(prog_named_read_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the snapshot_location len from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    tmp_str = (char *) malloc(sizeof(char) * len);
    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = read(prog_named_read_pipe_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to read the snapshot_location from named pipe (%s). %d\n",
                    prog_named_pipe_r, ret);
        goto ckpt_cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    if( 1 < strlen(tmp_str) ) {
        if( NULL != snapshot->local_location)
            free( snapshot->local_location );
        asprintf(&(snapshot->local_location), "%s/%s", tmp_str, snapshot->reference_name);
        
        if( NULL != snapshot->remote_location)
            free( snapshot->remote_location );
        snapshot->remote_location = strdup(snapshot->local_location);
        
        free(tmp_str);
        tmp_str = NULL;
    }
    
    /*
     * Raise the notification flag.
     * This will trigger the coordination, and checkpoint of the 
     * application if it is possible
     */
 STAGE_1:
    opal_cr_currently_stalled = false;

    ret = opal_cr_inc_core(app_pid, snapshot, app_term, &cr_state);
    if( OPAL_EXISTS == ret ) {
        opal_output_verbose(5, opal_cr_output,
                            "opal_cr: cr_notify_response: Stalling the checkpoint progress until state is stable again (PID = %d)\n",
                            getpid());
        opal_cr_currently_stalled = true;
        return exit_status;
    }
    else if(OPAL_SUCCESS != ret) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: checkpoint notification failed. %d\n", ret);
        goto ckpt_cleanup;
    }
    
    /* Don't stall any longer */
    opal_cr_stall_check = false;

    if(OPAL_CRS_RESTART == cr_state) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: cr_notify_response: Restarting...(%d)\n",
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
        opal_output_verbose(5, opal_cr_output,
                            "opal_cr: cr_notify_response: Unknown cr_state(%d) [%d]",
                            cr_state, getpid());
    }
    
    /*
     * Return the expected variables to the command line tool
     */
    len = strlen(snapshot->reference_name);
    len++; /* To account for the Null character */
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &len, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to write fname length to named pipe (%s). %d.\n",
                    prog_named_pipe_w, ret);
        goto ckpt_cleanup;
    }
    
    if(len > 0) {
        if( (ssize_t)(sizeof(char) * len) != 
            (ret = write(prog_named_write_pipe_fd, snapshot->reference_name, (sizeof(char) * len))) ) {
            opal_output(opal_cr_output,
                        "opal_cr: cr_notify_response: Error: Unable to write snapshot->reference_name to named pipe (%s). %d\n",
                        prog_named_pipe_w, ret);
            goto ckpt_cleanup;
        }
    }
    
    if( sizeof(int) != (ret = write(prog_named_write_pipe_fd, &cr_state, sizeof(int))) ) {
        opal_output(opal_cr_output,
                    "opal_cr: cr_notify_response: Error: Unable to write cr_state to named pipe (%s). %d\n",
                    prog_named_pipe_w, ret);
        goto ckpt_cleanup;
    }
    
 ckpt_cleanup:
    close(prog_named_write_pipe_fd);
    close(prog_named_read_pipe_fd);
    remove(prog_named_pipe_r);
    remove(prog_named_pipe_w);
    
    if(app_term) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: cr_notify_response: User has asked to terminate the application");
        exit(OPAL_SUCCESS);
    }
        
    /* Prepare to wait for another checkpoint action */
    opal_cr_checkpointing      = OPAL_CR_STATUS_NONE;

    opal_cr_currently_stalled = false;

    return exit_status;
}

static int cr_entry_point_notify_reopen_files(int *prog_read_fd, int *prog_write_fd)
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
    if( (ret = mkfifo(prog_named_pipe_r, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            opal_output_verbose(10, opal_cr_output,
                                "opal_cr: notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                prog_named_pipe_r, ret);
        }
        else {
            opal_output(opal_cr_output,
                        "opal_cr: notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        prog_named_pipe_r, ret);
            return OPAL_ERROR;
        }
    }
    
    *prog_read_fd = open(prog_named_pipe_r, O_RDWR);
    if(*prog_read_fd < 0) {
        opal_output(opal_cr_output,
                    "opal_cr: init: Error: open failed to open the named pipe (%s). %d\n",
                    prog_named_pipe_r, *prog_read_fd);
        return OPAL_ERROR;
    }

    /*
     * Open up the write pipe
     */
    if( (ret = mkfifo(prog_named_pipe_w, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            opal_output_verbose(10, opal_cr_output,
                                "opal_cr: notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                prog_named_pipe_w, ret);
        }
        else {
            opal_output(opal_cr_output,
                        "opal_cr: notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        prog_named_pipe_w, ret);
            return OPAL_ERROR;
        }
    }
    
    *prog_write_fd = open(prog_named_pipe_w, O_WRONLY);
    if(*prog_write_fd < 0) {
        opal_output(opal_cr_output,
                    "opal_cr: notify_reopen_files: Error: open failed to open the named pipe (%s). (%d)\n",
                    prog_named_pipe_w, *prog_write_fd);
        return OPAL_ERROR;
    }
    
    return OPAL_SUCCESS;
#endif  /* __WINDOWS__ */
#endif  /* HAVE_MKFIFO */
}
