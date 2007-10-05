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

/**
 * @file
 * ORTE Checkpoint Tool for checkpointing a multiprocess job
 *
 */

#include "orte_config.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
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
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */


#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "orte/orte_constants.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_cr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"
#include "orte/dss/dss.h"
#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

/******************
 * Local Functions
 ******************/
static int ckpt_init(int argc, char *argv[]); /* Initalization routine */
static int ckpt_finalize(void); /* Finalization routine */
static int parse_args(int argc, char *argv[]);
static int notify_process_for_checkpoint(char **global_snapshot_handle, int *seq_num, int term);
static int contact_hnp(orte_process_name_t *peer, char *global_snapshot_handle, int term);
static int wait_for_checkpoint(orte_process_name_t *peer, char **global_snapshot_handle, int *seq_num);
static int find_universe(void);
static int pretty_print_status(int state, char * snapshot_ref);
static int pretty_print_reference(int seq, char * snapshot_ref);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
enum orte_checkpoint_stage_t {
    ORTE_CKPT_STAGE_NULL,
    ORTE_CKPT_STAGE_INIT_OPAL_UTIL,
    ORTE_CKPT_STAGE_INIT_OPAL,
    ORTE_CKPT_STAGE_INIT_ORTE,
    ORTE_CKPT_STAGE_FINALIZE
    
};
typedef enum orte_checkpoint_stage_t orte_checkpoint_stage_t;

typedef struct {
    bool help;
    int  pid;
    bool term;
    bool verbose;
    char *req_universe_name; /**< User Requested Universe */
    int  stage;   /* Has completed init fully */
    bool nowait;  /* Do not wait for checkpoint to complete before returning */
    bool status;  /* Display status messages while checkpoint is progressing */
    int output;
    int ckpt_status;
} orte_checkpoint_globals_t;

orte_checkpoint_globals_t orte_checkpoint_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_checkpoint_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_checkpoint_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL, NULL, NULL, 
      '\0', NULL, "term", 
      0,
      &orte_checkpoint_globals.term, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the application after checkpoint" },

    { NULL, NULL, NULL, 
      'w', NULL, "nowait", 
      0,
      &orte_checkpoint_globals.nowait, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not wait for the application to finish checkpointing before returning" },

    { NULL, NULL, NULL, 
      's', NULL, "status", 
      0,
      &orte_checkpoint_globals.status, OPAL_CMD_LINE_TYPE_BOOL,
      "Display status messages describing the progression of the checkpoint" },

    { "universe", NULL, NULL,
      '\0', NULL, "universe", 
      1,
      &orte_checkpoint_globals.req_universe_name, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this "
      "application. This should be the universe of the application that you wish "
      "to checkpoint." },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;
    char *global_snapshot_handle;
    int seq_num = -1;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = ckpt_init(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*******************************
     * Checkpoint the requested PID
     *******************************/
    if( orte_checkpoint_globals.verbose ) {
        opal_output_verbose(10, orte_checkpoint_globals.output,
                            "orte_checkpoint: Checkpointing...");
        if(0 < orte_checkpoint_globals.pid) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t PID %d",
                                orte_checkpoint_globals.pid);
        }

        if(NULL != orte_checkpoint_globals.req_universe_name) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Universe (%s)",
                                orte_checkpoint_globals.req_universe_name);
        }
        else {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Connected to Universe (%s)",
                                orte_universe_info.name);
        }
        
        if(orte_checkpoint_globals.term) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Terminating after checkpoint\n");
        }
    }

    if(ORTE_SUCCESS != (ret = notify_process_for_checkpoint(&global_snapshot_handle,
                                                            &seq_num,
                                                            orte_checkpoint_globals.term)) ) {
        opal_show_help("help-orte-checkpoint.txt", "ckpt_failure", true,
                       orte_checkpoint_globals.pid, ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SNAPC_CKPT_STATE_ERROR == orte_checkpoint_globals.ckpt_status ) {
        opal_show_help("help-orte-checkpoint.txt", "ckpt_failure", true,
                       orte_checkpoint_globals.pid, ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( orte_checkpoint_globals.status ) {
        pretty_print_status(ORTE_SNAPC_CKPT_STATE_FINISHED, global_snapshot_handle);
    }

    if(!orte_checkpoint_globals.nowait) {
        pretty_print_reference(seq_num, global_snapshot_handle);
    }

 cleanup:
    /***************
     * Cleanup
     ***************/
    if (ORTE_SUCCESS != (ret = ckpt_finalize())) {
        return ret;
    }

    return exit_status;
}

static int parse_args(int argc, char *argv[]) {
    int i, ret, len, exit_status = ORTE_SUCCESS ;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;

    /* Init structure */
    memset(&orte_checkpoint_globals, 0, sizeof(orte_checkpoint_globals_t));
    orte_checkpoint_globals.help     = false;
    orte_checkpoint_globals.pid      = -1;
    orte_checkpoint_globals.term     = false;
    orte_checkpoint_globals.verbose  = false;
    orte_checkpoint_globals.req_universe_name = NULL;
    orte_checkpoint_globals.stage    = ORTE_CKPT_STAGE_NULL;
    orte_checkpoint_globals.nowait   = false;
    orte_checkpoint_globals.status   = false;
    orte_checkpoint_globals.output   = -1;
    orte_checkpoint_globals.ckpt_status = ORTE_SNAPC_CKPT_STATE_NONE;

    /* Parse the command line options */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    
    /** 
     * Put all of the MCA arguments in the environment 
     */
    mca_base_cmd_line_process_args(&cmd_line, &app_env, &global_env);

    len = opal_argv_count(app_env);
    for(i = 0; i < len; ++i) {
        putenv(app_env[i]);
    }

    len = opal_argv_count(global_env);
    for(i = 0; i < len; ++i) {
        putenv(global_env[i]);
    }

    opal_setenv(mca_base_param_env_var("opal_cr_is_tool"), 
                "1",
                true, &environ);

    /**
     * Now start parsing our specific arguments
     */
    /* get the remaining bits */
    opal_cmd_line_get_tail(&cmd_line, &argc, &argv);

    if (OPAL_SUCCESS != ret || 
        orte_checkpoint_globals.help ||
        (0 >= argc && NULL == orte_checkpoint_globals.req_universe_name)) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-checkpoint.txt", "usage", true,
                       args);
        free(args);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * If the user did not supply a universe, then they must 
     *  supply the PID of MPIRUN
     */
    if(0 >= argc && 
       NULL != orte_checkpoint_globals.req_universe_name) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    orte_checkpoint_globals.pid = atoi(argv[0]);
    if ( 0 >= orte_checkpoint_globals.pid ) {
        opal_show_help("help-orte-checkpoint.txt", "invalid_pid", true,
                       orte_checkpoint_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * JJH: No wait is currently not implemented or tested
     */
    if(orte_checkpoint_globals.nowait) {
        orte_checkpoint_globals.nowait = false;
        opal_show_help("help-orte-checkpoint.txt", "not_impl",
                       true,
                       "Disconnected checkpoint");
    }

 cleanup:
    return exit_status;
}

static int 
notify_process_for_checkpoint(char **global_snapshot_handle, int *seq_num, int term)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_process_name_t peer;

    peer = *ORTE_PROC_MY_HNP;

    /*
     * Contact HNP via RML
     * The notification will be received by the Global Snapshot Coordinator [SnapC]
     *  in the HNP(s)
     * See orte_snapc(7) for more information.
     */
    if( ORTE_SUCCESS != (ret = contact_hnp(&peer, *global_snapshot_handle, term)) ) {
        opal_show_help("help-orte-checkpoint.txt", "unable_to_connect", true,
                       orte_checkpoint_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * Wait for the global_snapshot_coordinator to notify us (via the RML) 
     *  of the completion of the checkpoint.
     * Unless the user wants us to return immediately
     */
    if(!orte_checkpoint_globals.nowait) {
        /*
         * Wait for progress updates, stop waiting when 'Finished' status
         */
        do {
            if( ORTE_SUCCESS != (ret = wait_for_checkpoint(&peer, global_snapshot_handle, seq_num) ) ) {
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            /*
             * If process said that it cannot checkpoint at this time return a
             * pretty message.
             */
            if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == orte_checkpoint_globals.ckpt_status ) {
                opal_show_help("help-orte-checkpoint.txt", "non-ckptable", 
                               true,
                               orte_checkpoint_globals.pid);
                exit_status = ORTE_ERROR;
                break;
            }
            /*
             * If we are to display the status progression
             */
            if( orte_checkpoint_globals.status ) {
                if(ORTE_SNAPC_CKPT_STATE_FINISHED != orte_checkpoint_globals.ckpt_status)
                    pretty_print_status(orte_checkpoint_globals.ckpt_status, *global_snapshot_handle);
            }
            /*
             * Otherwise only display it if we are going to be terminated soon
             */
            else {
                /* Since ORTE kills us before we get the Finished message,
                 * print out the global snapshot handle when we start running
                 */
                if(orte_checkpoint_globals.term && 
                   ORTE_SNAPC_CKPT_STATE_RUNNING == orte_checkpoint_globals.ckpt_status ) {
                    pretty_print_status(orte_checkpoint_globals.ckpt_status, *global_snapshot_handle);
                }
            }
        } while(ORTE_SNAPC_CKPT_STATE_FINISHED != orte_checkpoint_globals.ckpt_status &&
                ORTE_SNAPC_CKPT_STATE_ERROR    != orte_checkpoint_globals.ckpt_status );
    }

 cleanup:
    return exit_status;
}

/*
 * This function attempts to:
 *  1. Find the universe that matches one or both of the following:
 *     - --universe specified by the user (if any)
 *     - PID specified by the user (if any)
 *  2. Attach orte_checkpoint to that universe, so we can talk to 
 *     it's GPR.
 */
static int find_universe(void) {
    int ret, exit_status = ORTE_SUCCESS;
    char *fulldirpath = NULL,
        *prefix       = NULL,
        *frontend     = NULL,
        *univ_name    = NULL,
        *full_univ    = NULL;

    /*
     * If the user specified a universe, trust it as correct
     */
    if( NULL != orte_checkpoint_globals.req_universe_name ) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /*
     * Otherwise check to see if we can find it by looking for the 
     * default constructed with the PID
     */
    asprintf(&univ_name, "%s-%d", ORTE_DEFAULT_UNIVERSE, orte_checkpoint_globals.pid);

    if( ORTE_SUCCESS != (ret = orte_session_dir_get_name(&fulldirpath,
                                                         &prefix,
                                                         &frontend,
                                                         orte_system_info.user, 
                                                         orte_system_info.nodename,
                                                         NULL, /* Unknown batchid */ 
                                                         univ_name,
                                                         NULL, /* Unknown Jobid */ 
                                                         NULL) /* Unknown process ID */
                         )) {
        exit_status = ret;
        goto cleanup;
    }
    
    opal_output_verbose(10, orte_checkpoint_globals.output,
                        "orte_checkpoint: find_universe: Trying to find the session directory\n\t\t(%s)\n",
                        fulldirpath);

    /*
     * Check that the directory is accessable.
     */
    if( ORTE_SUCCESS != (ret = opal_os_dirpath_access(fulldirpath, 0) )) {
        opal_show_help("help-orte-checkpoint.txt", "no_universe", true,
                       orte_checkpoint_globals.pid, fulldirpath);
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Set the MCA parameter 
     * username@hostname:universe_name
     */
    asprintf(&full_univ, "%s@%s:%s", 
             orte_system_info.user,
             orte_system_info.nodename,
             univ_name);
    opal_setenv(mca_base_param_env_var("universe"),
                full_univ,
                true, &environ);

 cleanup:
    if(NULL != full_univ) 
        free(full_univ);
    if(NULL != fulldirpath)
        free(fulldirpath);
    if(NULL != prefix)
        free(prefix);
    if(NULL != frontend)
        free(frontend);
    if(NULL != univ_name)
        free(univ_name);

    return exit_status;
}

static int ckpt_init(int argc, char *argv[]) {
    int exit_status = ORTE_SUCCESS, ret;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util()) ) {
        return ret;
    }
    orte_checkpoint_globals.stage    = ORTE_CKPT_STAGE_INIT_OPAL_UTIL;

    /*
     * Parse Command Line Arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( orte_checkpoint_globals.verbose ) {
        orte_checkpoint_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_checkpoint_globals.output, 10);
    } else {
        orte_checkpoint_globals.output = 0; /* Default=STDOUT */
    }

    /*
     * We are trying to attach to another process' GPR so we need to 
     * attach no matter if it is identified as private or not.
     */
    opal_setenv(mca_base_param_env_var("universe_console"),
                "1",
                true, &environ);

    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);

    /* Select the none component, since we don't actually use a checkpointer */
    opal_setenv(mca_base_param_env_var("crs"),
                "none",
                true, &environ);
    
    /***************************
     * We need all of OPAL
     ***************************/
    if (ORTE_SUCCESS != (ret = opal_init())) {
        exit_status = ret;
        goto cleanup;
    }
    orte_checkpoint_globals.stage    = ORTE_CKPT_STAGE_INIT_OPAL; 

    /***************************
     * And ORTE, but need to do a bit of a dance first
     ***************************/
    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params(true))) {
        exit_status = ret;
        goto cleanup;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        exit_status = ret;
        goto cleanup;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        exit_status = ret;
        goto cleanup;
    }

    /***************************
     * Find the universe that we need to connect to, if it exists
     ***************************/
    if (ORTE_SUCCESS != (ret = find_universe())) {
        exit_status = ret;
        goto cleanup;

    }

    /* JJH XXX
     * JJH XXX In actuality, we only want to setup upto the RML
     * JJH XXX so we can talk to the HNP over the wire, but don't
     * JJH XXX become a job of the universe.
     * JJH XXX This is a bandaid until we do it right.
     * JJH XXX
     */
    if (ORTE_SUCCESS != (ret = orte_init(ORTE_INFRASTRUCTURE)) ) {
        exit_status = ret;
        goto cleanup;
    }

    orte_checkpoint_globals.stage    = ORTE_CKPT_STAGE_INIT_ORTE; 

 cleanup:
    return exit_status;
}

static int ckpt_finalize(void) {
    int exit_status = ORTE_SUCCESS, ret;

    if( ORTE_CKPT_STAGE_INIT_OPAL_UTIL == orte_checkpoint_globals.stage) {
        if (ORTE_SUCCESS != (ret = opal_finalize_util())) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else if( ORTE_CKPT_STAGE_INIT_OPAL == orte_checkpoint_globals.stage) {
        if (ORTE_SUCCESS != (ret = opal_finalize())) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        if (ORTE_SUCCESS != (ret = orte_finalize())) {
            exit_status = ret;
            goto cleanup;
        }
    }
    
 cleanup:
    return exit_status;
}

static int contact_hnp(orte_process_name_t *peer, char *global_snapshot_handle, int term) {
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *buffer;
    size_t command;
    pid_t hnp_pid;
    orte_std_cntr_t n;
    /* JJH XXX currently we assume jobid = 1, don't do this in the future */
    orte_jobid_t jobid = 1;

    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /***********************************
     * Notify HNP that we want to chat about a checkpoint
     ***********************************/
    command = ORTE_SNAPC_GLOBAL_INIT_CMD;
    if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, &command, 1, ORTE_CKPT_CMD)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if ( 0 > (ret = orte_rml.send_buffer(peer, buffer, ORTE_RML_TAG_CKPT, 0)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /********************
     * Receive the PID of the HNP, just to be doubly sure we are talking to 
     *  the right HNP.
     ********************/
    if( 0 > (ret = orte_rml.recv_buffer(peer, buffer, ORTE_RML_TAG_CKPT, 0)) ) {
        exit_status = ret;
        goto cleanup;
    }

    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(buffer, &hnp_pid, &n, ORTE_PID)) ) {
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(10, orte_checkpoint_globals.output,
                        "orte_checkpoint: contact_hnp: Head Node Process PID = %d\n",
                        hnp_pid);
    opal_output_verbose(10, orte_checkpoint_globals.output,
                        "orte_checkpoint: contact_hnp: Expected          PID = %d\n",
                        orte_checkpoint_globals.pid);
    
    if(hnp_pid != orte_checkpoint_globals.pid) {
        opal_show_help("help-orte-checkpoint.txt", "invalid_pid", true,
                       orte_checkpoint_globals.pid);
        exit_status = ORTE_ERROR;
        orte_snapc_base_global_coord_send_ack(peer, false);
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_send_ack(peer, true)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /**************************
     * Send over the term flag
     **************************/
    OBJ_RELEASE(buffer);
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, &term, 1, ORTE_BOOL))) {
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, buffer, ORTE_RML_TAG_CKPT, 0))) {
        exit_status = ret;
        goto cleanup;
    }

    /**************************
     * Send over the jobid flag
     **************************/
    OBJ_RELEASE(buffer);
    if (NULL == (buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_dss.pack(buffer, &jobid, 1, ORTE_SIZE))) {
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, buffer, ORTE_RML_TAG_CKPT, 0))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_RELEASE(buffer);

    return exit_status;
}

static int wait_for_checkpoint(orte_process_name_t *peer, char **global_snapshot_handle, int *seq_num) {
    int ret, exit_status = ORTE_SUCCESS;
    orte_buffer_t *loc_buffer;
    orte_std_cntr_t n    = 1;
    size_t str_len = 0;
    int ckpt_status = ORTE_SNAPC_CKPT_STATE_NONE;

    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /******************
     * Receive the checkpoint status
     ******************/
    OBJ_RELEASE(loc_buffer);
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, &ckpt_status, &n, ORTE_INT)) ) {
        exit_status = ret;
        goto cleanup;
    }

    orte_checkpoint_globals.ckpt_status = ckpt_status;

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_send_ack(peer, true)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /* If we cannot checkpoint, then just skip to the end */
    if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == ckpt_status) {
        *global_snapshot_handle = NULL;
        goto cleanup;
    }

    /******************
     * Receive the size of the global snapshot handle
     ******************/
    OBJ_RELEASE(loc_buffer);
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, &str_len, &n, ORTE_SIZE)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_send_ack(peer, true)) ) {
        exit_status = ret;
        goto cleanup;
    }
    
    /******************
     * Receive the global snapshot handle
     ******************/
    OBJ_RELEASE(loc_buffer);
    if (NULL == (loc_buffer = OBJ_NEW(orte_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_rml.recv_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, global_snapshot_handle, &n, ORTE_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }
    n = 1;
    if ( ORTE_SUCCESS != (ret = orte_dss.unpack(loc_buffer, seq_num, &n, ORTE_INT)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /* ACK */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_send_ack(peer, true)) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_RELEASE(loc_buffer);

    return exit_status;
}

static int pretty_print_status(int state, char * snapshot_ref) {
    char * state_str = NULL;

    state_str = orte_snapc_ckpt_state_str(state);

    opal_output(orte_checkpoint_globals.output,
                "%*s - Global Snapshot Reference: %s\n", 
                25, state_str, snapshot_ref);
    if( NULL != state_str) {
        free(state_str);
    }
    
    return ORTE_SUCCESS;
}

static int pretty_print_reference(int seq, char * snapshot_ref) {

    printf("Snapshot Ref.: %3d %s\n",
           seq, snapshot_ref);
    
    return ORTE_SUCCESS;
}
