/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
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
#include "orte/constants.h"

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
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
#include <string.h>


#include "opal/util/cmd_line.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_cr.h"
#include "orte/util/hnp_contact.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "opal/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/dss/dss.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"

#include MCA_timer_IMPLEMENTATION_HEADER

/******************
 * Local Functions
 ******************/
static int ckpt_init(int argc, char *argv[]); /* Initalization routine */
static int ckpt_finalize(void); /* Finalization routine */
static int parse_args(int argc, char *argv[]);
static int find_hnp(void);

static int start_listener(void);
static int stop_listener(void);
static void hnp_receiver(int status,
                         orte_process_name_t* sender,
                         opal_buffer_t* buffer,
                         orte_rml_tag_t tag,
                         void* cbdata);

static void process_ckpt_update_cmd(orte_process_name_t* sender,
                                    opal_buffer_t* buffer);

static int notify_process_for_checkpoint(opal_crs_base_ckpt_options_t *options);
static int pretty_print_status(void);
static int pretty_print_reference(void);

static int list_all_snapshots(void);

static orte_hnp_contact_t *orterun_hnp = NULL;
static char * global_snapshot_handle = NULL;
static int    global_sequence_num    = 0;

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
static bool listener_started = false;
static bool is_checkpoint_finished = false;
static bool is_checkpoint_established = false;
static bool is_checkpoint_recovered   = false;

static double timer_start = 0;
static double timer_last  = 0;
static double get_time(void);

typedef struct {
    bool help;
    int  pid;
    opal_crs_base_ckpt_options_t *options;
    bool term;
    bool stop;
    bool verbose;
    int  verbose_level;
    orte_jobid_t req_hnp; /**< User Requested HNP */
    bool nowait;  /* Do not wait for checkpoint to complete before returning */
    bool status;  /* Display status messages while checkpoint is progressing */
    int output;
    int ckpt_status;
    bool list_only; /* List available checkpoints only */
#if OPAL_ENABLE_CRDEBUG == 1
    bool enable_crdebug; /* Enable C/R Debugging */
    bool attach_debugger;
    bool detach_debugger;
#endif
} orte_checkpoint_globals_t;

orte_checkpoint_globals_t orte_checkpoint_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help",
      0,
      &orte_checkpoint_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      'v', NULL, "verbose",
      0,
      &orte_checkpoint_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL,
      'V', NULL, NULL,
      1,
      &orte_checkpoint_globals.verbose_level, OPAL_CMD_LINE_TYPE_INT,
      "Set the verbosity level (For additional debugging information)" },

    { NULL,
      '\0', NULL, "term",
      0,
      &(orte_checkpoint_globals.term), OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the application after checkpoint (Cannot be used with --stop)" },

    { NULL,
      '\0', NULL, "stop",
      0,
      &(orte_checkpoint_globals.stop), OPAL_CMD_LINE_TYPE_BOOL,
      "Send SIGSTOP to application just after checkpoint (checkpoint will not finish until SIGCONT is sent) (Cannot be used with --term)" },

    { NULL,
      'w', NULL, "nowait",
      0,
      &orte_checkpoint_globals.nowait, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not wait for the application to finish checkpointing before returning" },

    { NULL,
      's', NULL, "status",
      0,
      &orte_checkpoint_globals.status, OPAL_CMD_LINE_TYPE_BOOL,
      "Display status messages describing the progression of the checkpoint" },

    { "hnp-jobid",
      '\0', NULL, "hnp-jobid",
      1,
      &orte_checkpoint_globals.req_hnp, OPAL_CMD_LINE_TYPE_INT,
      "This should be the jobid of the HNP whose applications you wish "
      "to checkpoint." },

    { "hnp-pid",
      '\0', NULL, "hnp-pid",
      1,
      &orte_checkpoint_globals.pid, OPAL_CMD_LINE_TYPE_INT,
      "This should be the pid of the mpirun whose applications you wish "
      "to checkpoint." },

    { NULL,
      'l', NULL, "list",
      0,
      &orte_checkpoint_globals.list_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a list of checkpoint files available on this machine" },

#if OPAL_ENABLE_CRDEBUG == 1
    { NULL,
      '\0', "crdebug", "crdebug",
      0,
      &orte_checkpoint_globals.enable_crdebug, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable C/R Enhanced Debugging" },

    { NULL,
      '\0', "attach", "attach",
      0,
      &(orte_checkpoint_globals.attach_debugger), OPAL_CMD_LINE_TYPE_BOOL,
      "Wait for the debugger to attach directly after taking the checkpoint." },

    { NULL,
      '\0', "detach", "detach",
      0,
      &(orte_checkpoint_globals.detach_debugger), OPAL_CMD_LINE_TYPE_BOOL,
      "Do not wait for the debugger to reattach after taking the checkpoint." },
#endif

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = ckpt_init(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*************************************
     * Listing only Checkpoint References
     *************************************/
    if( orte_checkpoint_globals.list_only ) {
        if (ORTE_SUCCESS != (ret = list_all_snapshots())) {
            exit_status = ret;
            goto cleanup;
        }
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /***************************
     * Find the HNP that we want to connect to, if it exists
     ***************************/
    if (ORTE_SUCCESS != (ret = find_hnp())) {
        /* Error printed by called function */
        exit_status = ret;
        goto cleanup;
    }

    /*******************************
     * Checkpoint the requested PID
     *******************************/
    is_checkpoint_finished    = false;
    is_checkpoint_recovered   = false;
    is_checkpoint_established = false;

    if( orte_checkpoint_globals.verbose ) {
        opal_output_verbose(10, orte_checkpoint_globals.output,
                            "orte_checkpoint: Checkpointing...");
        if (0 < orte_checkpoint_globals.pid) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t PID %d",
                                orte_checkpoint_globals.pid);
        } else if (ORTE_JOBID_INVALID != orte_checkpoint_globals.req_hnp){
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Mpirun (%s)",
                                ORTE_JOBID_PRINT(orte_checkpoint_globals.req_hnp));
        }

        opal_output_verbose(10, orte_checkpoint_globals.output,
                            "\t Connected to Mpirun %s",
                            ORTE_NAME_PRINT(&orterun_hnp->name));

        if(orte_checkpoint_globals.options->term) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Terminating after checkpoint\n");
        }
        if(orte_checkpoint_globals.options->stop) {
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "\t Stopping after checkpoint\n");
        }
    }

    if(ORTE_SUCCESS != (ret = notify_process_for_checkpoint( orte_checkpoint_globals.options)) ) {
        opal_show_help("help-orte-checkpoint.txt", "ckpt_failure", true,
                       orte_checkpoint_globals.pid, ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait for the checkpoint to complete
     */
    if(!orte_checkpoint_globals.nowait) {
        while( !is_checkpoint_finished ) {
            opal_progress();
        }
    }

    if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == orte_checkpoint_globals.ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_ERROR   == orte_checkpoint_globals.ckpt_status ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if(!orte_checkpoint_globals.nowait) {
        pretty_print_reference();
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
    char * tmp_env_var = NULL;
    char *argv0 = NULL;

    /* Init structure */
    memset(&orte_checkpoint_globals, 0, sizeof(orte_checkpoint_globals_t));
    orte_checkpoint_globals.help     = false;
    orte_checkpoint_globals.pid      = -1;
    orte_checkpoint_globals.verbose  = false;
    orte_checkpoint_globals.verbose_level  = 0;
    orte_checkpoint_globals.req_hnp  = ORTE_JOBID_INVALID;
    orte_checkpoint_globals.nowait   = false;
    orte_checkpoint_globals.status   = false;
    orte_checkpoint_globals.output   = -1;
    orte_checkpoint_globals.ckpt_status = ORTE_SNAPC_CKPT_STATE_NONE;
    orte_checkpoint_globals.list_only  = false;
#if OPAL_ENABLE_CRDEBUG == 1
    orte_checkpoint_globals.enable_crdebug = false;
#endif

    orte_checkpoint_globals.options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    orte_checkpoint_globals.term     = false;
    orte_checkpoint_globals.stop     = false;
#if OPAL_ENABLE_CRDEBUG == 1
    orte_checkpoint_globals.attach_debugger = false;
    orte_checkpoint_globals.detach_debugger = false;
#endif

#if OPAL_ENABLE_FT_CR == 0
    /* Warn and exit if not configured with Checkpoint/Restart */
    {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-checkpoint.txt", "usage-no-cr",
                                    true, args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
#endif

    /* Parse the command line options */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, false, argc, argv);

    if (OPAL_SUCCESS != ret) {
        if (OPAL_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(ret));
        }
        exit_status = 1;
        goto cleanup;
    }

    if (orte_checkpoint_globals.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-checkpoint.txt", "usage", true,
                                    args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        /* If we show the help message, that should be all we do */
        exit(0);
    }

    /**
     * Put all of the MCA arguments in the environment
     */
    mca_base_cmd_line_process_args(argc, &app_env, &global_env);

    len = opal_argv_count(app_env);
    for(i = 0; i < len; ++i) {
        putenv(app_env[i]);
    }

    len = opal_argv_count(global_env);
    for(i = 0; i < len; ++i) {
        putenv(global_env[i]);
    }

    (void) mca_base_var_env_name("opal_cr_is_tool", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /**
     * Now start parsing our specific arguments
     */
    /* get the remaining bits */
    argv0 = strdup(argv[0]);
    opal_cmd_line_get_tail(&cmd_line, &argc, &argv);

    if(orte_checkpoint_globals.list_only ) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    if (0 >= argc && ORTE_JOBID_INVALID == orte_checkpoint_globals.req_hnp) {
        fprintf(stderr, "%s: Nothing to do\n", argv0);
        fprintf(stderr, "Type '%s --help' for usage.\n", argv0);
        exit_status = 1;
        goto cleanup;
    }

    orte_checkpoint_globals.options->term = orte_checkpoint_globals.term;
    orte_checkpoint_globals.options->stop = orte_checkpoint_globals.stop;
#if OPAL_ENABLE_CRDEBUG == 1
    orte_checkpoint_globals.options->attach_debugger = orte_checkpoint_globals.attach_debugger;
    orte_checkpoint_globals.options->detach_debugger = orte_checkpoint_globals.detach_debugger;
#endif

    if(orte_checkpoint_globals.verbose_level < 0 ) {
        orte_checkpoint_globals.verbose_level = 0;
    }

    if(orte_checkpoint_globals.verbose_level > 0) {
        orte_checkpoint_globals.verbose = true;
    }

    /*
     * If the user did not supply an hnp jobid, then they must
     *  supply the PID of MPIRUN
     */
    if(0 >= argc &&
       ORTE_JOBID_INVALID != orte_checkpoint_globals.req_hnp) {
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

    if(orte_checkpoint_globals.verbose) {
        orte_checkpoint_globals.status = true;
    }

 cleanup:
    if (NULL != argv0) {
        free(argv0);
    }

    return exit_status;
}

/*
 * This function attempts to find an HNP to connect to.
 */
static int find_hnp(void) {
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_t hnp_list;
    opal_list_item_t *item;
    orte_hnp_contact_t *hnpcandidate;

    /* get the list of local hnp's available to us and setup
     * contact info for them into the RML
     */
    OBJ_CONSTRUCT(&hnp_list, opal_list_t);
    if (ORTE_SUCCESS != (ret = orte_list_local_hnps(&hnp_list, true) ) ) {
        opal_show_help("help-orte-checkpoint.txt", "no_hnps", true,
                       orte_checkpoint_globals.pid,
                       orte_process_info.tmpdir_base,
                       orte_process_info.top_session_dir,
                       ret, ORTE_ERROR_NAME(ret));
        exit_status = ret;
        goto cleanup;
    }

    /* search the list for the desired hnp */
    while (NULL != (item = opal_list_remove_first(&hnp_list))) {
        hnpcandidate = (orte_hnp_contact_t*)item;
        if (hnpcandidate->name.jobid == orte_checkpoint_globals.req_hnp ||
            hnpcandidate->pid        == orte_checkpoint_globals.pid) {
            /* this is the one we want */
            orterun_hnp = hnpcandidate;
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }
    }

    /* If no match was found, error out */
    opal_show_help("help-orte-checkpoint.txt", "no_universe", true,
                   orte_checkpoint_globals.pid,
                   orte_process_info.tmpdir_base,
                   orte_process_info.top_session_dir);

cleanup:
    while (NULL != (item = opal_list_remove_first(&hnp_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&hnp_list);

    if( NULL == orterun_hnp ) {
        return ORTE_ERROR;
    } else {
        return exit_status;
    }
}

static int ckpt_init(int argc, char *argv[]) {
    int exit_status = ORTE_SUCCESS, ret;
    char * tmp_env_var = NULL;

    listener_started = false;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util(&argc, &argv)) ) {
        return ret;
    }

    /*
     * Parse Command Line Arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }

    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);

    /* Select the none component, since we don't actually use a checkpointer */
    (void) mca_base_var_env_name("crs", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /* we are never allowed to operate as a distributed tool,
     * so insist on the ess/tool component */
    opal_setenv("OMPI_MCA_ess", "tool", true, &environ);

    /***************************
     * We need all of OPAL and the TOOLS portion of ORTE - this
     * sets us up so we can talk to any HNP over the wire
     ***************************/
    if (ORTE_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup ORTE Output handle from the verbose argument
     */
    if( orte_checkpoint_globals.verbose ) {
        orte_checkpoint_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_checkpoint_globals.output, orte_checkpoint_globals.verbose_level);
    } else {
        orte_checkpoint_globals.output = 0; /* Default=STDERR */
    }

    /*
     * Start the listener
     */
    if( ORTE_SUCCESS != (ret = start_listener() ) ) {
        exit_status = ret;
    }

 cleanup:
    return exit_status;
}

static int ckpt_finalize(void) {
    int exit_status = ORTE_SUCCESS, ret;

    /*
     * Stop the listener
     */
    if( ORTE_SUCCESS != (ret = stop_listener() ) ) {
        exit_status = ret;
    }

    if (ORTE_SUCCESS != (ret = orte_finalize())) {
        exit_status = ret;
    }

    return exit_status;
}

static int start_listener(void)
{
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_CKPT,
                            ORTE_RML_PERSISTENT, hnp_receiver, NULL);

    listener_started = true;
    return ORTE_SUCCESS;
}

static int stop_listener(void)
{
    if( !listener_started ) {
        return ORTE_ERROR;
    }

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_CKPT);

    listener_started = false;
    return ORTE_SUCCESS;
}

static void hnp_receiver(int status,
                         orte_process_name_t* sender,
                         opal_buffer_t* buffer,
                         orte_rml_tag_t tag,
                         void* cbdata)
{
    orte_snapc_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    opal_output_verbose(5, orte_checkpoint_globals.output,
                        "orte_checkpoint: hnp_receiver: Receive a command message.");

    /*
     * Otherwise this is an inter-coordinator command (usually updating state info).
     */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    switch (command) {
        case ORTE_SNAPC_GLOBAL_UPDATE_CMD:
            opal_output_verbose(10, orte_checkpoint_globals.output,
                                "orte_checkpoint: hnp_receiver: Status Update.");

            process_ckpt_update_cmd(sender, buffer);
            break;

        case ORTE_SNAPC_GLOBAL_INIT_CMD:
        case ORTE_SNAPC_GLOBAL_TERM_CMD:
            /* Do Nothing */
            break;
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
}

static void process_ckpt_update_cmd(orte_process_name_t* sender,
                                    opal_buffer_t* buffer)
{
    int ret;
    orte_std_cntr_t count = 1;
    int ckpt_status = ORTE_SNAPC_CKPT_STATE_NONE;

    /*
     * Receive the data:
     * - ckpt_state
     * - global snapshot handle (upon finish only)
     * - sequence number        (upon finish only)
     */
    count = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &ckpt_status, &count, OPAL_INT)) ) {
        return;
    }
    orte_checkpoint_globals.ckpt_status = ckpt_status;

    if( ORTE_SNAPC_CKPT_STATE_RECOVERED   == orte_checkpoint_globals.ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_ESTABLISHED == orte_checkpoint_globals.ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_STOPPED     == orte_checkpoint_globals.ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_ERROR       == orte_checkpoint_globals.ckpt_status ) {
        count = 1;
        if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &global_snapshot_handle, &count, OPAL_STRING)) ) {
            return;
        }
        count = 1;
        if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &global_sequence_num, &count, OPAL_INT)) ) {
            return;
        }
    }

    /*
     * If the job is not able to be checkpointed, then return
     */
    if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == orte_checkpoint_globals.ckpt_status) {
        opal_show_help("help-orte-checkpoint.txt", "non-ckptable",
                       true,
                       orte_checkpoint_globals.pid);
        is_checkpoint_finished = true;
        return;
    }

    if( ORTE_SNAPC_CKPT_STATE_ERROR == orte_checkpoint_globals.ckpt_status) {
        opal_show_help("help-orte-checkpoint.txt", "ckpt_failure", true,
                       orte_checkpoint_globals.pid, ORTE_ERROR);
        is_checkpoint_finished = true;
        return;
    }

    /* Status progression */
    if( orte_checkpoint_globals.status ) {
        pretty_print_status();
    }

    if( ORTE_SNAPC_CKPT_STATE_STOPPED == orte_checkpoint_globals.ckpt_status) {
        is_checkpoint_finished = true;
        return;
    }

    /* Normal termination check */
    if( (ORTE_SNAPC_CKPT_STATE_RECOVERED   == orte_checkpoint_globals.ckpt_status && is_checkpoint_established) ||
        (ORTE_SNAPC_CKPT_STATE_ESTABLISHED == orte_checkpoint_globals.ckpt_status && is_checkpoint_recovered) ){
        is_checkpoint_finished = true;
        return;
    }
    else if( ORTE_SNAPC_CKPT_STATE_RECOVERED  == orte_checkpoint_globals.ckpt_status ) {
        is_checkpoint_recovered = true;
    }
    else if(ORTE_SNAPC_CKPT_STATE_ESTABLISHED == orte_checkpoint_globals.ckpt_status ) {
        is_checkpoint_established = true;
    }
}

static int notify_process_for_checkpoint(opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t *buffer = NULL;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_GLOBAL_INIT_CMD;
    orte_jobid_t jobid = ORTE_JOBID_INVALID;

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, orte_checkpoint_globals.output,
                        "orte_checkpoint: notify_hnp: Contact Head Node Process PID %d\n",
                        orte_checkpoint_globals.pid);

    timer_start = get_time();

    /***********************************
     * Notify HNP of checkpoint request
     * Send:
     * - Command
     * - options
     * - jobid
     ***********************************/
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_SNAPC_CMD)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_snapc_base_pack_options(buffer, options)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &jobid, 1, ORTE_JOBID))) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.send_buffer_nb(&(orterun_hnp->name), buffer,
                                                       ORTE_RML_TAG_CKPT, orte_rml_send_callback,
                                                       NULL))) {
        exit_status = ret;
        goto cleanup;
    }

    opal_output_verbose(10, orte_checkpoint_globals.output,
                        "orte_checkpoint: notify_hnp: Requested a checkpoint of jobid %s\n",
                        ORTE_JOBID_PRINT(jobid));

 cleanup:
    if( ORTE_SUCCESS != exit_status ) {
        opal_show_help("help-orte-checkpoint.txt", "unable_to_connect", true,
                       orte_checkpoint_globals.pid);
    }

    return exit_status;
}

/***************
 * Pretty Print
 ***************/
static double get_time(void) {
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

static int pretty_print_status(void) {
    char * state_str = NULL;
    double cur_time;

    cur_time = get_time();

    if( timer_last == 0 ) {
        timer_last = cur_time;
    }

    orte_snapc_ckpt_state_str(&state_str, orte_checkpoint_globals.ckpt_status);

    if( NULL != global_snapshot_handle ) {
        opal_output(0,
                    "[%6.2f / %6.2f] %*s - %s\n",
                    (cur_time - timer_last), (cur_time - timer_start),
                    25, state_str, global_snapshot_handle);
    } else {
        opal_output(0,
                    "[%6.2f / %6.2f] %*s - ...\n",
                    (cur_time - timer_last), (cur_time - timer_start),
                    25, state_str);
    }

    if( NULL != state_str) {
        free(state_str);
    }

    timer_last = cur_time;

    return ORTE_SUCCESS;
}

static int pretty_print_reference(void)
{
#if OPAL_ENABLE_CRDEBUG == 1
    if( orte_checkpoint_globals.enable_crdebug ) {
        printf("Checkpoint handle: -s %3d %s\n",
               global_sequence_num,
               global_snapshot_handle);
        return ORTE_SUCCESS;
    }
#endif

    printf("Snapshot Ref.: %3d %s\n",
           global_sequence_num,
           global_snapshot_handle);

    return ORTE_SUCCESS;
}

static int list_all_snapshots(void) {
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_t *all_snapshots = NULL;
    opal_list_item_t* item = NULL;
    orte_sstore_base_global_snapshot_info_t *global_snapshot = NULL;

    all_snapshots = OBJ_NEW(opal_list_t);

    if( ORTE_SUCCESS != (ret = orte_sstore_base_get_all_snapshots(all_snapshots, NULL)) ) {
        opal_output(0, "Error: Unable to list the checkpoints in the directory <%s>\n",
                    orte_sstore_base_global_snapshot_dir);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each reference
     */
    for(item  = opal_list_get_first(all_snapshots);
        item != opal_list_get_end(all_snapshots);
        item  = opal_list_get_next(item) ) {
        global_snapshot = (orte_sstore_base_global_snapshot_info_t*)item;

        /*
         * Get a list of valid sequence numbers
         */
        if( ORTE_SUCCESS != (ret = orte_sstore_base_find_all_seq_nums(global_snapshot,
                                                                      &(global_snapshot->num_seqs),
                                                                      &(global_snapshot->all_seqs)))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

#if OPAL_ENABLE_CRDEBUG == 1
        int s;
        /* Pretty print the result - C/R Debug version */
        if( orte_checkpoint_globals.enable_crdebug ) {
            for(s = 0; s < global_snapshot->num_seqs; ++s) {
                printf("-s %s %s\n", global_snapshot->all_seqs[s], global_snapshot->reference);
            }
        }
        else
#endif
        {
            /* Pretty print the result */
            printf("Snapshot Ref.: %s\t[",
                   global_snapshot->reference);
            if( 0 >= global_snapshot->num_seqs ) {
                printf("No Valid Checkpoints");
            } else {
                printf("%s",
                       opal_argv_join(global_snapshot->all_seqs, ','));
            }
            printf("]\n");
        }
    }

 cleanup:
    while (NULL != (item = opal_list_remove_first(all_snapshots))) {
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE(all_snapshots);

    return exit_status;
}
