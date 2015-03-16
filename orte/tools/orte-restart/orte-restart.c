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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * ORTE Restart Tool for restarting a previously checkpointed multiprocess job
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/runtime/opal.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/cmd_line.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/error.h"
#include "opal/util/path.h"
#include "opal/mca/base/base.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_cr.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/sstore/sstore.h"
#include "orte/mca/sstore/base/base.h"
#include "orte/mca/filem/base/base.h"
#include "opal/util/show_help.h"
#include "orte/util/proc_info.h"

/******************
 * Local Functions
 ******************/
static int initialize(int argc, char *argv[]);
static int finalize(void);
static int parse_args(int argc, char *argv[]);
static int create_appfile(orte_sstore_base_global_snapshot_info_t *snapshot);
static int spawn_children(orte_sstore_base_global_snapshot_info_t *snapshot, pid_t *child_pid);
static int snapshot_info(orte_sstore_base_global_snapshot_info_t *snapshot);
static int snapshot_sort_compare_fn(opal_list_item_t **a,
                                    opal_list_item_t **b);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    char *snapshot_ref;
    char *appfile;
    bool verbose;
    bool forked;
    int  seq_number;
    char *hostfile;
    int  output;
    bool info_only;
    bool app_only;
    bool showme;
    char *mpirun_opts;
#if OPAL_ENABLE_CRDEBUG == 1
    bool enable_crdebug;
#endif
} orte_restart_globals_t;

orte_restart_globals_t orte_restart_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help", 
      0,
      &orte_restart_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      'v', NULL, "verbose", 
      0,
      &orte_restart_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL,
      '\0', NULL, "fork", 
      0,
      &orte_restart_globals.forked, OPAL_CMD_LINE_TYPE_BOOL,
      "Fork off a new process which is the restarted process instead of "
      "replacing orte_restart" },

    { NULL,
      's', NULL, "seq", 
      1,
      &orte_restart_globals.seq_number, OPAL_CMD_LINE_TYPE_INT,
      "The sequence number of the checkpoint to start from. "
      "(Default: -1, or most recent)" },

    { NULL,
      '\0', "hostfile", "hostfile", 
      1,
      &orte_restart_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile to use for launch" },

    { NULL,
      '\0', "machinefile", "machinefile", 
      1,
      &orte_restart_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile to use for launch" },

    { NULL,
      'i', NULL, "info", 
      0,
      &orte_restart_globals.info_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Display information about the checkpoint" },

    { NULL,
      'a', NULL, "apponly", 
      0,
      &orte_restart_globals.app_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Only create the app context file, do not restart from it" },

    { NULL,
      '\0', NULL, "showme", 
      0,
      &orte_restart_globals.showme, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the full command line that would have been exec'ed." },

    { NULL,
      '\0', "mpirun_opts", "mpirun_opts", 
      1,
      &orte_restart_globals.mpirun_opts, OPAL_CMD_LINE_TYPE_STRING,
      "Command line options to pass directly to mpirun (be sure to quote long strings, and escape internal quotes)" },

#if OPAL_ENABLE_CRDEBUG == 1
    { NULL,
      '\0', "crdebug", "crdebug",
      0,
      &orte_restart_globals.enable_crdebug, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable C/R Enhanced Debugging" },
#endif

    /* End of list */
    { NULL,
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;
    pid_t child_pid = 0;
    orte_sstore_base_global_snapshot_info_t *snapshot = NULL;
    char *basedir = NULL;
    char *tmp_str = NULL;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = initialize(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    snapshot = OBJ_NEW(orte_sstore_base_global_snapshot_info_t);

    if( opal_path_is_absolute(orte_restart_globals.snapshot_ref) ) {
        basedir = opal_dirname(orte_restart_globals.snapshot_ref);
        tmp_str = opal_basename(orte_restart_globals.snapshot_ref);
        free(orte_restart_globals.snapshot_ref);
        orte_restart_globals.snapshot_ref = strdup(tmp_str);
        free(tmp_str);
        tmp_str = NULL;
    } else if( NULL != strchr(orte_restart_globals.snapshot_ref, '/') ) {
        basedir = opal_dirname(orte_restart_globals.snapshot_ref);
        tmp_str = opal_basename(orte_restart_globals.snapshot_ref);
        free(orte_restart_globals.snapshot_ref);
        orte_restart_globals.snapshot_ref = strdup(tmp_str);
        free(tmp_str);
        tmp_str = NULL;
    } else {
        basedir = NULL; /* Use MCA parameter */
    }

    /*
     * Note: If the seq # passed is -1, then the largest seq # is selected,
     *       ow the seq # requested is selected if available
     * 'basedir': Snapshot Base location to look in. If NULL then MCA parameter is used
     */
    if( ORTE_SUCCESS != (ret = orte_sstore.request_restart_handle(&(snapshot->ss_handle),
                                                                  basedir,
                                                                  orte_restart_globals.snapshot_ref,
                                                                  orte_restart_globals.seq_number,
                                                                  snapshot))) {
        opal_show_help("help-orte-restart.txt", "invalid_filename", true,
                       orte_restart_globals.snapshot_ref);
        exit_status = ret;
        goto cleanup;
    }
    orte_restart_globals.seq_number = snapshot->seq_num;

    if(orte_restart_globals.info_only ) {
        if (ORTE_SUCCESS != (ret = snapshot_info(snapshot))) {
            exit_status = ret;
            goto cleanup;
        }
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /******************************
     * Create the app file to use with mpirun/orterun
     ******************************/
    if( ORTE_SUCCESS != (ret = create_appfile(snapshot) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    if( orte_restart_globals.app_only ) {
        printf("Created Appfile:\n\t%s\n", orte_restart_globals.appfile);
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    /******************************
     * Restart in this process [mpirun/orterun]
     ******************************/
    if( orte_restart_globals.verbose ) {
        opal_output_verbose(10, orte_restart_globals.output,
                            "Restarting from file (%s)",
                            orte_restart_globals.snapshot_ref);
        
        if( orte_restart_globals.forked ) {
            opal_output_verbose(10, orte_restart_globals.output,
                                "\t Forking off a child");
        } else {
            opal_output_verbose(10, orte_restart_globals.output,
                                "\t Exec in self");
        }
    }

    if( ORTE_SUCCESS != (ret = spawn_children(snapshot, &child_pid)) ) {
        opal_show_help("help-orte-restart.txt", "restart_cmd_failure", true,
                       orte_restart_globals.snapshot_ref, ret);
        exit_status = ret;
        goto cleanup;
    }

    /***************
     * Cleanup
     ***************/
 cleanup:
    if( NULL != basedir ) {
        free(basedir);
        basedir = NULL;
    }
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    if( NULL != snapshot ) {
        OBJ_RELEASE(snapshot);
        snapshot = NULL;
    }

    if (OPAL_SUCCESS != (ret = finalize())) {
        return ret;
    }

    return exit_status;
}

static int initialize(int argc, char *argv[]) {
    int ret, exit_status = ORTE_SUCCESS;
    char * tmp_env_var = NULL;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util(&argc, &argv)) ) {
        return ret;
    }

    /*
     * Parse command line arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( orte_restart_globals.verbose ) {
        orte_restart_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_restart_globals.output, 10);
    } else {
        orte_restart_globals.output = 0; /* Default=STDERR */
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
    /* Don't free the environment variable name. It is used again below */

    /*
     * Setup any ORTE stuff we might need
     */
    if (OPAL_SUCCESS != (ret = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /* Unset these now that we no longer need them */
    opal_unsetenv(tmp_env_var, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    (void) mca_base_var_env_name("opal_cr_is_tool", &tmp_env_var);
    opal_unsetenv(tmp_env_var, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

 cleanup:
    return exit_status;
}

static int finalize(void)
{
    int ret;

    if (OPAL_SUCCESS != (ret = orte_finalize())) {
        return ret;
    }

    return ORTE_SUCCESS;
}

static int parse_args(int argc, char *argv[])
{
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    char * tmp_env_var = NULL;
    char *argv0 = NULL;
    orte_restart_globals_t tmp = { false, /* help */
                                   NULL,  /* filename */
                                   NULL,  /* appfile */
                                   false, /* verbose */
                                   false, /* forked */
                                   -1,    /* seq_number */
                                   NULL,  /* hostfile */
                                   -1,    /* output*/
                                   false, /* info only */
                                   false, /* app only */
                                   false, /* showme */
                                   NULL}; /* mpirun_opts */

    orte_restart_globals = tmp;
#if OPAL_ENABLE_CRDEBUG == 1
    orte_restart_globals.enable_crdebug = false;
#endif

#if OPAL_ENABLE_FT_CR == 0
    /* Warn and exit if not configured with Checkpoint/Restart */
    {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-restart.txt", "usage-no-cr",
                                    true, args);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        return ORTE_ERROR;
    }
#endif

    /* Parse the command line options */    
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    
    mca_base_open();
    mca_base_cmd_line_setup(&cmd_line);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    
    if (OPAL_SUCCESS != ret) {
        if (OPAL_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(ret));
        }
        return 1;
    }

    if (orte_restart_globals.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-restart.txt", "usage", true,
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
    mca_base_cmd_line_process_args(&cmd_line, &app_env, &global_env);
    
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
    if (0 == argc) {
        fprintf(stderr, "%s: Nothing to do\n", argv0);
        fprintf(stderr, "Type '%s --help' for usge.\n", argv0);
        free(argv0);
        return ORTE_ERROR;
    }
    free(argv0);

    orte_restart_globals.snapshot_ref = strdup(argv[0]);
    if ( NULL == orte_restart_globals.snapshot_ref || 
         0 >= strlen(orte_restart_globals.snapshot_ref) ) {
        opal_show_help("help-orte-restart.txt", "invalid_filename", true,
                       "<none provided>");
        return ORTE_ERROR;
    }

    /* If we have arguments after the command, then assume they
     * need to be grouped together.
     */
    if(argc > 1) {
        orte_restart_globals.snapshot_ref = strdup(opal_argv_join(argv, ' '));
    }
    
    return ORTE_SUCCESS;
}

static int create_appfile(orte_sstore_base_global_snapshot_info_t *snapshot)
{
    int exit_status = ORTE_SUCCESS;
    FILE *appfile = NULL;
    opal_list_item_t* item = NULL;
    char *tmp_str = NULL;
    char *amca_param = NULL;
    char *tune_param = NULL;
    char *reference_fmt_str = NULL;
    char *location_str = NULL;
    char *ref_location_fmt_str = NULL;
    orte_sstore_base_local_snapshot_info_t *vpid_snapshot = NULL;

    /*
     * Create the appfile
     */
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_LOC_ABS,
                         &tmp_str);
    asprintf(&orte_restart_globals.appfile, "%s/%s",
             tmp_str,
             strdup("restart-appfile"));
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_AMCA_PARAM,
                         &amca_param);

    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_TUNE_PARAM,
                         &tune_param);

    if (NULL == (appfile = fopen(orte_restart_globals.appfile, "w")) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* This will give a format string that we can use */
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_REF_FMT,
                         &reference_fmt_str);
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_LOC,
                         &location_str);
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_LOCAL_SNAP_REF_LOC_FMT,
                         &ref_location_fmt_str);

    /*
     * Sort the snapshots so that they are in order
     */
    opal_list_sort(&snapshot->local_snapshots, snapshot_sort_compare_fn);

    /*
     * Construct the appfile
     */
    for(item  = opal_list_get_first(&snapshot->local_snapshots);
        item != opal_list_get_end(&snapshot->local_snapshots);
        item  = opal_list_get_next(item) ) {
        vpid_snapshot = (orte_sstore_base_local_snapshot_info_t*)item;
        
        fprintf(appfile, "#\n");
        fprintf(appfile, "# Old Process Name: %u.%u\n", 
                vpid_snapshot->process_name.jobid,
                vpid_snapshot->process_name.vpid);
        fprintf(appfile, "#\n");
        fprintf(appfile, "-np 1 ");

        fprintf(appfile, "--sstore-load ");
        /* loc:ref:postfix:seq */
        fprintf(appfile, "%s:%s:",
                location_str,
                orte_restart_globals.snapshot_ref);
        fprintf(appfile, reference_fmt_str, vpid_snapshot->process_name.vpid);
        fprintf(appfile, ":%s:%s:%d ",
                (vpid_snapshot->compress_comp == NULL ? "" : vpid_snapshot->compress_comp),
                (vpid_snapshot->compress_postfix == NULL ? "" : vpid_snapshot->compress_postfix),
                orte_restart_globals.seq_number);

        if( NULL == amca_param ) {
            amca_param = strdup("ft-enable-cr");
            opal_show_help("help-orte-restart.txt", "amca_param_not_found", true,
                           amca_param);
        }
        fprintf(appfile, "-am %s ", amca_param);

        if( NULL == tune_param ) {
            tune_param = strdup("ft-enable-cr");
            opal_show_help("help-orte-restart.txt", "tune_param_not_found", true,
                           tune_param);
        }
        fprintf(appfile, "-tune %s ", tune_param);

        fprintf(appfile, " opal-restart ");

        /*
         * By default, point to the central storage location of the checkpoint.
         * The active SStore module at restart time will determine if files
         * need to be preloaded.
         */
        fprintf(appfile, "-l %s", location_str);
        fprintf(appfile, " -m %s ", orte_sstore_base_local_metadata_filename);

        fprintf(appfile, "-r ");
        fprintf(appfile, reference_fmt_str, vpid_snapshot->process_name.vpid);

        fprintf(appfile, "\n");
    }

 cleanup:
    if(NULL != appfile) {
        fclose(appfile);
        appfile = NULL;
    }
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    if( NULL != location_str ) {
        free(location_str);
        location_str = NULL;
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

static int spawn_children(orte_sstore_base_global_snapshot_info_t *snapshot, pid_t *child_pid)
{
    int ret, exit_status = ORTE_SUCCESS;
    char *amca_param = NULL;
    char *tune_param = NULL;
    char **argv = NULL;
    int argc = 0, i;
    int status;

    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_AMCA_PARAM,
                         &amca_param);

    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_TUNE_PARAM,
                         &tune_param);

    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "mpirun")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "-am")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( NULL == amca_param ) {
        amca_param = strdup("ft-enable-cr");
        opal_show_help("help-orte-restart.txt", "amca_param_not_found", true,
                       amca_param);
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, amca_param)) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "-tune")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( NULL == tune_param ) {
        tune_param = strdup("ft-enable-cr");
        opal_show_help("help-orte-restart.txt", "tune_param_not_found", true,
                       tune_param);
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, tune_param)) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( NULL != orte_restart_globals.hostfile ) {
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "--default-hostfile")) ) {
            exit_status = ret;
            goto cleanup;
        }
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.hostfile)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    if( orte_restart_globals.mpirun_opts ) {
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.mpirun_opts)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#if OPAL_ENABLE_CRDEBUG == 1
    if( orte_restart_globals.enable_crdebug ) {
        if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "--crdebug")) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#endif
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, "--app")) ) {
        exit_status = ret;
        goto cleanup;
    }
    if( ORTE_SUCCESS != (ret = opal_argv_append(&argc, &argv, orte_restart_globals.appfile)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if( orte_restart_globals.showme ) {
        for(i = 0; i < argc; ++i ) {
            /*printf("%2d: (%s)\n", i, argv[i]);*/
            printf("%s ", argv[i]);
        }
        printf("\n");
        return ORTE_SUCCESS;
    }

    /* To fork off a child */
    if( orte_restart_globals.forked ) {
        *child_pid = fork();
        
        if( 0 == *child_pid) {
            /* Child Process */
            status = execvp(strdup(argv[0]), argv);
            if( 0 > status) {
                opal_output(orte_restart_globals.output,
                            "orte_restart: execv failed with status = %d\n",
                            status);
            }
            exit_status = status;
            goto cleanup;
        }
        else if(0 < *child_pid) {
            /* Parent is done once it is started */
            ;
        }
        else {
            opal_output(orte_restart_globals.output,
                        "orte_restart: fork failed: This should never happen!");
            /* Fork failed :( */
            exit_status = *child_pid;
            goto cleanup;
        }
    }
    /* ... or not to fork off a child */
    else {
        /* Make sure to finalize so we don't leave our session directory */
        orte_finalize();

        status = execvp(strdup(argv[0]), argv);
        if( 0 > status) {
            /* execv failed */
        }
        exit_status = status;
        goto cleanup;
    }

    opal_output_verbose(10, orte_restart_globals.output,
                        "orte_restart: Restarted Child with PID = %d\n", *child_pid);

 cleanup:
    if( NULL != argv)
        opal_argv_free(argv);

    return exit_status;
}

int snapshot_info(orte_sstore_base_global_snapshot_info_t *snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    int num_seqs, processes, i;
    char **snapshot_ref_seqs = NULL;
    opal_list_item_t* item = NULL;
    orte_sstore_base_local_snapshot_info_t *vpid_snapshot = NULL;
    char *tmp_str = NULL;

    /*
     * Find all sequence numbers
     */
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_NUM_SEQ,
                         &tmp_str);
    num_seqs = atoi(tmp_str);
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    orte_sstore.get_attr(snapshot->ss_handle,
                         SSTORE_METADATA_GLOBAL_SNAP_ALL_SEQ,
                         &tmp_str);
    snapshot_ref_seqs = opal_argv_split(tmp_str, ',');
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    if( 0 > orte_restart_globals.seq_number ) {
        opal_output(orte_restart_globals.output,
                    "Sequences: %d\n",
                    num_seqs);
    }

    for(i=0; i < num_seqs; ++i) {
        snapshot->seq_num = atoi(snapshot_ref_seqs[i]);

        if( 0 <= orte_restart_globals.seq_number &&
            snapshot->seq_num != orte_restart_globals.seq_number ) {
            continue;
        }

        if( ORTE_SUCCESS != (ret = orte_sstore_base_extract_global_metadata( snapshot ) ) ) {
            exit_status = ret;
            goto cleanup;
        }

        opal_output(orte_restart_globals.output,
                    "Seq: %d\n",
                    snapshot->seq_num);

        if (NULL != snapshot->start_time ) {
            opal_output(orte_restart_globals.output,
                        "\tBegin Timestamp: %s\n",
                        snapshot->start_time);
        }
        if (NULL != snapshot->end_time ) {
            opal_output(orte_restart_globals.output,
                        "\tEnd Timestamp  : %s\n",
                        snapshot->end_time);
        }

        processes = opal_list_get_size(&snapshot->local_snapshots);
        opal_output(orte_restart_globals.output,
                    "\tProcesses: %d\n",
                    processes);

        for(item  = opal_list_get_first(&snapshot->local_snapshots);
            item != opal_list_get_end(&snapshot->local_snapshots);
            item  = opal_list_get_next(item) ) {
            vpid_snapshot = (orte_sstore_base_local_snapshot_info_t*)item;

            opal_output_verbose(10, orte_restart_globals.output,
                                "\t\tProcess: %u.%u \t CRS: %s \t Compress: %s (%s)",
                                vpid_snapshot->process_name.jobid,
                                vpid_snapshot->process_name.vpid,
                                vpid_snapshot->crs_comp,
                                vpid_snapshot->compress_comp,
                                vpid_snapshot->compress_postfix);
        }
    }

 cleanup:
    return exit_status;
}

static int snapshot_sort_compare_fn(opal_list_item_t **a,
                                    opal_list_item_t **b)
{
    orte_sstore_base_local_snapshot_info_t *snap_a, *snap_b;

    snap_a = (orte_sstore_base_local_snapshot_info_t*)(*a);
    snap_b = (orte_sstore_base_local_snapshot_info_t*)(*b);

    if( snap_a->process_name.vpid > snap_b->process_name.vpid ) {
        return 1;
    }
    else if( snap_a->process_name.vpid == snap_b->process_name.vpid ) {
        return 0;
    }
    else {
        return -1;
    }
}
