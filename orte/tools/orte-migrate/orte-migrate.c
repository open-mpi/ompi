/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * ORTE Process Migration Tool for migrating processes in a multiprocess job
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"

#include MCA_timer_IMPLEMENTATION_HEADER

/******************
 * Local Functions
 ******************/
static int tool_init(int argc, char *argv[]); /* Initalization routine */
static int tool_finalize(void); /* Finalization routine */
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

static int notify_hnp(void);
static int pretty_print_status(void);
static int pretty_print_migration(void);

static orte_hnp_contact_t *orterun_hnp = NULL;
static int    orte_migrate_ckpt_status = ORTE_ERRMGR_MIGRATE_STATE_NONE;

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
static bool listener_started = false;

static double timer_start = 0;
static double timer_last  = 0;
static double get_time(void);

typedef struct {
    bool help; 
    int  pid;
    bool verbose;
    int  verbose_level;
    bool status;
    int output;
    char *off_nodes;
    char *off_procs;
    char *onto_nodes;
} orte_migrate_globals_t;

orte_migrate_globals_t orte_migrate_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL,
      'h', NULL, "help", 
      0,
      &orte_migrate_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL,
      'v', NULL, "verbose", 
      0,
      &orte_migrate_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL,
      'V', NULL, NULL, 
      1,
      &orte_migrate_globals.verbose_level, OPAL_CMD_LINE_TYPE_INT,
      "Set the verbosity level (For additional debugging information)" },

    { "hnp-pid",
      '\0', NULL, "hnp-pid", 
      1,
      &orte_migrate_globals.pid, OPAL_CMD_LINE_TYPE_INT,
      "This should be the pid of the mpirun whose applications you wish "
      "to migrate." },

    { NULL,
      'x', NULL, "off", 
      1,
      &orte_migrate_globals.off_nodes, OPAL_CMD_LINE_TYPE_STRING,
      "List of nodes to migrate off of (comma separated)" },

    { NULL,
      'r', NULL, "ranks", 
      1,
      &orte_migrate_globals.off_procs, OPAL_CMD_LINE_TYPE_STRING,
      "List of MPI_COMM_WORLD ranks to migrate (comma separated)" },

    { NULL,
      't', NULL, "onto", 
      1,
      &orte_migrate_globals.onto_nodes, OPAL_CMD_LINE_TYPE_STRING,
      "List of nodes to migrate onto (comma separated)" },

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
    if (ORTE_SUCCESS != (ret = tool_init(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /***************************
     * Find the HNP that we want to connect to, if it exists
     ***************************/
    if( orte_migrate_globals.verbose ) {
        opal_output_verbose(10, orte_migrate_globals.output,
                            "orte_migrate: Finding HNP...");
    }
    if (ORTE_SUCCESS != (ret = find_hnp())) {
        opal_show_help("help-orte-migrate.txt", "invalid_pid",
                       true, orte_migrate_globals.pid);
        exit_status = ret;
        goto cleanup;
    }

    /*******************************
     * Send migration information to HNP
     *******************************/
    if( orte_migrate_globals.verbose ) {
        opal_output_verbose(10, orte_migrate_globals.output,
                            "orte_migrate: Sending info to HNP...");
    }
    if (ORTE_SUCCESS != (ret = notify_hnp())) {
        opal_output(0,
                    "HNP with PID %d Not found!",
                    orte_migrate_globals.pid);
        exit_status = ret;
        goto cleanup;
    }

    /*******************************
     * Wait for migration to complete
     *******************************/
    while( ORTE_ERRMGR_MIGRATE_STATE_FINISH         != orte_migrate_ckpt_status &&
           ORTE_ERRMGR_MIGRATE_STATE_ERROR          != orte_migrate_ckpt_status &&
           ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS != orte_migrate_ckpt_status) {
        opal_progress();
    }

    if( orte_migrate_globals.status ) {
        orte_migrate_ckpt_status = ORTE_ERRMGR_MIGRATE_STATE_FINISH;
        pretty_print_status();
    }

 cleanup:
    /***************
     * Cleanup
     ***************/
    if (ORTE_SUCCESS != (ret = tool_finalize())) {
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
    memset(&orte_migrate_globals, 0, sizeof(orte_migrate_globals_t));
    orte_migrate_globals.help       = false;
    orte_migrate_globals.pid        = -1;
    orte_migrate_globals.verbose    = false;
    orte_migrate_globals.verbose_level  = 0;
    orte_migrate_globals.status     = false;
    orte_migrate_globals.output     = -1;
    orte_migrate_globals.off_nodes  = NULL;
    orte_migrate_globals.off_procs  = NULL;
    orte_migrate_globals.onto_nodes = NULL;

#if OPAL_ENABLE_FT_CR == 0
    /* Warn and exit if not configured with Migrate/Restart */
    {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-migrate.txt", "usage-no-cr",
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
    ret = opal_cmd_line_parse(&cmd_line, false, argc, argv);
    
    if (OPAL_SUCCESS != ret) {
        if (OPAL_ERR_SILENT != ret) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(ret));
        }
        exit_status = 1;
        goto cleanup;
    }

    if (orte_migrate_globals.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orte-migrate.txt", "usage", true,
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

    if (NULL == orte_migrate_globals.off_nodes && 
        NULL == orte_migrate_globals.off_procs) {
        fprintf(stderr, "%s: Nothing to do\n", argv0);
        fprintf(stderr, "Type '%s --help' for usage.\n", argv0);
        exit_status = 1;
        goto cleanup;
    }

    if(orte_migrate_globals.verbose_level < 0 ) {
        orte_migrate_globals.verbose_level = 0;
    }

    if(orte_migrate_globals.verbose_level > 0) {
        orte_migrate_globals.verbose = true;
    }

    /*
     * If the user did not supply an hnp jobid, then they must 
     *  supply the PID of MPIRUN
     */
    if(0 >= argc ) {
        fprintf(stderr, "%s: Nothing to do\n", argv[0]);
        fprintf(stderr, "Type '%s --help' for usage.\n", argv[0]);
        
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    orte_migrate_globals.pid = atoi(argv[0]);
    if ( 0 >= orte_migrate_globals.pid ) {
        opal_show_help("help-orte-migrate.txt", "invalid_pid", true,
                       orte_migrate_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    if(orte_migrate_globals.verbose) {
        orte_migrate_globals.status = true;
    }

    if(orte_migrate_globals.verbose) {
        pretty_print_migration();
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
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    
    /* search the list for the desired hnp */
    while (NULL != (item = opal_list_remove_first(&hnp_list))) {
        hnpcandidate = (orte_hnp_contact_t*)item;
        if( hnpcandidate->pid        == orte_migrate_globals.pid) {
            /* this is the one we want */
            orterun_hnp = hnpcandidate;
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }
    }
    
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

static int tool_init(int argc, char *argv[]) {
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

    /* Disable the migrate notification routine for this
     * tool. As we will never need to migrate this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);

    /* Select the none component, since we don't actually use a migrateer */
    (void) mca_base_var_env_name("crs", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;
    
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
    if( orte_migrate_globals.verbose ) {
        orte_migrate_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_migrate_globals.output, orte_migrate_globals.verbose_level);
    } else {
        orte_migrate_globals.output = 0; /* Default=STDERR */
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

static int tool_finalize(void) {
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
    int ret, exit_status = ORTE_SUCCESS;

    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_MIGRATE,
                                                       ORTE_RML_PERSISTENT,
                                                       hnp_receiver,
                                                       NULL))) {
        exit_status = ret;
        goto cleanup;
    }

    listener_started = true;

 cleanup:
    return exit_status;
}

static int stop_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if( !listener_started ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                    ORTE_RML_TAG_MIGRATE))) {
        exit_status = ret;
        goto cleanup;
    }

    listener_started = false;
 cleanup:
    return exit_status;
}

static void hnp_receiver(int status,
                         orte_process_name_t* sender,
                         opal_buffer_t* buffer,
                         orte_rml_tag_t tag,
                         void* cbdata)
{
    orte_errmgr_tool_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;

    opal_output_verbose(5, orte_migrate_globals.output,
                        "orte_migrate: hnp_receiver: Receive a command message.");

    /*
     * Otherwise this is an inter-coordinator command (usually updating state info).
     */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, ORTE_ERRMGR_MIGRATE_TOOL_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_ERRMGR_MIGRATE_TOOL_UPDATE_CMD:
            opal_output_verbose(10, orte_migrate_globals.output,
                                "orte_migrate: hnp_receiver: Status Update.");

            process_ckpt_update_cmd(sender, buffer);
            break;

        case ORTE_ERRMGR_MIGRATE_TOOL_INIT_CMD:
            /* Do Nothing */
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
}

static void process_ckpt_update_cmd(orte_process_name_t* sender,
                                    opal_buffer_t* buffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count = 1;
    int ckpt_status = ORTE_ERRMGR_MIGRATE_STATE_NONE;

    /*
     * Receive the data:
     * - ckpt_state
     */
    count = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &ckpt_status, &count, OPAL_INT)) ) {
        exit_status = ret;
        goto cleanup;
    }
    orte_migrate_ckpt_status = ckpt_status;

    /*
     * If the job is not able to be migrateed, then return
     */
    if( ORTE_SNAPC_CKPT_STATE_NO_CKPT == orte_migrate_ckpt_status) {
        opal_show_help("help-orte-migrate.txt", "non-ckptable", 
                       true,
                       orte_migrate_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * If a migration is already in progress, then we must tell the user to
     * try again later.
     */
    if( ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS == orte_migrate_ckpt_status) {
        opal_show_help("help-orte-migrate.txt", "err-inprogress", 
                       true,
                       orte_migrate_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * If there was an error, display a message and exit
     */
    if( ORTE_ERRMGR_MIGRATE_STATE_ERROR == orte_migrate_ckpt_status ) {
        opal_show_help("help-orte-migrate.txt", "err-other", 
                       true,
                       orte_migrate_globals.pid);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * If we are to display the status progression
     */
    if( orte_migrate_globals.status ) {
        if(ORTE_ERRMGR_MIGRATE_STATE_FINISH != orte_migrate_ckpt_status) {
            pretty_print_status();
        }
    }

 cleanup:
    return;
}

static int notify_hnp(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t *buffer = NULL;
    orte_errmgr_tool_cmd_flag_t command = ORTE_ERRMGR_MIGRATE_TOOL_INIT_CMD;

    if (NULL == (buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, orte_migrate_globals.output,
                        "orte_migrate: notify_hnp: Contact Head Node Process PID %d\n",
                        orte_migrate_globals.pid);

    timer_start = get_time();

    /***********************************
     * Notify HNP of migrate request
     * Send:
     * - Command
     * - Off Nodes
     * - Off Procs
     * - Onto Nodes
     ***********************************/
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &command, 1, ORTE_ERRMGR_MIGRATE_TOOL_CMD)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(orte_migrate_globals.off_procs), 1, OPAL_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(orte_migrate_globals.off_nodes), 1, OPAL_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(orte_migrate_globals.onto_nodes), 1, OPAL_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if ( 0 > (ret = orte_rml.send_buffer(&(orterun_hnp->name), buffer, ORTE_RML_TAG_MIGRATE, 0)) ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != buffer) {
        OBJ_RELEASE(buffer);
        buffer = NULL;
    }

    if( ORTE_SUCCESS != exit_status ) {
        opal_show_help("help-orte-migrate.txt", "unable_to_connect", true,
                       orte_migrate_globals.pid);
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

    orte_errmgr_base_migrate_state_str(&state_str, orte_migrate_ckpt_status);

    opal_output(0,
                "[%6.2f / %6.2f] %*s - ...\n", 
                (cur_time - timer_last), (cur_time - timer_start),
                25, state_str);

    if( NULL != state_str) {
        free(state_str);
    }

    timer_last = cur_time;

    return ORTE_SUCCESS;
}

static int pretty_print_migration(void)
{
    char **loc_off_nodes = NULL;
    char **loc_off_procs = NULL;
    char **loc_onto_nodes = NULL;
    int  loc_off_nodes_cnt = 0;
    int  loc_off_procs_cnt = 0;
    int  loc_onto_cnt = 0;
    int i;

    if( NULL != orte_migrate_globals.off_nodes ) {
        loc_off_nodes = opal_argv_split(orte_migrate_globals.off_nodes, ',');
        loc_off_nodes_cnt = opal_argv_count(loc_off_nodes);
    }

    if( NULL != orte_migrate_globals.off_procs ) {
        loc_off_procs     = opal_argv_split(orte_migrate_globals.off_procs, ',');
        loc_off_procs_cnt = opal_argv_count(loc_off_procs);
    }

    if( NULL != orte_migrate_globals.onto_nodes ) {
        loc_onto_nodes = opal_argv_split(orte_migrate_globals.onto_nodes, ',');
        loc_onto_cnt = opal_argv_count(loc_onto_nodes);
    }

    printf("Migrate Nodes: (%d nodes)\n", loc_off_nodes_cnt);
    for(i = 0; i < loc_off_nodes_cnt; ++i) {
        printf("\t\"%s\"\n", loc_off_nodes[i]);
    }

    printf("Migrate Ranks: (%d ranks)\n", loc_off_procs_cnt);
    for(i = 0; i < loc_off_procs_cnt; ++i) {
        printf("\t\"%s\"\n", loc_off_procs[i]);
    }

    printf("Migrate Onto : (%d nodes)\n", loc_onto_cnt);
    for(i = 0; i < loc_onto_cnt; ++i) {
        printf("\t\"%s\"\n", loc_onto_nodes[i]);
    }

    return ORTE_SUCCESS;    
}

