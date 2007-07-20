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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @fie
 * ORTE PS command
 *
 */

#include "orte_config.h"

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
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "orte/orte_constants.h"
#include "orte/runtime/runtime.h"
#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/ras/base/ras_private.h"

/*******************
 * Universe/job/vpid information Objects
 *******************/
struct orte_ps_vpid_info_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;
    
    /** General VPID Information */
    orte_std_cntr_t rank;
    pid_t pid;
    orte_process_name_t name;
    char * node;
    orte_proc_state_t state;
    
    orte_std_cntr_t app_context_idx;

#if OPAL_ENABLE_FT == 1
    size_t ckpt_state;
    char *ckpt_ref;
    char *ckpt_loc;
#endif
};
typedef struct orte_ps_vpid_info_t orte_ps_vpid_info_t;

void orte_ps_vpid_info_construct(orte_ps_vpid_info_t *obj);
void orte_ps_vpid_info_destruct( orte_ps_vpid_info_t *obj);

OBJ_CLASS_INSTANCE(orte_ps_vpid_info_t,
                   opal_list_item_t,
                   orte_ps_vpid_info_construct,
                   orte_ps_vpid_info_destruct);

struct orte_ps_job_info_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;
    
    /** General Job Information */
    orte_jobid_t     id;
    orte_job_state_t state;

    orte_std_cntr_t num_init;
    orte_std_cntr_t num_launched;
    orte_std_cntr_t num_running;
    orte_std_cntr_t num_finalized;
    orte_std_cntr_t num_terminated;
    orte_std_cntr_t num_aborted;
    orte_std_cntr_t slots;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;

#if OPAL_ENABLE_FT == 1
    size_t ckpt_state;
    char *ckpt_ref;
    char *ckpt_loc;
#endif

    orte_app_context_t **app_context;
    orte_std_cntr_t num_app_context;

    /** List of vpids */
    opal_list_t vpid_list;
};
typedef struct orte_ps_job_info_t orte_ps_job_info_t;

void orte_ps_job_info_construct(orte_ps_job_info_t *obj);
void orte_ps_job_info_destruct( orte_ps_job_info_t *obj);

OBJ_CLASS_INSTANCE(orte_ps_job_info_t,
                   opal_list_item_t,
                   orte_ps_job_info_construct,
                   orte_ps_job_info_destruct);


struct orte_ps_universe_info_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;
    
    /** Universe information */
    orte_universe_t  universe_info;
    
    /** List of Jobs */
    opal_list_t job_list;
    
    /** List of nodes on orte-node segment */
    opal_list_t nodes;
};
typedef struct orte_ps_universe_info_t orte_ps_universe_info_t;

void orte_ps_universe_info_construct(orte_ps_universe_info_t *obj);
void orte_ps_universe_info_destruct( orte_ps_universe_info_t *obj);

OBJ_CLASS_INSTANCE(orte_ps_universe_info_t,
                   opal_list_item_t,
                   orte_ps_universe_info_construct,
                   orte_ps_universe_info_destruct);


/******************
 * Local Functions
 ******************/
static int orte_ps_init(int argc, char *argv[]);
static int parse_args(int argc, char *argv[]);

static int connect_to_universe(orte_universe_t universe_info);

static int gather_information(orte_ps_universe_info_t* universe);
static int gather_active_jobs(orte_ps_universe_info_t* universe);
static int gather_nodes(orte_ps_universe_info_t* universe);
static int gather_job_info(orte_ps_universe_info_t* universe);
static int gather_vpid_info(orte_ps_universe_info_t* universe);

static int pretty_print(orte_ps_universe_info_t* universe);
static int pretty_print_nodes(opal_list_t *nodes);
static int pretty_print_jobs(opal_list_t *jobs);
static int pretty_print_vpids(orte_ps_job_info_t *job);

static char *pretty_univ_state(orte_universe_state_t state);
static char *pretty_node_state(orte_node_state_t state);
static char *pretty_job_state(orte_job_state_t state);
static char *pretty_vpid_state(orte_proc_state_t state);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool verbose;
    char *universe;
    int  jobid;
    int  vpid;
    bool gpr_dump;
    bool attached;
    bool nodes;
    int  output;
} orte_ps_globals_t;

orte_ps_globals_t orte_ps_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_ps_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_ps_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

    { NULL, NULL, NULL, 
      'd', NULL, "dump", 
      0,
      &orte_ps_globals.gpr_dump, OPAL_CMD_LINE_TYPE_BOOL,
      "Dump the state of the GPR" },

    { NULL, NULL, NULL, 
      '\0', NULL, "universe", 
      1,
      &orte_ps_globals.universe, OPAL_CMD_LINE_TYPE_STRING,
      "Specify a universe" },

    { NULL, NULL, NULL, 
      'j', NULL, "jobid", 
      1,
      &orte_ps_globals.jobid, OPAL_CMD_LINE_TYPE_INT,
      "Specify a specific jobid" },

    { NULL, NULL, NULL, 
      'p', NULL, "vpid", 
      1,
      &orte_ps_globals.vpid, OPAL_CMD_LINE_TYPE_INT,
      "Specify a specific vpid. Must specify a --jobid as well" },

    { NULL, NULL, NULL, 
      'n', NULL, "nodes", 
      0,
      &orte_ps_globals.nodes, OPAL_CMD_LINE_TYPE_INT,
      "Print Node Information" },

    /* End of list */
    { NULL, NULL, NULL, 
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_t universe_list;
    opal_list_item_t* item = NULL;
    opal_list_t universe_search_result;

    /***************
     * Initialize
     ***************/
    OBJ_CONSTRUCT(&universe_list, opal_list_t);
    OBJ_CONSTRUCT(&universe_search_result, opal_list_t);
    orte_ps_globals.attached = false;

    if (ORTE_SUCCESS != (ret = orte_ps_init(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Get the directory listing
     */
    opal_output_verbose(10, orte_ps_globals.output,
                        "orte_ps: Acquiring universe list...\n");

    if (ORTE_SUCCESS != (ret = orte_universe_search(&universe_search_result, true, false) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * For each universe in the listing
     */
    for(item  = opal_list_get_first(&universe_search_result);
        item != opal_list_get_end(&universe_search_result);
        item  = opal_list_get_next(item) ) {
        orte_ps_universe_info_t *univ;
        orte_universe_t *tmp_univ;

        /*
         * Copy over the universe information
         */
        univ = OBJ_NEW(orte_ps_universe_info_t);
        tmp_univ = (orte_universe_t *) item;
        univ->universe_info.state       = tmp_univ->state;
        univ->universe_info.persistence = tmp_univ->persistence;
        univ->universe_info.console     = tmp_univ->console;
        univ->universe_info.console_connected = tmp_univ->console_connected;
        if( NULL != tmp_univ->name )
            univ->universe_info.name    = strdup(tmp_univ->name);
        else
            univ->universe_info.name    = NULL;
        if( NULL != tmp_univ->host )
            univ->universe_info.host    = strdup(tmp_univ->host);
        else
            univ->universe_info.host    = NULL;
        if( NULL != tmp_univ->uid )
            univ->universe_info.uid     = strdup(tmp_univ->uid);
        else 
            univ->universe_info.uid     = NULL;
        if( NULL != tmp_univ->scope )
            univ->universe_info.scope   = strdup(tmp_univ->scope);
        else 
            univ->universe_info.scope   = NULL;
        if( NULL != tmp_univ->seed_uri)
            univ->universe_info.seed_uri   = strdup(tmp_univ->seed_uri);
        else 
            univ->universe_info.seed_uri   = NULL;
        if( NULL != tmp_univ->scriptfile )
            univ->universe_info.scriptfile = strdup(tmp_univ->scriptfile);
        else 
            univ->universe_info.scriptfile = NULL;

        opal_list_append(&universe_list, &(univ->super));

        /*
         * Connect to the universe
         */
        opal_output_verbose(10, orte_ps_globals.output,
                            "orte_ps: Connecting to universe: %s\n",
                            univ->universe_info.name);

        if( ORTE_SUCCESS != (ret = connect_to_universe(univ->universe_info)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Gather the information
         */
        opal_output_verbose(10, orte_ps_globals.output,
                            "orte_ps: Gathering Universe Information\n");

        if( ORTE_SUCCESS != (ret = gather_information(univ)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Print the General Universe information
         */
        if(ORTE_SUCCESS != (ret = pretty_print(univ)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * If we were asked to dump the GPR then do so
         */
        if( orte_ps_globals.gpr_dump) {
            if( ORTE_SUCCESS != (ret = orte_gpr.dump_all() ) ) {
                exit_status = ret;
                goto cleanup;
            }
        }
        
        /* JJH: This should be fixed eventually
         * Since connecting and disconnecting from a universe is
         * not well defined, only allow connection to the first
         * universe found.
         */
        break;
    }

    /***************
     * Cleanup
     ***************/
 cleanup:
    while (NULL != (item = opal_list_remove_first(&universe_list))) {
        OBJ_RELEASE(item);
    }
    while (NULL != (item = opal_list_remove_first(&universe_search_result))) {
        OBJ_RELEASE(item);
    }

    /*
     * Only finalize if we are attached to a specific universe
     */
    if(orte_ps_globals.attached) {
        if (OPAL_SUCCESS != (ret = orte_finalize())) {
            return ret;
        }
    }
    else {
        opal_finalize();
    }

    return exit_status;
}

static int parse_args(int argc, char *argv[]) {
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    orte_ps_globals_t tmp = { false, 
                              false, 
                              NULL, 
                              -1, 
                              -1, 
                              false,
                              false,
                              false,
                              -1};

    orte_ps_globals = tmp;

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

#if OPAL_ENABLE_FT == 1
    opal_setenv(mca_base_param_env_var("opal_cr_is_tool"),
                "1",
                true, &environ);
#endif

    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || 
        orte_ps_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-ps.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    /*
     * If they specify a vpid, they must specify a jobid
     */
    if( 0 <= orte_ps_globals.vpid) {
        if( 0 > orte_ps_globals.jobid) {
            opal_show_help("help-orte-ps.txt", "vpid-usage", true,
                           orte_ps_globals.vpid);
            return ORTE_ERROR;
        }
    }

    return ORTE_SUCCESS;
}

static int orte_ps_init(int argc, char *argv[]) {
    int exit_status = ORTE_SUCCESS, ret;

    /*
     * Make sure to init util before parse_args
     * to ensure installdirs is setup properly
     * before calling mca_base_open();
     */
    if( ORTE_SUCCESS != (ret = opal_init_util()) ) {
        return ret;
    }

    /*
     * Parse Command Line Arguments
     */
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup OPAL Output handle from the verbose argument
     */
    if( orte_ps_globals.verbose ) {
        orte_ps_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_ps_globals.output, 10);
    } else {
        orte_ps_globals.output = 0; /* Default=STDOUT */
    }

    /*
     * We are trying to attach to another process' GPR so we need to 
     * attach no matter if it is identified as private or not.
     */
    opal_setenv(mca_base_param_env_var("universe_console"),
                "1", true, &environ);

#if OPAL_ENABLE_FT == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);

    /* Select the none component, since we don't actually use a checkpointer */
    opal_setenv(mca_base_param_env_var("crs"),
                "none",
                true, &environ);
#endif
    
    /***************************
     * We need all of OPAL
     ***************************/
    if (ORTE_SUCCESS != (ret = opal_init())) {
        exit_status = ret;
        goto cleanup;
    }

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

 cleanup:
    return exit_status;
}

static int pretty_print(orte_ps_universe_info_t* universe) {
    int i, line_len;
    int len_name  = 0,
        len_host  = 0,
        len_uid   = 0,
        len_scope = 0,
        len_per   = 0,
        len_state = 0;

    /*
     * Calculate segment lengths
     */
    len_name  = (int) (strlen(universe->universe_info.name) < strlen("Universe Name") ? 
                       strlen("Universe Name") : 
                       strlen(universe->universe_info.name) );
    len_host  = (int) (strlen(universe->universe_info.host) < strlen("Hostname") ?
                       strlen("Hostname") :
                       strlen(universe->universe_info.host));
    len_uid   = (int) (strlen(universe->universe_info.uid) < strlen("UID") ?
                       strlen("UID") :
                       strlen(universe->universe_info.uid));
    len_per   = (int) strlen("Persistent");
    len_scope = (int) (strlen(universe->universe_info.scope) < strlen("Scope") ?
                       strlen("Scope") :
                       strlen(universe->universe_info.scope));
    len_state = (int) (strlen(pretty_univ_state(universe->universe_info.state)) < strlen("State") ?
                       strlen("State") :
                       strlen(pretty_univ_state(universe->universe_info.state)) );

    line_len = (len_name   + 3 +
                len_host   + 3 +
                len_uid    + 3 +
                len_per    + 3 +
                len_scope  + 3 +
                len_state) + 2 ;

    /*
     * Print header
     */
    printf("%*s | ", len_name , "Universe Name");
    printf("%*s | ", len_host , "Hostname");
    printf("%*s | ", len_uid  , "UID");
    printf("%*s | ", len_per  , "Persistent");
    printf("%*s | ", len_scope, "Scope");
    printf("%*s |" , len_state, "State");
    printf("\n");

    for(i = 0; i < line_len; ++i) {
        printf("-");
    }
    printf("\n");

    /*
     * Print Info
     */
    printf("%*s | ", len_name,  universe->universe_info.name);
    printf("%*s | ", len_host,  universe->universe_info.host);
    printf("%*s | ", len_uid,   universe->universe_info.uid);
    if(universe->universe_info.persistence)
        printf("%*s | ", len_per, "true");
    else 
        printf("%*s | ", len_per, "false");
    printf("%*s | ", len_scope, universe->universe_info.scope);
    printf("%*s |",  len_state, pretty_univ_state(universe->universe_info.state));
    printf("\n");

    printf("\n");

    /*
     * Print Node Information
     */
    if( orte_ps_globals.nodes )
        pretty_print_nodes(&universe->nodes);

    /*
     * Print Job Information
     */
    pretty_print_jobs(&universe->job_list);

    return ORTE_SUCCESS;
}

static int pretty_print_nodes(opal_list_t *nodes) {
    opal_list_item_t* node_item = NULL;
    int i, line_len;
    int len_name    = 0,
        len_arch    = 0,
        len_cell    = 0,
        len_state   = 0,
        len_slots   = 0,
        len_slots_i = 0,
        len_slots_m = 0;

    /*
     * Caculate segment lengths
     */
    len_name    = (int) strlen("Node Name");
    len_arch    = (int) strlen("Arch");
    len_cell    = (int) strlen("Cell ID");
    len_state   = (int) strlen("State");
    len_slots   = (int) strlen("Slots");
    len_slots_i = (int) strlen("Slots In Use");
    len_slots_m = (int) strlen("Slots Max");

    for(node_item  = opal_list_get_first(nodes);
        node_item != opal_list_get_end(nodes);
        node_item  = opal_list_get_next(node_item) ) {
        orte_ras_node_t *node;
        node = (orte_ras_node_t *)node_item;
        
        if( NULL != node->node_name &&
            (int)strlen(node->node_name) > len_name)
            len_name = (int) strlen(node->node_name);

        if( NULL != node->node_arch &&
            (int)strlen(node->node_arch) > len_arch)
            len_arch = (int) strlen(node->node_arch);
        
        if( (int)strlen(pretty_node_state(node->node_state)) > len_state )
            len_state = (int)strlen(pretty_node_state(node->node_state));
    }

    line_len = (len_name    + 3 +
                len_arch    + 3 +
                len_cell    + 3 +
                len_state   + 3 +
                len_slots   + 3 +
                len_slots_i + 3 +
                len_slots_m) + 2;

    /*
     * Print the header
     */
    printf("%*s | ", len_name,    "Node Name");
    printf("%*s | ", len_arch,    "Arch");
    printf("%*s | ", len_cell,    "Cell ID");
    printf("%*s | ", len_state,   "State");
    printf("%*s | ", len_slots,   "Slots");
    printf("%*s | ", len_slots_m, "Slots Max");
    printf("%*s | ", len_slots_i, "Slots In Use");
    printf("\n");

    for(i = 0; i < line_len; ++i) {
        printf("-");
    }
    printf("\n");
    
    /*
     * Print Info
     */
    for(node_item  = opal_list_get_first(nodes);
        node_item != opal_list_get_end(nodes);
        node_item  = opal_list_get_next(node_item) ) {
        orte_ras_node_t *node;
        node = (orte_ras_node_t *)node_item;

        printf("%*s | ", len_name,    node->node_name);
        printf("%*s | ", len_arch,    (NULL == node->node_arch ?
                                       "" :
                                       node->node_arch));
        printf("%*d | ", len_cell,    node->node_cellid);
        printf("%*s | ", len_state,   pretty_node_state(node->node_state));
        printf("%*d | ", len_slots,   (uint)node->node_slots);
        printf("%*d | ", len_slots_m, (uint)node->node_slots_max);
        printf("%*d | ", len_slots_i, (uint)node->node_slots_inuse);
        printf("\n");
    }
    
    return ORTE_SUCCESS;
}

static int pretty_print_jobs(opal_list_t *jobs) {
    opal_list_item_t* job_item = NULL;
    int len_jobid = 0,
        len_state = 0,
        len_slots = 0,
        len_vpid_s = 0,
        len_vpid_r = 0,
        len_ckpt_s = 0,
        len_ckpt_r = 0,
        len_ckpt_l = 0;
    int i, line_len;

    for(job_item  = opal_list_get_first(jobs);
        job_item != opal_list_get_end(jobs);
        job_item  = opal_list_get_next(job_item) ) {
        orte_ps_job_info_t *job;
        job = (orte_ps_job_info_t *)job_item;

        /*
         * Caculate segment lengths
         */
        len_jobid  = 6;
        len_state  = (int) (strlen(pretty_job_state(job->state)) < strlen("State") ?
                            strlen("State") :
                            strlen(pretty_job_state(job->state)));
        len_slots  = 6;
        len_vpid_s = (int) strlen("VPID Start");
        len_vpid_r = (int) strlen("VPID Range");
#if OPAL_ENABLE_FT == 1
        len_ckpt_s = (int) (strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) < strlen("Ckpt State") ?
                            strlen("Ckpt State") :
                            strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) );
        len_ckpt_r = (int) (NULL == job->ckpt_ref ? strlen("Ckpt Ref") :
                            (strlen(job->ckpt_ref) < strlen("Ckpt Ref") ?
                             strlen("Ckpt Ref") :
                             strlen(job->ckpt_ref) ) );
        len_ckpt_l = (int) (NULL == job->ckpt_loc ? strlen("Ckpt Loc") :
                            (strlen(job->ckpt_loc) < strlen("Ckpt Loc") ?
                             strlen("Ckpt Loc") :
                             strlen(job->ckpt_loc) ) );
#else
        len_ckpt_s = -3;
        len_ckpt_r = -3;
        len_ckpt_l = 0;
#endif
        line_len = (len_jobid  + 3 +
                    len_state  + 3 +
                    len_slots  + 3 +
                    len_vpid_s + 3 +
                    len_vpid_r + 3 +
                    len_ckpt_s + 3 +
                    len_ckpt_r + 3 +
                    len_ckpt_l)
                    + 2;

        /*
         * Print Header
         */
        printf("\n");
        printf("%*s | ", len_jobid  , "JobID");
        printf("%*s | ", len_state  , "State");
        printf("%*s | ", len_slots  , "Slots");
        printf("%*s | ", len_vpid_s , "VPID Start");
        printf("%*s | ", len_vpid_r , "VPID Range");
#if OPAL_ENABLE_FT == 1
        printf("%*s | ", len_ckpt_s , "Ckpt State");
        printf("%*s | ", len_ckpt_r , "Ckpt Ref");
        printf("%*s |",  len_ckpt_l , "Ckpt Loc");
#endif
        printf("\n");

        for(i = 0; i < line_len; ++i) {
            printf("-");
        }
        printf("\n");

        /*
         * Print Info
         */
        printf("%*d | ",  len_jobid ,  job->id);
        printf("%*s | ",  len_state ,  pretty_job_state(job->state));
        printf("%*d | ",  len_slots ,  (uint)job->slots);
        printf("%*d | ",  len_vpid_s,  job->vpid_start);
        printf("%*d | ",  len_vpid_r,  job->vpid_range);
#if OPAL_ENABLE_FT == 1
        printf("%*s | ",  len_ckpt_s,  orte_snapc_ckpt_state_str(job->ckpt_state));
        printf("%*s | ",  len_ckpt_r,  (NULL == job->ckpt_ref ? 
                                        "" :
                                        job->ckpt_ref) );
        printf("%*s |",   len_ckpt_l,  (NULL == job->ckpt_loc ? 
                                        "" :
                                        job->ckpt_loc) );
#endif
        printf("\n");

        /*
         * Pretty print all VPID's in job
         */
        if(0 == job->id) { /* No vpids for the HNP */
            continue;
        }

        pretty_print_vpids(job);
    }
    
    return ORTE_SUCCESS;
}

static int pretty_print_vpids(orte_ps_job_info_t *job) {
    opal_list_item_t* vpid_item = NULL;
    int len_o_proc_name = 0, 
        len_proc_name   = 0, 
        len_rank        = 0, 
        len_pid         = 0,
        len_state       = 0,
        len_node        = 0,
        len_ckpt_s      = 0,
        len_ckpt_r      = 0,
        len_ckpt_l      = 0;
    int i, line_len;

    /*
     * Caculate segment lengths
     */
    len_o_proc_name = (int)strlen("ORTE Name");
    len_proc_name   = (int)strlen("Process Name");
    len_rank        = 6;
    len_pid         = 6;
    len_state       = 0;
    len_node        = 0;
#if OPAL_ENABLE_FT == 1
    len_ckpt_s      = strlen("Ckpt State");
    len_ckpt_r      = strlen("Ckpt Ref");
    len_ckpt_l      = strlen("Ckpt Loc");
#else
    len_ckpt_s      = -3;
    len_ckpt_r      = -3;
    len_ckpt_l      = 0;
#endif

    for(vpid_item  = opal_list_get_first(&(job->vpid_list));
        vpid_item != opal_list_get_end(&(job->vpid_list));
        vpid_item  = opal_list_get_next(vpid_item) ) {
        orte_ps_vpid_info_t *vpid;
        char *proc_name = NULL;
        vpid = (orte_ps_vpid_info_t *)vpid_item;
        
        /*
         * Find my app context
         */
        len_proc_name = len_proc_name;
        for( i = 0; i < (int)job->num_app_context; ++i) {
            if( job->app_context[i]->idx == vpid->app_context_idx ) {
                if( (int)strlen(job->app_context[i]->app) > len_proc_name) 
                    len_proc_name = strlen(job->app_context[i]->app);
                break;
            }
        }
        
        asprintf(&proc_name, "%d.%d.%d", vpid->name.cellid, vpid->name.jobid, vpid->name.vpid);
        if( (int)strlen(proc_name) > len_o_proc_name )
            len_o_proc_name = strlen(proc_name);
        
        if( (int)strlen(vpid->node) > len_node)
            len_node = strlen(vpid->node);
        
        if( (int)strlen(pretty_vpid_state(vpid->state)) > len_state)
            len_state = strlen(pretty_vpid_state(vpid->state));
        
#if OPAL_ENABLE_FT == 1
        if( (int)strlen(orte_snapc_ckpt_state_str(vpid->ckpt_state)) > len_ckpt_s)
            len_ckpt_s = strlen(orte_snapc_ckpt_state_str(vpid->ckpt_state));
        
        if( NULL != vpid->ckpt_ref &&
            (int)strlen(vpid->ckpt_ref) > len_ckpt_r) 
            len_ckpt_r = strlen(vpid->ckpt_ref);
        
        if( NULL != vpid->ckpt_loc &&
            (int)strlen(vpid->ckpt_loc) > len_ckpt_l) 
            len_ckpt_l = strlen(vpid->ckpt_loc);
#endif

        if( NULL != proc_name) {
            free(proc_name);
            proc_name = NULL;
        }
    }

    line_len = (len_o_proc_name + 3 +
                len_proc_name   + 3 +
                len_rank        + 3 +
                len_pid         + 3 +
                len_state       + 3 +
                len_node        + 3 +
                len_ckpt_s      + 3 +
                len_ckpt_r      + 3 +
                len_ckpt_l)
                + 2;

    /*
     * Print Header
     */
    printf("\t");
    printf("%*s | ", len_proc_name   , "Process Name");
    printf("%*s | ", len_o_proc_name , "ORTE Name");
    printf("%*s | ", len_rank        , "Rank");
    printf("%*s | ", len_pid         , "PID");
    printf("%*s | ", len_node        , "Node");
    printf("%*s | ", len_state       , "State");
#if OPAL_ENABLE_FT == 1
    printf("%*s | ", len_ckpt_s      , "Ckpt State");
    printf("%*s | ", len_ckpt_r      , "Ckpt Ref");
    printf("%*s |",  len_ckpt_l      , "Ckpt Loc");
#endif
    printf("\n");
    
    printf("\t");
    for(i = 0; i < line_len; ++i) {
        printf("-");
    }
    printf("\n");
    
    /*
     * Print Info
     */
    for(vpid_item  = opal_list_get_first(&(job->vpid_list));
        vpid_item != opal_list_get_end(&(job->vpid_list));
        vpid_item  = opal_list_get_next(vpid_item) ) {
        orte_ps_vpid_info_t *vpid;
        char *proc_name = NULL;
        vpid = (orte_ps_vpid_info_t *)vpid_item;
        
        printf("\t");

        asprintf(&proc_name, "%d.%d.%d", vpid->name.cellid, vpid->name.jobid, vpid->name.vpid);

        for( i = 0; i < (int)job->num_app_context; ++i) {
            if( job->app_context[i]->idx == vpid->app_context_idx ) {
                printf("%*s | ", len_proc_name, job->app_context[i]->app);
                break;
            }
        }
        
        printf("%*s | ",  len_o_proc_name, proc_name);
        printf("%*d | ",  len_rank       , (uint)vpid->rank);
        printf("%*d | ",  len_pid        , vpid->pid);
        printf("%*s | ",  len_node       , vpid->node);
        printf("%*s | ",  len_state      , pretty_vpid_state(vpid->state));
        
#if OPAL_ENABLE_FT == 1
        printf("%*s | ",  len_ckpt_s, orte_snapc_ckpt_state_str(vpid->ckpt_state));
        printf("%*s | ",  len_ckpt_r, (NULL == vpid->ckpt_ref ? 
                                       "" : 
                                       vpid->ckpt_ref));
        printf("%*s |",   len_ckpt_l, (NULL == vpid->ckpt_loc ? 
                                       "" : 
                                       vpid->ckpt_loc));
#endif
        printf("\n");
        
        if( NULL != proc_name) {
            free(proc_name);
            proc_name = NULL;
        }
    }
    
    return ORTE_SUCCESS;
}

static int connect_to_universe(orte_universe_t universe_info) {
    int ret, exit_status = ORTE_SUCCESS;
    char * univ_mca_param = NULL;

    /*
     * Construct the MCA parameter
     */
    asprintf(&univ_mca_param, "%s@%s:%s",
             universe_info.uid,
             universe_info.host,
             universe_info.name);
#if 0 /* JJH: Does not currently work as expected */
    /*
     * Disconnect from the current universe
     */
    if(orte_ps_globals.attached) {
        if (OPAL_SUCCESS != (ret = orte_system_finalize())) {
            return ret;
        }
    }
#endif
    /*
     * Set the environment universe information
     */
    opal_setenv(mca_base_param_env_var("universe"),
                univ_mca_param, true, &environ);

    /*
     * Restart ORTE in the requested universe
     */
    if(!orte_ps_globals.attached) {
        if (ORTE_SUCCESS != (ret = orte_system_init(ORTE_INFRASTRUCTURE, ORTE_NON_BARRIER)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        if( ORTE_SUCCESS != (ret = orte_restart(orte_process_info.my_name, universe_info.seed_uri)) ) {
            opal_output(orte_ps_globals.output,
                        "orte_ps: restart: FAILED (%d)\n", ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    orte_ps_globals.attached = true;

 cleanup:
    if( NULL != univ_mca_param)
        free(univ_mca_param);

    return exit_status;
}

static int gather_information(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;
    
    if( ORTE_SUCCESS != (ret = gather_active_jobs(universe) )) {
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = gather_nodes(universe) )) {
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = gather_job_info(universe) )) {
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = gather_vpid_info(universe) )) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int gather_active_jobs(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL;
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, j, num_values = 0;

    /**********************
     * Job Info segment
     **********************/
    segment = strdup(ORTE_JOBINFO_SEGMENT);

    if( ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                            segment,
                                            NULL,
                                            NULL,
                                            &num_values,
                                            &values ) ) ) {
        
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Parse the structure returned
     */
    for(i = 0; i < num_values; ++i) {
        orte_gpr_value_t* value = values[i];
        orte_ps_job_info_t *job = NULL;

        job = OBJ_NEW(orte_ps_job_info_t);
        orte_schema.extract_jobid_from_segment_name(&(job->id), value->tokens[0]);

        /*
         * If the user specified a jobid, then
         * only access the info for that jobid
         */
        if( 0 <= orte_ps_globals.jobid ) {
            if( (int)job->id != orte_ps_globals.jobid) {
                continue;
            }
        }

        for( j = 0; j < value->cnt; ++j) {
            orte_gpr_keyval_t* keyval = value->keyvals[j];
            orte_job_state_t *job_state;

            if( 0 == strncmp(keyval->key, ORTE_JOB_STATE_KEY, strlen(ORTE_JOB_STATE_KEY)) ) {
                if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &job_state, keyval->value, ORTE_JOB_STATE))) {
                    exit_status = ret;
                    goto cleanup;
                }

                job->state = *job_state;
                continue;
            }
        }

        opal_list_append(&universe->job_list, &(job->super));
    }

 cleanup:
    return exit_status;
}

static int gather_nodes(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;

    if( ORTE_SUCCESS != (ret = orte_ras_base_node_query(&(universe->nodes)))) {
        exit_status = ret;
    }
    
    return exit_status;
}

static int gather_job_info(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *tokens[2];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, j, num_values = 0;
    opal_list_item_t* item = NULL;

    /*
     * For each job in the universe
     */
    for(item  = opal_list_get_first(&(universe->job_list));
        item != opal_list_get_end(&(universe->job_list));
        item  = opal_list_get_next(item) ) {
        orte_ps_job_info_t *job;
        job = (orte_ps_job_info_t *)item;

        /*
         * Get the App Context(s)
         */
        orte_rmgr.get_app_context(job->id,
                                  &job->app_context,
                                  &job->num_app_context);
        /*
         * Access the job segment
         */
        orte_schema.get_job_segment_name(&segment, job->id);
        
        /*
         * Here we are just focused on the orte-job-globals container
         */
        tokens[0] = strdup(ORTE_JOB_GLOBALS);
        tokens[1] = NULL;
        
        if( ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                                segment,
                                                tokens,
                                                NULL,
                                                &num_values,
                                                &values ) ) ) {
            
            exit_status = ret;
            goto cleanup;
        }
        
        /*
         * Parse the structure returned
         */
        for(i = 0; i < num_values; ++i) {
            orte_gpr_value_t* value = values[i];
            
            for( j = 0; j < value->cnt; ++j) {
                orte_gpr_keyval_t* keyval = value->keyvals[j];
                orte_std_cntr_t *tmp_num;
                orte_vpid_t *tmp_vpid;

                if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_AT_INIT, strlen(ORTE_PROC_NUM_AT_INIT)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_init = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_LAUNCHED, strlen(ORTE_PROC_NUM_LAUNCHED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_launched = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_RUNNING, strlen(ORTE_PROC_NUM_RUNNING)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_running = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_FINALIZED, strlen(ORTE_PROC_NUM_FINALIZED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_finalized = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_TERMINATED, strlen(ORTE_PROC_NUM_TERMINATED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_terminated = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_ABORTED, strlen(ORTE_PROC_NUM_ABORTED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_aborted = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_SLOTS_KEY, strlen(ORTE_JOB_SLOTS_KEY)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_STD_CNTR))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->slots = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_VPID_START_KEY, strlen(ORTE_JOB_VPID_START_KEY)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_vpid, keyval->value, ORTE_VPID))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->vpid_start = *tmp_vpid;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_VPID_RANGE_KEY, strlen(ORTE_JOB_VPID_RANGE_KEY)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_vpid, keyval->value, ORTE_VPID))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->vpid_range = *tmp_vpid;
                    continue;
                }
#if OPAL_ENABLE_FT == 1
                else if( 0 == strncmp(keyval->key, ORTE_JOB_CKPT_STATE_KEY, strlen(ORTE_JOB_CKPT_STATE_KEY)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->ckpt_state = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_REF_KEY, strlen(ORTE_JOB_CKPT_SNAPSHOT_REF_KEY)) ) {
                    char *tmp_str = NULL;
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_str, keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->ckpt_ref = strdup(tmp_str);
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY, strlen(ORTE_JOB_CKPT_SNAPSHOT_LOC_KEY)) ) {
                    char *tmp_str = NULL;
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_str, keyval->value, ORTE_STRING))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->ckpt_loc = strdup(tmp_str);
                    continue;
                }
#endif
            }
        }
    }

 cleanup:
    return exit_status;
}

static int gather_vpid_info(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL;
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, j, num_values = 0;
    opal_list_item_t* job_item  = NULL;
    orte_vpid_t v = 0;

    /*
     * For each Job in the universe
     */
    for(job_item  = opal_list_get_first(&(universe->job_list));
        job_item != opal_list_get_end(&(universe->job_list));
        job_item  = opal_list_get_next(job_item) ) {
        orte_ps_job_info_t *job;
        job = (orte_ps_job_info_t *)job_item;

        /*
         * Skip getting the vpid's for the HNP, since the information is not complete
         */
        if( 0 == job->id) {
            continue;
        }

        /*
         * For each vpid in the job
         */
        for(v = job->vpid_start; v < (job->vpid_start + job->vpid_range); ++v) {
            orte_ps_vpid_info_t *vpid = NULL;
            orte_process_name_t proc;
            char **tokens = NULL;
            orte_std_cntr_t num_tokens = 0;

            /*
             * If the user specified a vpid, then just get that one
             */
            if( 0 <= orte_ps_globals.vpid) {
                /*
                 * Check to make sure it is a valid vpid
                 */
                if( (int)(job->vpid_start + job->vpid_range) <= orte_ps_globals.vpid) {
                    opal_show_help("help-orte-ps.txt", "invalid-vpid", true,
                                   orte_ps_globals.vpid,
                                   orte_ps_globals.jobid );
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }
                
                if( (int)v != orte_ps_globals.vpid ) {
                    continue;
                }
            }

            vpid = OBJ_NEW(orte_ps_vpid_info_t);
            
            /*
             * Access the job segment
             */
            orte_schema.get_job_segment_name(&segment, job->id);

            /*
             * Access the vpid container
             */
            proc.cellid = 0;
            proc.jobid  = job->id;
            proc.vpid   = v;
            
            orte_schema.get_proc_tokens(&tokens,
                                        &num_tokens,
                                        &proc);
            

            if( ORTE_SUCCESS != (ret = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                                    segment,
                                                    tokens,
                                                    NULL,
                                                    &num_values,
                                                    &values ) ) ) {
                
                exit_status = ret;
                goto cleanup;
            }
            
            /*
             * Parse the structure returned
             */
            for(i = 0; i < num_values; ++i) {
                orte_gpr_value_t* value = values[i];

                for( j = 0; j < value->cnt; ++j) {
                    orte_gpr_keyval_t* keyval = value->keyvals[j];
                    
                    if( 0 == strncmp(keyval->key, ORTE_PROC_RANK_KEY, strlen(ORTE_PROC_RANK_KEY)) ) {
                        orte_std_cntr_t *tmp_size;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_size, keyval->value, ORTE_VPID))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->rank = *tmp_size;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY, strlen(ORTE_PROC_APP_CONTEXT_KEY)) ) {
                        orte_std_cntr_t *tmp_size;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_size, keyval->value, ORTE_STD_CNTR))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->app_context_idx = *tmp_size;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY, strlen(ORTE_PROC_LOCAL_PID_KEY)) ) {
                        pid_t *tmp_pid;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_pid, keyval->value, ORTE_PID))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->pid = *tmp_pid;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_NAME_KEY, strlen(ORTE_PROC_NAME_KEY)) ) {
                        orte_process_name_t *tmp_proc;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_proc, keyval->value, ORTE_NAME))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->name.cellid = tmp_proc->cellid;
                        vpid->name.jobid  = tmp_proc->jobid;
                        vpid->name.vpid   = tmp_proc->vpid;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_NODE_NAME_KEY, strlen(ORTE_NODE_NAME_KEY)) ) {
                        char *tmp_node = NULL;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_node, keyval->value, ORTE_STRING))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->node = strdup(tmp_node);
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_STATE_KEY, strlen(ORTE_PROC_STATE_KEY)) ) {
                        orte_proc_state_t *tmp_state;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_state, keyval->value, ORTE_PROC_STATE))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->state = *tmp_state;
                        continue;
                    }
#if OPAL_ENABLE_FT == 1
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_CKPT_STATE_KEY, strlen(ORTE_PROC_CKPT_STATE_KEY)) ) {
                        size_t *tmp_state;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_state, keyval->value, ORTE_SIZE))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->ckpt_state = *tmp_state;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_REF_KEY, strlen(ORTE_PROC_CKPT_SNAPSHOT_REF_KEY)) ) {
                        char *tmp_str = NULL;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_str, keyval->value, ORTE_STRING))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->ckpt_ref = strdup(tmp_str);
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY, strlen(ORTE_PROC_CKPT_SNAPSHOT_LOC_KEY)) ) {
                        char *tmp_str = NULL;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_str, keyval->value, ORTE_STRING))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        vpid->ckpt_loc = strdup(tmp_str);
                        continue;
                    }
#endif
                }
            }

            /* A bit of dummy check - Can happen if the GPR is corrupted*/
            if( NULL == vpid->node ) {
                vpid->node = strdup("");
            }

#if OPAL_ENABLE_FT == 1
            if( NULL == vpid->ckpt_ref ) {
                vpid->ckpt_ref = strdup("");
            }
            if( NULL == vpid->ckpt_loc ) {
                vpid->ckpt_loc = strdup("");
            }
#endif

            opal_list_append(&job->vpid_list, &(vpid->super));
        }
    }

 cleanup:
    return exit_status;
}

/************************
 * Object handling
 ************************/
void orte_ps_vpid_info_construct(orte_ps_vpid_info_t *obj) {
    obj->node     = NULL;

#if OPAL_ENABLE_FT == 1
    obj->ckpt_ref = NULL;
    obj->ckpt_loc = NULL;
#endif
}

void orte_ps_vpid_info_destruct( orte_ps_vpid_info_t *obj) {
    if( NULL != obj->node) 
        free(obj->node);

#if OPAL_ENABLE_FT == 1
    if( NULL != obj->ckpt_ref) 
        free(obj->ckpt_ref);
    if( NULL != obj->ckpt_loc) 
        free(obj->ckpt_loc);
#endif
}

void orte_ps_job_info_construct(orte_ps_job_info_t *obj) {
    OBJ_CONSTRUCT(&obj->vpid_list, opal_list_t);

    obj->app_context = NULL;
    obj->num_app_context = 0;

#if OPAL_ENABLE_FT == 1
    obj->ckpt_ref = NULL;
    obj->ckpt_loc = NULL;
#endif
}

void orte_ps_job_info_destruct( orte_ps_job_info_t *obj) {
    opal_list_item_t* item = NULL;
    orte_std_cntr_t i;

#if OPAL_ENABLE_FT == 1
    if( NULL != obj->ckpt_ref)
        free(obj->ckpt_ref);
    if( NULL != obj->ckpt_loc)
        free(obj->ckpt_loc);
#endif
    
    for(i = 0; i < obj->num_app_context; ++i) {
        free(obj->app_context[i]);
    }
    obj->num_app_context = 0;

    while (NULL != (item = opal_list_remove_first(&obj->vpid_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&obj->vpid_list);
}

void orte_ps_universe_info_construct(orte_ps_universe_info_t *obj) {
    OBJ_CONSTRUCT(&obj->job_list, opal_list_t);
    OBJ_CONSTRUCT(&obj->nodes,    opal_list_t);
    OBJ_CONSTRUCT(&obj->universe_info, orte_universe_t);
}

void orte_ps_universe_info_destruct( orte_ps_universe_info_t *obj) {
    opal_list_item_t* item = NULL;
    
    while (NULL != (item = opal_list_remove_first(&obj->job_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&obj->job_list);

    while (NULL != (item = opal_list_remove_first(&obj->nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&obj->nodes);
    
    OBJ_DESTRUCT(&obj->universe_info);
}

static char *pretty_job_state(orte_job_state_t state) {
    switch(state) {
    case ORTE_JOB_STATE_INIT:
        return strdup("Init");
        break;
    case ORTE_JOB_STATE_LAUNCHED:
        return strdup("Launched");
        break;
    case ORTE_JOB_STATE_AT_STG1:
        return strdup("Stage 1");
        break;
    case ORTE_JOB_STATE_AT_STG2:
        return strdup("Stage 2");
        break;
    case ORTE_JOB_STATE_RUNNING:
        return strdup("Running");
        break;
    case ORTE_JOB_STATE_AT_STG3:
        return strdup("Stage 3");
        break;
    case ORTE_JOB_STATE_FINALIZED:
        return strdup("Finalized");
        break;
    case ORTE_JOB_STATE_TERMINATED:
        return strdup("Terminated");
        break;
    case ORTE_JOB_STATE_ABORTED:
        return strdup("Aborted");
        break;
    default:
        break;
    }
    
    return strdup("");
}

static char *pretty_vpid_state(orte_proc_state_t state) {
    switch(state) {
    case ORTE_PROC_STATE_INIT:
        return strdup("Init");
        break;
    case ORTE_PROC_STATE_LAUNCHED:
        return strdup("Launched");
        break;
    case ORTE_PROC_STATE_AT_STG1:
        return strdup("Stage 1");
        break;
    case ORTE_PROC_STATE_AT_STG2:
        return strdup("Stage 2");
        break;
    case ORTE_PROC_STATE_RUNNING:
        return strdup("Running");
        break;
    case ORTE_PROC_STATE_AT_STG3:
        return strdup("Stage 3");
        break;
    case ORTE_PROC_STATE_FINALIZED:
        return strdup("Finalized");
        break;
    case ORTE_PROC_STATE_TERMINATED:
        return strdup("Terminated");
        break;
    case ORTE_PROC_STATE_ABORTED:
        return strdup("Aborted");
        break;
    default:
        break;
    }
    
    return strdup("");
}

static char *pretty_univ_state(orte_universe_state_t state) {
    switch(state) {
    case ORTE_UNIVERSE_STATE_PRE_INIT:
        return strdup("Pre-Init");
        break;
    case ORTE_UNIVERSE_STATE_INIT:
        return strdup("Initializing");
        break;
    case ORTE_UNIVERSE_STATE_RUNNING:
        return strdup("Running");
        break;
    case ORTE_UNIVERSE_STATE_FINALIZE:
        return strdup("Finalized");
        break;
    default:
        return strdup("Unknown");
        break;
    }
}

static char *pretty_node_state(orte_node_state_t state) {
    switch(state) {
    case ORTE_NODE_STATE_DOWN:
        return strdup("Down");
        break;
    case ORTE_NODE_STATE_UP:
        return strdup("Up");
        break;
    case ORTE_NODE_STATE_REBOOT:
        return strdup("Reboot");
        break;
    case ORTE_NODE_STATE_UNKNOWN:
    default:
        return strdup("Unknown");
        break;
    }
}
