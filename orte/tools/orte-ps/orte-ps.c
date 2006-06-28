/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

/*
 * JJH Temp workaround until this symbol is exported
 */
#define OPAL_ENABLE_FT 0

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <libgen.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <sys/types.h>
#include <dirent.h>

#include "orte/orte_constants.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/mca/gpr/gpr.h"
#if OPAL_ENABLE_FT == 1
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/rmgr/base/base.h"

#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT == 1
#include "opal/runtime/opal_cr.h"
#endif
#include "orte/runtime/runtime.h"


extern char **environ;

/*******************
 * Universe/job/vpid information Objects
 *******************/
struct orte_ps_vpid_info_t {
    /** This is an object, so it must have a super */
    opal_list_item_t super;
    
    /** General VPID Information */
    size_t rank;
    pid_t pid;
    orte_process_name_t name;
    char * node;
    orte_proc_state_t state;
    
    size_t app_context_idx;

#if OPAL_ENABLE_FT == 1
    size_t ckpt_state;
    char *ckpt_ref;
    char *ckpt_loc;
#endif
};
typedef struct orte_ps_vpid_info_t orte_ps_vpid_info_t;

OBJ_CLASS_DECLARATION(orte_ps_vpid_info_t);

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

    size_t num_init;
    size_t num_launched;
    size_t num_running;
    size_t num_finalized;
    size_t num_terminated;
    size_t num_aborted;
    size_t slots;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;

#if OPAL_ENABLE_FT == 1
    size_t ckpt_state;
    char *ckpt_ref;
    char *ckpt_loc;
#endif

    orte_app_context_t **app_context;
    size_t num_app_context;

    /** List of vpids */
    opal_list_t vpid_list;
};
typedef struct orte_ps_job_info_t orte_ps_job_info_t;

OBJ_CLASS_DECLARATION(orte_ps_job_info_t);

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
};
typedef struct orte_ps_universe_info_t orte_ps_universe_info_t;

OBJ_CLASS_DECLARATION(orte_ps_universe_info_t);

void orte_ps_universe_info_construct(orte_ps_universe_info_t *obj);
void orte_ps_universe_info_destruct( orte_ps_universe_info_t *obj);

OBJ_CLASS_INSTANCE(orte_ps_universe_info_t,
                   opal_list_item_t,
                   orte_ps_universe_info_construct,
                   orte_ps_universe_info_destruct);


/******************
 * Local Functions
 ******************/
static int orte_ps_init(void);
static int parse_args(int argc, char *argv[]);

static int connect_to_universe(orte_universe_t universe_info);

static int gather_information(orte_ps_universe_info_t* universe);

static int pretty_print(orte_ps_universe_info_t* universe);
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
    int ret, exit_status;
    opal_list_t universe_list;
    opal_list_item_t* item = NULL;
    opal_list_t universe_search_result;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }

    if (ORTE_SUCCESS != (ret = orte_ps_init())) {
        exit_status = ret;
        goto cleanup;
    }

    OBJ_CONSTRUCT(&universe_list, opal_list_t);
    OBJ_CONSTRUCT(&universe_search_result, opal_list_t);

    /*
     * Get the directory listing
     */
    if( orte_ps_globals.verbose ) {
        printf("orte_ps: Acquiring universe list...\n");
    }
    if (ORTE_SUCCESS != (ret = orte_universe_search(&universe_search_result) ) ) {
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
        if( orte_ps_globals.verbose ) {
            printf("orte_ps: Connecting to universe: %s\n", univ->universe_info.name);
        }
        if( ORTE_SUCCESS != (ret = connect_to_universe(univ->universe_info)) ) {
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Gather the information
         */
        if( orte_ps_globals.verbose ) {
            printf("orte_ps: Gathering Universe Information\n");
        }
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

    return exit_status;
}

static int parse_args(int argc, char *argv[]) {
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    orte_ps_globals_t tmp = { false, false, NULL, -1, -1, false};

    /* Parse the command line options */
    
    orte_ps_globals = tmp;
    
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
    opal_setenv(mca_base_param_env_var("crs_base_is_tool"),
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

static int orte_ps_init(void) {
    int exit_status = ORTE_SUCCESS, ret;

    /*
     * We are trying to attach to another process' GPR so we need to 
     * attach no matter if it is identified as private or not.
     */
    opal_setenv(mca_base_param_env_var("universe_console"),
                "1",
                true, &environ);

#if OPAL_ENABLE_FT == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_is_enabled(false);

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
    opal_list_item_t* job_item = NULL;
    opal_list_item_t* vpid_item = NULL;
    int len_opn = 0, 
        len_pn = 0, 
        len_r  = 0, 
        len_p  = 0,
        len_s  = 0,
#if OPAL_ENABLE_FT == 1
        len_cs = 0,
        len_cr = 0,
        len_cl = 0,
#endif
        len_n  = 0;

    int i, line_len;

    printf("------------------------------------------------------\n");
    printf("------------------------------------------------------\n");
    printf("%*s | ", (int)strlen(universe->universe_info.name) , "Universe Name");
    printf("%*s | ", (int)strlen(universe->universe_info.host) , "Hostname");
    printf("%*s | ", (int)strlen(universe->universe_info.uid)  , "uid");
    printf("%*s | ", 11  , "Persistent");
    printf("%*s | ", (int)strlen(universe->universe_info.scope)  , "Scope");
    printf(" State");
    printf("\n");

    line_len = (strlen(universe->universe_info.name) + 3 +
                strlen(universe->universe_info.host) + 3 +
                strlen(universe->universe_info.uid)  + 3 +
                11 + 3 +
                strlen(universe->universe_info.scope) + 3 +
                6 + 3);
    for(i = 0; i < line_len; ++i) {
        printf("-");
    }
    printf("\n");

    printf("%s | ", universe->universe_info.name);
    printf("%s | ", universe->universe_info.host);
    printf("%s | ", universe->universe_info.uid);

    if(universe->universe_info.persistence)
        printf("%*s | ", 11, "true");
    else 
        printf("%*s | ", 11, "false");

    printf("%s | ", universe->universe_info.scope);

    switch(universe->universe_info.state) {
    case ORTE_UNIVERSE_STATE_PRE_INIT:
        printf("Pre-Init");
        break;
    case ORTE_UNIVERSE_STATE_INIT:
        printf("Initializing");
        break;
    case ORTE_UNIVERSE_STATE_RUNNING:
        printf("Running");
        break;
    case ORTE_UNIVERSE_STATE_FINALIZE:
        printf("Finalized");
        break;
    default:
        printf("Unknown");
        break;
    }
    printf("\n");

#if 0
    printf("\tSeed URI   :\t %s\n", universe->universe_info.seed_uri);
    printf("\tScriptfile :\t %s\n", universe->universe_info.scriptfile);
#endif

    printf("\n");

    /*
     * Print job information
     */
    for(job_item  = opal_list_get_first(&(universe->job_list));
        job_item != opal_list_get_end(&(universe->job_list));
        job_item  = opal_list_get_next(job_item) ) {
        orte_ps_job_info_t *job;
        job = (orte_ps_job_info_t *)job_item;

        printf("\n");
        printf("%*s | ", 6 , "JobID");
        printf("%*s | ", (int)strlen(pretty_job_state(job->state)) , "State");
        printf("%*s | ", 6 , "Slots");
        printf("%*s | ", 10 , "VPID start");
        printf("%*s | ", 10 , "VPID range");
#if 0
        printf("%*s | ", 10 , "Num Init");
        printf("%*s | ", 10 , "Num Launched");
        printf("%*s | ", 10 , "Num Running");
        printf("%*s | ", 10 , "Num Finalized");
        printf("%*s | ", 10 , "Num Terminated");
        printf("%*s | ", 10 , "Num aborted");
#endif
#if OPAL_ENABLE_FT == 1
        printf("%*s | ", (int)strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) , "Ckpt State");
        printf("%*s | ", (int)(NULL == job->ckpt_ref ? strlen("Ckpt Ref") : strlen(job->ckpt_ref)) , "Ckpt Ref");
        printf("%*s",    (int)(NULL == job->ckpt_loc ? strlen("Ckpt Loc") : strlen(job->ckpt_loc)) , "Ckpt Loc");
#endif
        printf("\n");

        line_len = (6  + 3 +
                    strlen(pretty_job_state(job->state)) + 3 +
                    6  + 3 +
                    10 + 3 +
                    10 + 3 +
#if OPAL_ENABLE_FT == 1
                    (strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) < strlen("Ckpt State") ?
                     strlen("Ckpt State") : 
                     strlen(orte_snapc_ckpt_state_str(job->ckpt_state))) + 3 +
                    (NULL == job->ckpt_ref || strlen(job->ckpt_ref) < strlen("Ckpt Ref") ? 
                     strlen("Ckpt Ref") : 
                     strlen(job->ckpt_ref)) + 3 +
                    (NULL == job->ckpt_loc || strlen(job->ckpt_loc) < strlen("Ckpt Loc") ? 
                     strlen("Ckpt Loc") : 
                     strlen(job->ckpt_loc)) + 3
#else
                    0
#endif
                    );
        for(i = 0; i < line_len; ++i) {
            printf("-");
        }
        printf("\n");

        printf("%*d | ",  6,  job->id);
        printf("%s | ",       pretty_job_state(job->state));
        printf("%*lu | ", 6,  job->slots);
        printf("%*d | ",  10, job->vpid_start);
        printf("%*d | ",  10, job->vpid_range);
#if 0
        printf("%*lu | ", 10, job->num_init);
        printf("%*lu | ", 10, job->num_launched);
        printf("%*lu | ", 10, job->num_running);
        printf("%*lu | ", 10, job->num_finalized);
        printf("%*lu | ", 10, job->num_terminated);
        printf("%*lu | ", 10, job->num_aborted);
#endif
#if OPAL_ENABLE_FT == 1
        printf("%*s | ", 
               (strlen("Ckpt State") > strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) ?
                (int)strlen("Ckpt State") : (int)strlen(orte_snapc_ckpt_state_str(job->ckpt_state)) ),
               orte_snapc_ckpt_state_str(job->ckpt_state));
        if( NULL == job->ckpt_ref ) {
            printf("%*s", (int)strlen("Ckpt Ref"), "");
        }
        else {
            printf("%*s", 
                   (strlen("Ckpt Ref") > strlen(job->ckpt_ref) ?
                    (int)strlen("Ckpt Ref") : (int)strlen(job->ckpt_ref) ),
                   job->ckpt_ref);
        }
        printf(" | ");
        if( NULL == job->ckpt_loc ) {
            printf("%*s", (int)strlen("Ckpt Loc"), "");
        }
        else {
            printf("%*s", 
                   (strlen("Ckpt Loc") > strlen(job->ckpt_loc) ?
                    (int)strlen("Ckpt Loc") : (int)strlen(job->ckpt_loc) ),
                   job->ckpt_loc);
        }
#endif
        printf("\n");

        if(0 == job->id) { /* No vpids for the HNP */
            continue;
        }

        /*
         * For each VPID in the job
         */
        /* First time through to get the max len */
        len_pn  = strlen("Process Name");
        len_opn = strlen("ORTE Name");
        len_r   = 6;
        len_p   = 6;
        len_s   = 0;
#if OPAL_ENABLE_FT == 1
        len_cs  = strlen("Ckpt State");
        len_cr  = strlen("Ckpt Ref");
        len_cl  = strlen("Ckpt Loc");
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
            for( i = 0; i < (int)job->num_app_context; ++i) {
                if( job->app_context[i]->idx == vpid->app_context_idx ) {
                    if( (int)strlen(job->app_context[i]->app) > len_pn) 
                        len_pn = strlen(job->app_context[i]->app);
                    break;
                }
            }

            asprintf(&proc_name, "%d.%d.%d", vpid->name.cellid, vpid->name.jobid, vpid->name.vpid);
            if( (int)strlen(proc_name) > len_opn )
                len_opn = strlen(proc_name);
            
            if( (int)strlen(vpid->node) > len_n)
                len_n = strlen(vpid->node);

            if( (int)strlen(pretty_vpid_state(vpid->state)) > len_s)
                len_s = strlen(pretty_vpid_state(vpid->state));

#if OPAL_ENABLE_FT == 1
            if( (int)strlen(orte_snapc_ckpt_state_str(vpid->ckpt_state)) > len_cs)
                len_cs = strlen(orte_snapc_ckpt_state_str(vpid->ckpt_state));

            if( NULL != vpid->ckpt_ref &&
                (int)strlen(vpid->ckpt_ref) > len_cr) 
                len_cr = strlen(vpid->ckpt_ref);

            if( NULL != vpid->ckpt_loc &&
                (int)strlen(vpid->ckpt_loc) > len_cl) 
                len_cl = strlen(vpid->ckpt_loc);
#endif
            if( NULL != proc_name) {
                free(proc_name);
                proc_name = NULL;
            }
        }

        printf("\t");
        printf("%*s | ", len_pn , "Process Name");
        printf("%*s | ", len_opn, "ORTE Name");
        printf("%*s | ", len_r  , "Rank");
        printf("%*s | ", len_p  , "PID");
        printf("%*s | ", len_n  , "Node");
        printf("%*s | ", len_s  , "State");
#if OPAL_ENABLE_FT == 1
        printf("%*s | ", len_cs , "Ckpt State");
        printf("%*s | ", len_cr , "Ckpt Ref");
        printf("%*s",    len_cl , "Ckpt Loc");
#endif
        printf("\n");

        line_len = (len_pn + 3 +
                    len_opn + 3 +
                    len_r  + 3 +
                    len_p  + 3 +
                    len_s  + 3 +
                    len_n  + 3 +
#if OPAL_ENABLE_FT == 1
                    len_cs + 3 +
                    len_cr + 3 +
                    len_cl + 3
#else
                    0
#endif
                    );
        printf("\t");
        for(i = 0; i < line_len; ++i) {
            printf("-");
        }
        printf("\n");

        for(vpid_item  = opal_list_get_first(&(job->vpid_list));
            vpid_item != opal_list_get_end(&(job->vpid_list));
            vpid_item  = opal_list_get_next(vpid_item) ) {
            orte_ps_vpid_info_t *vpid;
            char *proc_name = NULL;
            vpid = (orte_ps_vpid_info_t *)vpid_item;

            asprintf(&proc_name, "%d.%d.%d", vpid->name.cellid, vpid->name.jobid, vpid->name.vpid);
            
            printf("\t");

            for( i = 0; i < (int)job->num_app_context; ++i) {
                if( job->app_context[i]->idx == vpid->app_context_idx ) {
                    printf("%*s | ", len_pn, job->app_context[i]->app);
                    break;
                }
            }

            printf("%*s | ",  len_opn, proc_name);
            printf("%*lu | ", len_r,  vpid->rank);
            printf("%*d | ",  len_p,  vpid->pid);
            printf("%*s | ",  len_n,  vpid->node);
            printf("%s | ",  pretty_vpid_state(vpid->state));

#if OPAL_ENABLE_FT == 1
            printf("%*s | ",  len_cs, orte_snapc_ckpt_state_str(vpid->ckpt_state));
            printf("%*s | ",  len_cr, (NULL == vpid->ckpt_ref ? "" : vpid->ckpt_ref));
            printf("%*s",     len_cl, (NULL == vpid->ckpt_loc ? "" : vpid->ckpt_loc));
#endif
            printf("\n");

            if( NULL != proc_name) {
                free(proc_name);
                proc_name = NULL;
            }
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
#if 0
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
                univ_mca_param,
                true, &environ);

    /*
     * Restart ORTE in the requested universe
     */
    if(!orte_ps_globals.attached) {
        if (ORTE_SUCCESS != (ret = orte_system_init(true)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        if( ORTE_SUCCESS != (ret = orte_restart(orte_process_info.my_name, universe_info.seed_uri)) ) {
            printf("orte_restart: FAILED (%d)\n", ret);
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

static int gather_active_jobs(orte_ps_universe_info_t* universe);
static int gather_nodes(orte_ps_universe_info_t* universe);
static int gather_job_info(orte_ps_universe_info_t* universe);
static int gather_vpid_info(orte_ps_universe_info_t* universe);

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
    size_t i, j, num_values = 0;

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
    return ORTE_SUCCESS;
}

static int gather_job_info(orte_ps_universe_info_t* universe) {
    int ret, exit_status = ORTE_SUCCESS;
    char *segment = NULL, *tokens[2];
    orte_gpr_value_t** values = NULL;
    size_t i, j, num_values = 0;
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
        orte_rmgr_base_get_app_context(job->id,
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
                size_t *tmp_num;
                orte_vpid_t *tmp_vpid;

                if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_AT_INIT, strlen(ORTE_PROC_NUM_AT_INIT)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_init = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_LAUNCHED, strlen(ORTE_PROC_NUM_LAUNCHED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_launched = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_RUNNING, strlen(ORTE_PROC_NUM_RUNNING)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_running = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_FINALIZED, strlen(ORTE_PROC_NUM_FINALIZED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_finalized = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_TERMINATED, strlen(ORTE_PROC_NUM_TERMINATED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_terminated = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_PROC_NUM_ABORTED, strlen(ORTE_PROC_NUM_ABORTED)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
                        exit_status = ret;
                        goto cleanup;
                    }
                    job->num_aborted = *tmp_num;
                    continue;
                }
                else if( 0 == strncmp(keyval->key, ORTE_JOB_SLOTS_KEY, strlen(ORTE_JOB_SLOTS_KEY)) ) {
                    if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_num, keyval->value, ORTE_SIZE))) {
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
    size_t i, j, num_values = 0;
    opal_list_item_t* job_item  = NULL;
    size_t v = 0;

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
            size_t num_tokens = 0;

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
                    return ORTE_ERROR;
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
                        size_t *tmp_size;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_size, keyval->value, ORTE_SIZE))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        
                        vpid->rank = *tmp_size;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY, strlen(ORTE_PROC_APP_CONTEXT_KEY)) ) {
                        size_t *tmp_size;
                        if( ORTE_SUCCESS != (ret = orte_dss.get( (void **) &tmp_size, keyval->value, ORTE_SIZE))) {
                            exit_status = ret;
                            goto cleanup;
                        }
                        
                        vpid->app_context_idx = *tmp_size;
                        continue;
                    }
                    else if( 0 == strncmp(keyval->key, ORTE_PROC_PID_KEY, strlen(ORTE_PROC_PID_KEY)) ) {
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

#if OPAL_ENABLE_FT == 1
    obj->ckpt_ref = NULL;
    obj->ckpt_loc = NULL;
#endif

    obj->app_context = NULL;
    obj->num_app_context = 0;
}

void orte_ps_job_info_destruct( orte_ps_job_info_t *obj) {
    opal_list_item_t* item = NULL;
    size_t i;

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
    OBJ_CONSTRUCT(&obj->universe_info, orte_universe_t);
}

void orte_ps_universe_info_destruct( orte_ps_universe_info_t *obj) {
    opal_list_item_t* item = NULL;
    
    while (NULL != (item = opal_list_remove_first(&obj->job_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&obj->job_list);
    
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
    
    return NULL;
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
    
    return NULL;
}
