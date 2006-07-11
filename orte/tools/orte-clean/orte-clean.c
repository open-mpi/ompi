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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
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

#include "orte/orte_constants.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"

#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT == 1
#include "opal/runtime/opal_cr.h"
#endif
#include "orte/runtime/runtime.h"

extern char **environ;

/******************
 * Local Functions
 ******************/
static int orte_clean_init(void);
static int parse_args(int argc, char *argv[]);
static int orte_clean_check_universe(orte_universe_t *universe);
static int orte_clean_universe(orte_universe_t *universe);

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool verbose;
} orte_clean_globals_t;

orte_clean_globals_t orte_clean_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_clean_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_clean_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be Verbose" },

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
    opal_list_item_t* item = NULL;
    opal_list_t universe_search_result;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }

    if (ORTE_SUCCESS != (ret = orte_clean_init())) {
        exit_status = ret;
        goto cleanup;
    }

    OBJ_CONSTRUCT(&universe_search_result, opal_list_t);

    /*
     * Get the list of universes on this machine
     */
    if( orte_clean_globals.verbose ) {
        printf("orte_clean: Acquiring universe list...\n");
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
        orte_universe_t *search_result;
        search_result = (orte_universe_t *) item;

        /*
         * Try to connect to the universe
         */
        if( orte_clean_globals.verbose ) {
            printf("orte_clean: Connecting to universe: %s\n", search_result->name);
        }
        if( ORTE_SUCCESS == (ret = orte_clean_check_universe(search_result)) ) {
            /*
             * The universe was able to be contacted, so let it be
             */
            continue;
        }
        
        /*
         * If unable to connect to the universe,
         * clean it up!
         */
        if( orte_clean_globals.verbose ) {
            printf("orte_clean: Cleaning the session directory for universe: %s\n", search_result->name);
        }
        if( ORTE_SUCCESS != (ret = orte_clean_universe(search_result)) ){
            exit_status = ret;
            goto cleanup;
        }
    }

    /***************
     * Cleanup
     ***************/
 cleanup:
    while (NULL != (item = opal_list_remove_first(&universe_search_result))) {
        OBJ_RELEASE(item);
    }

    return exit_status;
}

static int parse_args(int argc, char *argv[]) {
    int i, ret, len;
    opal_cmd_line_t cmd_line;
    char **app_env = NULL, **global_env = NULL;
    orte_clean_globals_t tmp = { false, false };

    /* Parse the command line options */
    
    orte_clean_globals = tmp;
    
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

    opal_setenv(mca_base_param_env_var("crs_base_is_tool"),
                "1",
                true, &environ);

    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || 
        orte_clean_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-ps.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static int orte_clean_init(void) {
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

    if (ORTE_SUCCESS != (ret = orte_system_init(true))) {
        exit_status = ret;
        goto cleanup;
    }
#if 0
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
#endif

 cleanup:
    return exit_status;
}

static int orte_clean_universe(orte_universe_t *universe) {
    int ret, exit_status = ORTE_SUCCESS;
    char *fulldirpath = NULL;
    char *prefix = NULL;
    char *frontend = NULL;
    char *command = NULL;

    if( ORTE_SUCCESS != (ret = orte_session_dir_get_name(&fulldirpath,
                                                         &prefix,
                                                         &frontend,
                                                         universe->uid,
                                                         universe->host,
                                                         NULL, /* batch ID -- Not used */
                                                         universe->name,
                                                         NULL, /* jobid */
                                                         NULL  /* vpid */
                                                         ) ) ) {
        exit_status = ret;
        goto cleanup;
    }

    opal_os_dirpath_destroy( fulldirpath, true, NULL );

    /********************
     * If the session directory is empty, then remove that too
     * Need to check
     *  - openmpi-sessions-UID@gethostbyname()_0
     *  - openmpi-sessions-UID@localhost_0
     *  - remote nodes...
     ********************/

 cleanup:
    if( NULL != fulldirpath)
        free(fulldirpath);
    if( NULL != prefix)
        free(prefix);
    if( NULL != frontend)
        free(frontend);
    if( NULL != command)
        free(command);

    return exit_status;
}

static int orte_clean_check_universe(orte_universe_t *universe) {
    int ret, exit_status = ORTE_SUCCESS;
    struct timeval ping_wait = {2, 0};

    /*
     * Make sure session directory still exists
     */
    if (ORTE_SUCCESS != (ret = orte_session_dir(false,
                                                orte_process_info.tmpdir_base,
                                                universe->uid,
                                                universe->host,
                                                NULL, /* Batch ID -- Not used */
                                                universe->name,
                                                NULL, /* Jobid */
                                                NULL  /* VPID  */
                                                )) ) {
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    /*
     * Contact the HNP to see if it is still around
     */
    if( ORTE_SUCCESS != (ret = orte_rml.ping(universe->seed_uri, &ping_wait)) ) {
        exit_status = ORTE_ERR_CONNECTION_FAILED;
        goto cleanup;
    }
    
 cleanup:
    return exit_status;
}
