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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "orte/orte_constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/gpr/gpr.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

#include "filem_rsh.h"

/**********
 * Local Function and Variable Declarations
 **********/
static int orte_filem_base_rsh_copy(orte_filem_base_request_t *request, bool put_func);

static int orte_filem_rsh_query_remote_path(char **remote_ref, orte_process_name_t *proc, int *flag);
static void orte_filem_rsh_query_callback(int status,
                                          orte_process_name_t* sender,
                                          orte_buffer_t *buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata);

/*
 * Rsh module
 */
static orte_filem_base_module_t loc_module = {
    /** Initialization Function */
    orte_filem_rsh_module_init,
    /** Finalization Function */
    orte_filem_rsh_module_finalize,

    orte_filem_base_rsh_put,
    orte_filem_base_rsh_get,
    orte_filem_base_rsh_rm
};

/*
 * MCA Functions
 */
orte_filem_base_module_1_0_0_t *
orte_filem_rsh_component_query(int *priority)
{
    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: component_query()");

    *priority = mca_filem_rsh_component.super.priority;

    return &loc_module;
}

int orte_filem_rsh_module_init(void)
{
    int ret;

    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: module_init()");
    
    if( ORTE_SUCCESS != (ret = orte_filem_base_listener_init(orte_filem_rsh_query_callback) ) ) {
        return ret;
    }
    
    return ORTE_SUCCESS;
}

int orte_filem_rsh_module_finalize(void)
{
    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: module_finalize()");

    orte_filem_base_listener_cancel();

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/
int orte_filem_base_rsh_put(orte_filem_base_request_t *request)
{
    int ret;

    if( ORTE_SUCCESS != (ret = orte_filem_base_rsh_copy(request, true) ) ) {
        return ret;
    }

    return ORTE_SUCCESS;
}

int orte_filem_base_rsh_get(orte_filem_base_request_t *request)
{
    int ret;

    if( ORTE_SUCCESS != (ret = orte_filem_base_rsh_copy(request, false) ) ) {
        return ret;
    }

    return ORTE_SUCCESS;
}
 
static int orte_filem_base_rsh_copy(orte_filem_base_request_t *request, bool put_func) {
    int ret = ORTE_SUCCESS, exit_status = ORTE_SUCCESS;
    char *command        = NULL;
    char *remote_machine = NULL;
    char *remote_file    = NULL;
    char *dir_arg        = NULL;
    int fn, pn;

    /* For each process: */
    for( pn = 0; pn < request->num_procs; ++pn) {
        /*
         * Get the remote machine identifier from the process_name struct
         */
        if( ORTE_SUCCESS != (ret = orte_filem_base_get_proc_node_name(&request->proc_name[pn], &remote_machine))) {
            exit_status = ret;
            goto cleanup;
        }

        /* For each file: */
        for( fn = 0; fn < request->num_targets; ++fn) {
            /*
             * Fix the remote_filename.
             * If it is an absolute path, then assume it is valid for the remote server
             * ow then we must construct the correct path.
             */
            remote_file = strdup(request->remote_targets[fn]);
            if( ORTE_SUCCESS != (ret = orte_filem_rsh_query_remote_path(&remote_file, &request->proc_name[pn], &request->target_flags[fn]) ) ) {
                exit_status = ret;
                goto cleanup;
            }
            
            /*
             * Transfer the file or directory
             */
            if(ORTE_FILEM_TYPE_DIR == request->target_flags[fn]) {
                dir_arg = strdup(" -r ");
            }
            else if(ORTE_FILEM_TYPE_UNKNOWN == request->target_flags[fn]) {
                continue;
            }
            else {
                dir_arg = strdup("");
            }

            /*
             * If this is the put() routine
             */
            if( put_func ) {
                asprintf(&command, "%s %s %s %s:%s > /dev/null", 
                         mca_filem_rsh_component.cp_command, 
                         dir_arg, 
                         request->local_targets[fn],
                         remote_machine, 
                         remote_file);
                opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                                    "filem:rsh:put about to execute %s", command);
                if( 0 > (ret = system(command) ) ) {
                    exit_status = ret;
                    goto cleanup;
                }
            }
            /*
             * ow it is the get() routine
             */
            else {
                asprintf(&command, "%s %s %s:%s %s > /dev/null", 
                         mca_filem_rsh_component.cp_command, 
                         dir_arg, 
                         remote_machine, 
                         remote_file,
                         request->local_targets[fn]);
                
                opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                                    "filem:rsh:get about to execute %s", command);
                
                /* TBP: This needs to be fixed at some point. If scp can not authorize, it
                 * returns an error code of 256 
                 * JJH: Implement a bound on the loop, but still would like something better.
                 */
                {
                    int wait_limit = 30;
                    int wait_i = 0;
                    while(256 == (ret = system(command) ) ) {
                        sleep(1);

                        ++wait_i;
                        if(wait_i >= wait_limit) {
                            opal_output(mca_filem_rsh_component.super.output_handle,
                                        "filem:rsh:get %s failed after %d attempts and returned %d", 
                                        mca_filem_rsh_component.cp_command,
                                        wait_limit,
                                        ret);
                            break;
                        }
                    }
                }
                if( 0 != ret) {
                    opal_output(mca_filem_rsh_component.super.output_handle,
                                "filem:rsh:get %s failed and returned %d", 
                                mca_filem_rsh_component.cp_command,
                                ret);
                    exit_status = ret;
                    goto cleanup;
                }
            }

            /* A small bit of cleanup */
            if( NULL != dir_arg) {
                free(dir_arg);
                dir_arg = NULL;
            }
            if( NULL != remote_file) {
                free(remote_file);
                remote_file = NULL;
            }
        }

        /* a small bit of cleanup */
        if(NULL != remote_machine) {
            free(remote_machine);
            remote_machine = NULL;
        }
    }
 cleanup:
    if( NULL != command )
        free(command);
    if( NULL != remote_machine)
        free(remote_machine);
    if( NULL != dir_arg) 
        free(dir_arg);
    if( NULL != remote_file)
        free(remote_file);
        
    return ret;
}

int orte_filem_base_rsh_rm(orte_filem_base_request_t *request)
{
    int ret = ORTE_SUCCESS, exit_status = ORTE_SUCCESS;
    char *command        = NULL;
    char *remote_machine = NULL;
    char *remote_targets   = NULL;
    char *dir_arg        = NULL;
    char **remote_file_set = NULL;
    char *tmp_file         = NULL;
    int fn, pn, argc = 0;


    /* For each process: */
    for( pn = 0; pn < request->num_procs; ++pn) {
        /*
         * Get the remote machine identifier from the process_name struct
         */
        if( ORTE_SUCCESS != (ret = orte_filem_base_get_proc_node_name(&request->proc_name[pn], &remote_machine))) {
            exit_status = ret;
            goto cleanup;
        }

        /* For each file: */
        for( fn = 0; fn < request->num_targets; ++fn) {

            tmp_file = strdup(request->remote_targets[fn]);

            /*
             * Fix the remote_filename.
             * If it is an absolute path, then assume it is valid for the remote server
             * ow then we must construct the correct path.
             */
            if( ORTE_SUCCESS != (ret = orte_filem_rsh_query_remote_path(&tmp_file, &request->proc_name[pn], &request->target_flags[fn]) ) ) {
                exit_status = ret;
                goto cleanup;
            }
            
            if(ORTE_FILEM_TYPE_UNKNOWN == request->target_flags[fn]) {
                continue;
            }

            opal_argv_append(&argc, &remote_file_set, tmp_file);
            
            /*
             * If we are removing a directory in the mix, then we
             * need the recursive argument.
             */
            if(NULL == dir_arg) {
                if(ORTE_FILEM_TYPE_DIR == request->target_flags[fn]) {
                    dir_arg = strdup(" -rf ");
                }
            }
        }

        if(NULL == dir_arg)
            dir_arg = strdup(" -f ");
        
        remote_targets = opal_argv_join(remote_file_set, ' ');
        asprintf(&command, "%s %s rm %s %s > /dev/null", 
                 mca_filem_rsh_component.remote_sh_command, 
                 remote_machine,
                 dir_arg,
                 remote_targets);
        
        opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                            "filem:rsh:rm about to execute %s", command);
        if( 0 > (ret = system(command) ) ) {
            exit_status = ret;
            goto cleanup;
        }
        
        /* A small bit of cleanup */
        if( NULL != dir_arg) {
            free(dir_arg);
            dir_arg = NULL;
        }

        if( NULL != remote_targets) {
            free(remote_targets);
            remote_targets = NULL;
        }

        if( NULL != remote_file_set) {
            opal_argv_free(remote_file_set);
            remote_file_set = NULL;
        }

        if(NULL != remote_machine) {
            free(remote_machine);
            remote_machine = NULL;
        }
    }

 cleanup:
    if( NULL != command )
        free(command);
    if( NULL != remote_machine)
        free(remote_machine);
    if( NULL != dir_arg) 
        free(dir_arg);
    if( NULL != remote_targets)
        free(remote_targets);
    if( NULL != remote_file_set)
        opal_argv_free(remote_file_set);
    if( NULL != tmp_file)
        free(tmp_file);

    return ret;
}

/******************
 * Local Functions
 ******************/
/*
 * This function is paired with the orte_filem_rsh_query_callback() function on the remote machine
 */
static int orte_filem_rsh_query_remote_path(char **remote_ref, orte_process_name_t *peer, int *flag) {
    int ret;

#if 1 /* JJH: Some debugging */
    /* If it is an absolute path */
    if( *remote_ref[0] == '/' ) {
        *flag = ORTE_FILEM_TYPE_DIR;
        return ORTE_SUCCESS;
    }
#endif

    /* Put our listener on hold */
    orte_filem_base_listener_cancel();

    /* Call the base function */
    if( ORTE_SUCCESS != (ret = orte_filem_base_query_remote_path(remote_ref, peer, flag) ) ) {
        return ret;
    }

    /* Reset the listener */
    if( ORTE_SUCCESS != (ret = orte_filem_base_listener_init(orte_filem_rsh_query_callback) ) ) {
        return ret;
    }

    return ORTE_SUCCESS;
}

/*
 * This function is paired with the orte_filem_rsh_query_remote_path() function on the 
 * requesting machine.
 * This function is responsible for:
 * - Constructing the remote absolute path for the specified file/dir
 * - Verify the existence of the file/dir
 * - Determine if the specified file/dir is in fact a file or dir or unknown if not found.
 * 
 */
static void orte_filem_rsh_query_callback(int status,
                                          orte_process_name_t* peer,
                                          orte_buffer_t *buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata) 
{
    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: query_callback(%s -> %s)",
                        ORTE_NAME_PRINT(orte_process_info.my_name),
                        ORTE_NAME_PRINT(peer));

    /* Call the base callback function */
    orte_filem_base_query_callback(status, peer, buffer, tag, cbdata);

    /* Reset the listener */
    orte_filem_base_listener_init(orte_filem_rsh_query_callback);
}
