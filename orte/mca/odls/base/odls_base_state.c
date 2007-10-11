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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/util/sys_info.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/dss/dss.h"

#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


/*
 * Function for reporting the state and other process-related info
 * for newly spawned child processes
 */
int orte_odls_base_report_spawn(opal_list_t *children)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    char **tokens, *segment;
    orte_std_cntr_t num_tokens;
    orte_gpr_addr_mode_t mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR;
    orte_data_value_t dval = ORTE_DATA_VALUE_EMPTY;
    orte_buffer_t *buffer;
    int rc;
    
    buffer = OBJ_NEW(orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_gpr.begin_compound_cmd(buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    
    for (item = opal_list_get_first(children);
         item != opal_list_get_end(children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (ORTE_PROC_STATE_LAUNCHED == child->state) {
            /* when we launch the child, we need to store the pid
             * in addition to setting the state. Be sure to store
             * the pid first, though, as setting the state can
             * cause triggers to fire
             */
            if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, child->name))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buffer);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, child->name->jobid))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                OBJ_RELEASE(buffer);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&(child->pid), ORTE_PID))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                free(segment);
                OBJ_RELEASE(buffer);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_gpr.put_1(mode, segment, tokens, ORTE_PROC_LOCAL_PID_KEY, &dval))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                free(segment);
                OBJ_RELEASE(buffer);
                return rc;
            }
            dval.data = NULL;
            opal_argv_free(tokens);
            free(segment);
            
            /* now set the process state to LAUNCHED */
            if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_LAUNCHED, 0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buffer);
                return rc;
            }
        } else if (ORTE_PROC_STATE_FAILED_TO_START == child->state) {
            if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_FAILED_TO_START, child->exit_code))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buffer);
                return rc;
            }
        }
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.exec_compound_cmd(buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }
    OBJ_RELEASE(buffer);
    
    /* All done */
    return ORTE_SUCCESS;
}

/*
 * Preload all files for a single app context
 */
static int orte_odls_base_preload_append_binary(orte_app_context_t* context,
                                               orte_filem_base_request_t *filem_request);
static int orte_odls_base_preload_append_files(orte_app_context_t* context,
                                              orte_filem_base_request_t *filem_request);
static bool orte_odls_base_is_preload_local_dup(char *local_ref,
                                                orte_filem_base_request_t *filem_request);

int orte_odls_base_preload_files_app_context(orte_app_context_t* app_context)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_filem_base_request_t *filem_request;
    orte_filem_base_process_set_t *p_set = NULL;

    /* Sanity Check - Make sure there are files to preload */
    if(!app_context->preload_binary &&
       NULL == app_context->preload_files) {
        return exit_status;
    }

    filem_request = OBJ_NEW(orte_filem_base_request_t);

    /* Define the process set */
    p_set = OBJ_NEW(orte_filem_base_process_set_t);
    if( NULL == orte_process_info.gpr_replica ) {
        p_set->source.jobid = orte_process_info.my_name->jobid;
        p_set->source.vpid  = orte_process_info.my_name->vpid;
    }
    else {
        p_set->source.jobid = orte_process_info.gpr_replica->jobid;
        p_set->source.vpid  = orte_process_info.gpr_replica->vpid;
    }
    p_set->sink.jobid   = orte_process_info.my_name->jobid;
    p_set->sink.vpid    = orte_process_info.my_name->vpid;

    opal_list_append(&(filem_request->process_sets), &(p_set->super) );

    if(app_context->preload_binary) {
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s) Preload Binary...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if( ORTE_SUCCESS != (ret = orte_odls_base_preload_append_binary(app_context, 
                                                                       filem_request) ) ){
            opal_show_help("help-orte-odls-base.txt",
                           "orte-odls-base:could-not-preload-binary",
                           true, app_context->app);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            /* Keep accumulating files anyway */
        }
    }
    if( NULL != app_context->preload_files) {
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s) Preload Files... [%s]",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             app_context->preload_files));
        if( ORTE_SUCCESS != (ret = orte_odls_base_preload_append_files(app_context, 
                                                                      filem_request) ) ){
            opal_show_help("help-orte-odls-base.txt",
                           "orte-odls-base:could-not-preload-files",
                           true, app_context->preload_files);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            /* Keep accumulating files anyway */
        }
    }
    /* Actually bring over the files - One app context at a time 
     * JJH: This could be improved for multiple app contexts by making
     *      this a non-blocking filem get and then waiting on all of
     *      the requests for all app contexts.
     */
    if( ORTE_SUCCESS != (ret = orte_filem.get(filem_request)) ) {
        opal_show_help("help-orte-odls-base.txt",
                       "orte-odls-base:could-not-preload",
                       true,
                       (app_context->preload_binary ? app_context->app : ""),
                       (NULL != app_context->preload_files ? app_context->preload_files : ""));
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != filem_request ) {
        OBJ_RELEASE(filem_request);
        filem_request = NULL;
    }

    return exit_status;
}

/*
 * The difference between preloading a file, and a binary file is that 
 * we may need to update the app_context to reflect the placement of the binary file
 * on the local machine.
 */
static int orte_odls_base_preload_append_binary(orte_app_context_t* context, 
                                               orte_filem_base_request_t *filem_request) {
    char * local_bin = NULL;
    orte_filem_base_file_set_t * f_set = NULL;

    f_set = OBJ_NEW(orte_filem_base_file_set_t);

    /* Local Placement */
    asprintf(&local_bin, "%s/%s", orte_process_info.job_session_dir, opal_basename(context->app));
    if(orte_odls_base_is_preload_local_dup(local_bin, filem_request) ) {
        goto cleanup;
    }
    f_set->local_target = strdup(local_bin);

    /* Remote reference */
    f_set->remote_target = strdup(context->app);

    /* Flag as a single file */
    f_set->target_flag = ORTE_FILEM_TYPE_FILE;

    /* Add to the request list */
    opal_list_append(&(filem_request->file_sets), &(f_set->super) );

 cleanup:
    /*
     * Adjust the process name to point to the new local version
     */
    if( NULL != local_bin ) {
        if(NULL != context->app) {
            free(context->app);
            context->app = NULL;
        }

        context->app = strdup(local_bin);
        free(local_bin);
    }
    
    return ORTE_SUCCESS;
}


static int orte_odls_base_preload_append_files(orte_app_context_t* context, 
                                              orte_filem_base_request_t *filem_request) {
    char * local_ref = NULL;
    int i, remote_argc = 0;
    char **remote_targets = NULL;
    char * temp = NULL;
    orte_filem_base_file_set_t * f_set = NULL;

    remote_targets = opal_argv_split(context->preload_files, ',');
    remote_argc  = opal_argv_count(remote_targets);

    for(i = 0; i < remote_argc; ++i) {
        if(NULL != context->preload_files_dest_dir) {
            if(context->preload_files_dest_dir[0] == '.') {
                asprintf(&local_ref, "%s/%s/%s", context->cwd, context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
            else {
                asprintf(&local_ref, "%s/%s", context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
        }
        else {
            /* 
             * If the preload_files_dest_dir is not specified
             * If this is an absolute path, copy it to that path. Otherwise copy it to the cwd.
             */
            if('/' == remote_targets[i][0]) {
                asprintf(&local_ref, "%s", remote_targets[i]);
            } else {
                asprintf(&local_ref, "%s/%s", context->cwd, opal_basename(remote_targets[i]) );
            }
        }

        asprintf(&temp, "test -e %s", local_ref);
        if(0 == system(temp)) {
            char hostname[MAXHOSTNAMELEN];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-orte-odls-base.txt",
                           "orte-odls-base:preload-file-exists",
                           true, local_ref, hostname);
            free(temp);
            temp = NULL;
            free(local_ref);
            local_ref = NULL;
            continue;
        }
        free(temp);
        temp = NULL;
        
        /*
         * Is this a duplicate
         */
        if(orte_odls_base_is_preload_local_dup(local_ref, filem_request) ) {
            free(local_ref);
            local_ref = NULL;
            continue;
        }

        f_set = OBJ_NEW(orte_filem_base_file_set_t);

        /* Local Placement */
        f_set->local_target = strdup(local_ref);

        /* Remote reference */
        f_set->remote_target = strdup(remote_targets[i]);

        /* Flag as unknown, let FileM figure it out */
        f_set->target_flag = ORTE_FILEM_TYPE_UNKNOWN;

        /* Add to the request list */
        opal_list_append(&(filem_request->file_sets), &(f_set->super) );

        free(local_ref);
        local_ref = NULL;
    }

    if(NULL != local_ref)
        free(local_ref);
    if(NULL != remote_targets)
        opal_argv_free(remote_targets);

    return ORTE_SUCCESS;
}

/*
 * Keeps us from transfering the same file more than once.
 */
static bool orte_odls_base_is_preload_local_dup(char *local_ref,
                                                orte_filem_base_request_t *filem_request) {
    opal_list_item_t *item = NULL;

    for (item  = opal_list_get_first( &filem_request->file_sets);
         item != opal_list_get_end(   &filem_request->file_sets);
         item  = opal_list_get_next(   item) ) {
        orte_filem_base_file_set_t * f_set = (orte_filem_base_file_set_t*)item;

        if(0 == strncmp(local_ref, f_set->local_target, strlen(local_ref)+1) ) {
            return true;
        }
    }

    return false;
}
