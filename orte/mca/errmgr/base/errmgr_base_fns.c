/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdarg.h>

#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "opal/util/opal_sos.h"

#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

/*
 * Public interfaces
 */
void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    OPAL_TRACE(1);
    
    if (ORTE_ERR_SILENT == OPAL_SOS_GET_ERROR_CODE(error_code)) {
        /* if the error is silent, say nothing */
        return;
    }
    
    opal_output(0, "%s ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_ERROR_NAME(error_code), filename, line);
}

int orte_errmgr_base_update_state(orte_jobid_t job,
                                  orte_job_state_t jobstate,
                                  orte_process_name_t *name,
                                  orte_proc_state_t state,
                                  pid_t pid,
                                  orte_exit_code_t exit_code)
{
    int rc=ORTE_SUCCESS;
    int i;
    orte_errmgr_stack_state_t stack_state;
    orte_errmgr_base_module_t *module;
    
    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                         "errmgr:base:update_state() %s) "
                         "------- %s state updated for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == name) ? "App. Process" : (name->jobid == ORTE_PROC_MY_HNP->jobid ? "Daemon" : "App. Process"),
                         (NULL == name) ? "NULL" : ORTE_NAME_PRINT(name)));
    
    stack_state  = ORTE_ERRMGR_STACK_STATE_NONE;
    stack_state |= ORTE_ERRMGR_STACK_STATE_JOB_ABORT;
    
    /********************************
     * Call the active modules
     ********************************/
    for (i = 0; i < orte_errmgr_base.modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base.modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->update_state ) {
            rc = module->update_state(job, jobstate, name, state, pid, exit_code, &stack_state);
            if (ORTE_SUCCESS != rc || ORTE_ERRMGR_STACK_STATE_COMPLETE & stack_state) {
                break;
            }
        }
    }
    
    return rc;
}

int orte_errmgr_base_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, "%s", buffer );
        free( buffer );
    }
    va_end(arglist);
    
    /* if I am a daemon or the HNP... */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* whack my local procs */
        orte_odls.kill_local_procs(NULL);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    } else {
        /* cleanup my session directory */
        orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    }
    
    /* if a critical connection failed, exit without dropping a core */
    if (ORTE_ERR_CONNECTION_FAILED == OPAL_SOS_GET_ERROR_CODE(error_code)) {
        orte_ess.abort(error_code, false);
    } else {
        orte_ess.abort(error_code, true);
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_predicted_fault(char ***proc_list,
                                     char ***node_list,
                                     char ***suggested_nodes)
{
    orte_errmgr_base_module_t *module = NULL;
    int i, rc;
    orte_errmgr_stack_state_t stack_state = ORTE_ERRMGR_STACK_STATE_NONE;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                         "errmgr:base:predicted_fault() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base.modules.size));

    for(i = 0; i < orte_errmgr_base.modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base.modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->predicted_fault ) {
            rc = module->predicted_fault(proc_list, node_list, suggested_nodes, &stack_state);
            if (ORTE_SUCCESS != rc || ORTE_ERRMGR_STACK_STATE_COMPLETE & stack_state) {
                break;
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_suggest_map_targets(orte_proc_t *proc,
                                         orte_node_t *oldnode,
                                         opal_list_t *node_list)
{
    orte_errmgr_base_module_t *module = NULL;
    int i, rc;
    orte_errmgr_stack_state_t stack_state = ORTE_ERRMGR_STACK_STATE_NONE;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                         "errmgr:base:suggest_map_targets() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base.modules.size));

    for(i = 0; i < orte_errmgr_base.modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base.modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->suggest_map_targets ) {
            rc = module->suggest_map_targets(proc, oldnode, node_list, &stack_state);
            if (ORTE_SUCCESS != rc || ORTE_ERRMGR_STACK_STATE_COMPLETE & stack_state) {
                break;
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_ft_event(int state)
{
    orte_errmgr_base_module_t *module = NULL;
    int i;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                         "errmgr:base:ft_event() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base.modules.size));

    for(i = 0; i < orte_errmgr_base.modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base.modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->ft_event ) {
            module->ft_event(state);
        }
    }

    return ORTE_SUCCESS;
}
