/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/routed/routed.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_app.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static int update_state(orte_jobid_t job,
                        orte_job_state_t jobstate,
                        orte_process_name_t *proc_name,
                        orte_proc_state_t state,
                        pid_t pid,
                        orte_exit_code_t exit_code);

/******************
 * HNP module
 ******************/
orte_errmgr_base_module_t orte_errmgr_app_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    update_state,
    NULL,
    NULL,
    NULL,
    orte_errmgr_base_register_migration_warning
};

/************************
 * API Definitions
 ************************/
static int init(void)
{
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static int update_state(orte_jobid_t job,
                        orte_job_state_t jobstate,
                        orte_process_name_t *proc,
                        orte_proc_state_t state,
                        pid_t pid,
                        orte_exit_code_t exit_code)
{
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:app: job %s reported state %s"
                         " for proc %s state %s exit_code %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         orte_job_state_to_str(jobstate),
                         (NULL == proc) ? "NULL" : ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state), exit_code));
    
    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        return ORTE_SUCCESS;
    }

    if (ORTE_PROC_STATE_COMM_FAILED == state) {
        /* if it is our own connection, ignore it */
        if (ORTE_PROC_MY_NAME->jobid == proc->vpid &&
            ORTE_PROC_MY_NAME->vpid == proc->vpid) {
            return ORTE_SUCCESS;
        }
        
        /* delete the route */
        orte_routed.delete_route(proc);
        /* see is this was a lifeline */
        if (ORTE_SUCCESS != orte_routed.route_lost(proc)) {
            return ORTE_ERR_UNRECOVERABLE;
        }
    }
    return ORTE_SUCCESS;
}
