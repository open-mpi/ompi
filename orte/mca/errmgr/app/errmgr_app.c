/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 *
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
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"

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
static int orte_errmgr_app_abort_peers(orte_process_name_t *procs,
                                       orte_std_cntr_t num_procs);

/******************
 * HNP module
 ******************/
orte_errmgr_base_module_t orte_errmgr_app_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    orte_errmgr_app_abort_peers,
    update_state,
    NULL,
    NULL,
    NULL,
    orte_errmgr_base_register_migration_warning,
    NULL, /* post_startup */
    NULL, /* pre_shutdown */
    NULL, /* mark_processes_as_dead */
    NULL, /* set_fault_callback */
    NULL  /* failure_notification */
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

static int orte_errmgr_app_abort_peers(orte_process_name_t *procs, orte_std_cntr_t num_procs)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_std_cntr_t i;
    orte_daemon_cmd_flag_t command = ORTE_DAEMON_ABORT_PROCS_CALLED;

    /*
     * Pack up the list of processes and send them to the HNP
     */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* pack number of processes */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(num_procs), 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /* Pack the list of names */
    for( i = 0; i < num_procs; ++i ) {
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(procs[i]), 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /* Send to HNP for termination */
    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}
