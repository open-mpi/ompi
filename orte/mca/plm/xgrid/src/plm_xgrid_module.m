/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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
 *
 */

#import "orte_config.h"

#import <stdlib.h>
#import <unistd.h>
#import <errno.h>
#import <string.h>
#import <sys/types.h>
#import <sys/stat.h>
#import <sys/wait.h>
#import <fcntl.h>
#ifdef HAVE_SYS_TIME_H
#import <sys/time.h>
#endif

#import "orte/constants.h"
#import "opal/util/argv.h"
#import "opal/class/opal_pointer_array.h"

#import "orte/util/show_help.h"
#import "orte/util/session_dir.h"
#import "opal/mca/event/event.h"
#import "orte/runtime/orte_wait.h"
#import "orte/mca/plm/plm.h"
#import "orte/mca/plm/base/plm_private.h"
#import "orte/mca/rml/rml.h"
#import "orte/mca/errmgr/errmgr.h"
#import "orte/mca/rmaps/rmaps.h"
#import "orte/mca/iof/iof.h"

#import "plm_xgrid.h"

int orte_plm_xgrid_init(void);
int orte_plm_xgrid_spawn(orte_job_t *jdata);
int orte_plm_xgrid_terminate_orteds(void);
int orte_plm_xgrid_signal_job(orte_jobid_t job, int32_t signal);
int orte_plm_xgrid_finalize(void);

orte_plm_base_module_1_0_0_t orte_plm_xgrid_module = {
    orte_plm_xgrid_init,
    orte_plm_base_set_hnp_name,
    orte_plm_xgrid_spawn,
    NULL,
    orte_plm_base_orted_terminate_job,
    orte_plm_xgrid_terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    orte_plm_xgrid_signal_job,
    orte_plm_xgrid_finalize
};


/* counter of number of "nodes" created */
static int node_counter = 0;

int
orte_plm_xgrid_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


static int
orte_plm_xgrid_make_nodes(orte_job_t *jdata)
{
    int num_nodes = 0, param, i, rc;
    orte_app_context_t *app, **apps;

    /* figure out how many slots we need */
    apps = (orte_app_context_t**)jdata->apps->addr;
    for(i = 0 ; i < jdata->num_apps ; i++) {
        app = apps[i];
        if (0 == app->num_procs) return ORTE_ERR_NOT_SUPPORTED;
        num_nodes += app->num_procs;
    }

    /* Create node entries for the orteds we're going to spawn. */
    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_size(orte_node_pool, num_nodes))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (i = 0 ; i < num_nodes ; ++i) {
        orte_node_t *node = OBJ_NEW(orte_node_t);
        if (NULL == node) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            return rc;
        }
        asprintf(&node->name, "ompi-xgrid-node-%d", node_counter++);
        node->state = ORTE_NODE_STATE_UP;
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = 1;
        node->slots_alloc = 1;
        node->index = opal_pointer_array_add(orte_node_pool, (void*)node);
        /* update the total slots in the job */
        jdata->total_slots_alloc += node->slots_alloc;
    }
    jdata->oversubscribe_override = true;

    return ORTE_SUCCESS;
}


int
orte_plm_xgrid_spawn(orte_job_t *jdata)
{
    int rc;
    orte_process_name_t name = {ORTE_JOBID_INVALID, 0};
    bool failed_launch = true;
    
    /* create a jobid for this job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:xgrid: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* insert the job object into the global pool */
    opal_pointer_array_add(orte_job_data, jdata);

    if (ORTE_SUCCESS != (rc = orte_plm_xgrid_make_nodes(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:xgrid: mapping job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
	    
    if (ORTE_SUCCESS != (rc = orte_rmaps.map_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }         

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:xgrid: setting up I/O for %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* launch new daemons */
    rc = [mca_plm_xgrid_component.client launchOrteds: jdata];
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* Daemons are running - launch the applications */
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(jdata->jobid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:xgrid: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

    /* get here if launch went okay */
    failed_launch = false;
    
 cleanup:
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_errmgr.update_state(jdata->jobid, ORTE_JOB_STATE_FAILED_TO_START,
                                 NULL, ORTE_PROC_STATE_UNDEF,
                                 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    return rc;
}


int
orte_plm_xgrid_terminate_orteds(void)
{
    int rc;

    rc = orte_plm_base_orted_exit(ORTE_DAEMON_HALT_VM_CMD);
    if (ORTE_SUCCESS != rc) {
	rc = [mca_plm_xgrid_component.client terminateOrteds];
    }

    if (ORTE_SUCCESS != rc) ORTE_ERROR_LOG(rc);
    return rc;
}


int
orte_plm_xgrid_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


int
orte_plm_xgrid_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }

    [mca_plm_xgrid_component.client release];
    [mca_plm_xgrid_component.pool release];

    opal_progress_unregister(orte_plm_xgrid_progress);

    return ORTE_SUCCESS;
}

