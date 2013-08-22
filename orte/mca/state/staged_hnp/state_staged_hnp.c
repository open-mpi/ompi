/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
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

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_staged_hnp.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/******************
 * STAGED module
 ******************/
orte_state_base_module_t orte_state_staged_hnp_module = {
    init,
    finalize,
    orte_state_base_activate_job_state,
    orte_state_base_add_job_state,
    orte_state_base_set_job_state_callback,
    orte_state_base_set_job_state_priority,
    orte_state_base_remove_job_state,
    orte_state_base_activate_proc_state,
    orte_state_base_add_proc_state,
    orte_state_base_set_proc_state_callback,
    orte_state_base_set_proc_state_priority,
    orte_state_base_remove_proc_state
};

static void setup_job_complete(int fd, short args, void *cbdata);

/* defined state machine sequence - individual
 * plm's must add a state for launching daemons
 */
static orte_job_state_t launch_states[] = {
    ORTE_JOB_STATE_INIT,
    ORTE_JOB_STATE_INIT_COMPLETE,
    ORTE_JOB_STATE_ALLOCATE,
    ORTE_JOB_STATE_ALLOCATION_COMPLETE,
    ORTE_JOB_STATE_DAEMONS_LAUNCHED,
    ORTE_JOB_STATE_DAEMONS_REPORTED,
    ORTE_JOB_STATE_VM_READY,
    ORTE_JOB_STATE_MAP,
    ORTE_JOB_STATE_MAP_COMPLETE,
    ORTE_JOB_STATE_SYSTEM_PREP,
    ORTE_JOB_STATE_LAUNCH_APPS,
    ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE,
    ORTE_JOB_STATE_RUNNING,
    ORTE_JOB_STATE_REGISTERED,
    /* termination states */
    ORTE_JOB_STATE_TERMINATED,
    ORTE_JOB_STATE_NOTIFY_COMPLETED,
    ORTE_JOB_STATE_ALL_JOBS_COMPLETE,
    ORTE_JOB_STATE_DAEMONS_TERMINATED
};
static orte_state_cbfunc_t launch_callbacks[] = {
    orte_plm_base_setup_job,
    setup_job_complete,
    orte_ras_base_allocate,
    orte_plm_base_allocation_complete,
    orte_plm_base_daemons_launched,
    orte_plm_base_daemons_reported,
    orte_plm_base_vm_ready,
    orte_rmaps_base_map_job,
    orte_plm_base_mapping_complete,
    orte_plm_base_complete_setup,
    orte_plm_base_launch_apps,
    orte_state_base_local_launch_complete,
    orte_plm_base_post_launch,
    orte_plm_base_registered,
    orte_state_base_check_all_complete,
    orte_state_base_cleanup_job,
    orte_quit,
    orte_quit
};

/* staged_hnp execution requires that we start as many
 * procs initially as we have resources - if we have
 * adequate resources, then we behave just like the
 * default HNP module. If we don't, then we will have
 * some procs left over - whenever a proc completes,
 * we then initiate execution of one of the remaining
 * procs. This continues until all procs are complete.
 *
 * NOTE: MPI DOESN'T KNOW HOW TO WORK WITH THIS SCENARIO,
 * so detection of a call to MPI_Init from a proc while
 * this module is active is cause for abort!
 */
static void track_procs(int fd, short args, void *cbdata);

static orte_proc_state_t proc_states[] = {
    ORTE_PROC_STATE_RUNNING,
    ORTE_PROC_STATE_REGISTERED,
    ORTE_PROC_STATE_IOF_COMPLETE,
    ORTE_PROC_STATE_WAITPID_FIRED,
    ORTE_PROC_STATE_TERMINATED
};
static orte_state_cbfunc_t proc_callbacks[] = {
    track_procs,
    track_procs,
    track_procs,
    track_procs,
    track_procs
};

/************************
 * API Definitions
 ************************/
static int init(void)
{
    int i, rc;
    int num_states;

    /* setup the state machines */
    OBJ_CONSTRUCT(&orte_job_states, opal_list_t);
    OBJ_CONSTRUCT(&orte_proc_states, opal_list_t);

    /* setup the job state machine */
    num_states = sizeof(launch_states) / sizeof(orte_job_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_job_state(launch_states[i],
                                                           launch_callbacks[i],
                                                           ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* add a default error response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_FORCED_EXIT,
                                                       orte_quit, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add callback to report progress, if requested */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_REPORT_PROGRESS,
                                                       orte_state_base_report_progress, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_job_state_machine();
    }

    /* populate the proc state machine to allow us to
     * track proc lifecycle changes
     */
    num_states = sizeof(proc_states) / sizeof(orte_proc_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_proc_state(proc_states[i],
                                                            proc_callbacks[i],
                                                            ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_proc_state_machine();
    }

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    opal_list_item_t *item;

    /* cleanup the proc state machine */
    while (NULL != (item = opal_list_remove_first(&orte_proc_states))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_proc_states);

    return ORTE_SUCCESS;
}

static void setup_job_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;
    int i, j;
    orte_app_context_t *app;
    orte_proc_t *proc;
    orte_vpid_t vpid;

    /* check that the job meets our requirements */
    vpid = 0;
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (app->num_procs <= 0) {
            /* must specify -np for staged_hnp execution */
            orte_show_help("help-state-staged-hnp.txt", "no-np", true);
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_SILENT_ABORT);
            OBJ_RELEASE(caddy);
            return;
        }
        /* build the proc arrays - we'll need them later */
        for (j=0; j < app->num_procs; j++) {
            proc = OBJ_NEW(orte_proc_t);
            proc->name.jobid = jdata->jobid;
            proc->name.vpid = vpid;
            proc->app_idx = i;
            proc->app_rank = j;
            /* flag that the proc is NOT to be included
             * in a pidmap message so we don't do it until
             * the proc is actually scheduled for launch
             */
            proc->updated = false;
            /* procs must not barrier when executing in stages */
            proc->do_not_barrier = true;
            /* add it to the job */
            opal_pointer_array_set_item(jdata->procs, vpid, proc);
            jdata->num_procs++;
            vpid++;
            /* add it to the app */
            OBJ_RETAIN(proc);
            opal_pointer_array_set_item(&app->procs, j, proc);
        }
    }

    /* set the job map to use the staged_hnp mapper */
    if (NULL == jdata->map) {
        jdata->map = OBJ_NEW(orte_job_map_t);
        jdata->map->req_mapper = strdup("staged");
        ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_STAGED);
        ORTE_SET_MAPPING_DIRECTIVE(jdata->map->mapping, ORTE_MAPPING_NO_OVERSUBSCRIBE);
        jdata->map->display_map = orte_rmaps_base.display_map;
    }

    /* if there are any file_maps attached to this job, load them */
    if (NULL != jdata->file_maps) {
        orte_dfs.load_file_maps(jdata->jobid, jdata->file_maps, NULL, NULL);
    }
    orte_plm_base_setup_job_complete(0, 0, (void*)caddy);
}

static void cleanup_node(orte_proc_t *proc)
{
    orte_node_t *node;
    orte_proc_t *p;
    int i;

    if (NULL == (node = proc->node)) {
        return;
    }
    node->num_procs--;
    node->slots_inuse--;
    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                         "%s state:staged_hnp:track_procs proc %s termed node %s has %d slots, %d slots inuse",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&proc->name), node->name,
                         (int)node->slots, (int)node->slots_inuse));
    for (i=0; i < node->procs->size; i++) {
        if (NULL == (p = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
            continue;
        }
        if (p->name.jobid == proc->name.jobid &&
            p->name.vpid == proc->name.vpid) {
            opal_pointer_array_set_item(node->procs, i, NULL);
            OBJ_RELEASE(p);
            break;
        }
    }
}

static void track_procs(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;
    orte_job_t *jdata;
    orte_proc_t *pdata;

    opal_output_verbose(2, orte_state_base_framework.framework_output,
                        "%s state:staged_hnp:track_procs called for proc %s state %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(proc),
                        orte_proc_state_to_str(state));

    /* get the job object for this proc */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    pdata = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);

    if (ORTE_PROC_STATE_RUNNING == state) {
        /* update the proc state */
        pdata->state = state;
        jdata->num_launched++;
        if (jdata->num_launched == jdata->num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_RUNNING);
        }
    }

    /* if this is a registration, check to see if it came from
     * inside MPI_Init - if it did, that is not acceptable
     */
    if (ORTE_PROC_STATE_REGISTERED == state) {
        if (pdata->mpi_proc && !jdata->gang_launched) {
            /* we can't support this - issue an error and abort */
            orte_show_help("help-state-staged-hnp.txt", "mpi-procs-not-supported", true);
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_SILENT_ABORT);
        }
        /* update the proc state */
        pdata->state = state;
        jdata->num_reported++;
        if (jdata->num_reported == jdata->num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_REGISTERED);
        }
        OBJ_RELEASE(caddy);
        return;
    }

    if (ORTE_PROC_STATE_IOF_COMPLETE == state) {
        /* update the proc state */
        pdata->state = state;
        /* Release only the stdin IOF file descriptor for this child, if one
         * was defined. File descriptors for the other IOF channels - stdout,
         * stderr, and stddiag - were released when their associated pipes
         * were cleared and closed due to termination of the process
         */
        if (NULL != orte_iof.close) {
            orte_iof.close(proc, ORTE_IOF_STDIN);
        }
        pdata->iof_complete = true;
        if (pdata->waitpid_recvd) {
            goto terminated;
        }
        OBJ_RELEASE(caddy);
        return;
    }

    if (ORTE_PROC_STATE_WAITPID_FIRED == state) {
        /* update the proc state */
        pdata->state = state;
        pdata->waitpid_recvd = true;
        if (pdata->iof_complete) {
            goto terminated;
        }
        OBJ_RELEASE(caddy);
        return;
    }
    
    /* if the proc terminated, see if any other procs are
     * waiting to run. We assume that the app_contexts are
     * in priority order, with the highest priority being
     * at position 0 in the app_context array for this job
     */
    if (ORTE_PROC_STATE_TERMINATED == state) {
    terminated:
        /* update the proc state */
        pdata->alive = false;
        pdata->state = ORTE_PROC_STATE_TERMINATED;
        if (pdata->local_proc) {
            /* Clean up the session directory as if we were the process
             * itself.  This covers the case where the process died abnormally
             * and didn't cleanup its own session directory.
             */
            orte_session_dir_finalize(proc);
        }
        /* return the allocated slot for reuse */
        cleanup_node(pdata);
        /* track job status */
        jdata->num_terminated++;
        if (jdata->num_terminated == jdata->num_procs) {
            /* no other procs are waiting, so end this job */
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
        } else if (jdata->num_mapped < jdata->num_procs) {
            /* schedule the job for re-mapping so that procs
             * waiting for resources can execute
             */
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP);
        }
	/* otherwise, do nothing until more procs terminate */
        OBJ_RELEASE(caddy);
        return;
    }
}
