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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_hnp.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/******************
 * HNP module - just uses base functions after
 * initializing the proc state machine. Job state
 * machine is unused by hnplication procs at this
 * time.
 ******************/
orte_state_base_module_t orte_state_hnp_module = {
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

static void local_launch_complete(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *state = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = state->jdata;

    if (orte_report_launch_progress) {
        if (0 == jdata->num_daemons_reported % 100 ||
            jdata->num_daemons_reported == orte_process_info.num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_REPORT_PROGRESS);
        }
    }
    OBJ_RELEASE(state);
}

static void track_procs(int fd, short argc, void *cbdata);
static void check_all_complete(int fd, short argc, void *cbdata);
static void report_progress(int fd, short argc, void *cbdata);
static void cleanup_job(int fd, short argc, void *cbdata);

/* defined default state machine sequence - individual
 * plm's must add a state for launching daemons
 */
static orte_job_state_t launch_states[] = {
    ORTE_JOB_STATE_INIT,
    ORTE_JOB_STATE_INIT_COMPLETE,
    ORTE_JOB_STATE_ALLOCATE,
    ORTE_JOB_STATE_DAEMONS_LAUNCHED,
    ORTE_JOB_STATE_DAEMONS_REPORTED,
    ORTE_JOB_STATE_VM_READY,
    ORTE_JOB_STATE_MAP,
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
    orte_plm_base_setup_job_complete,
    orte_ras_base_allocate,
    orte_plm_base_daemons_launched,
    orte_plm_base_daemons_reported,
    orte_plm_base_vm_ready,
    orte_rmaps_base_map_job,
    orte_plm_base_complete_setup,
    orte_plm_base_launch_apps,
    local_launch_complete,
    orte_plm_base_post_launch,
    orte_plm_base_registered,
    check_all_complete,
    cleanup_job,
    orte_quit,
    orte_quit
};

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
                                                       report_progress, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    if (5 < opal_output_get_verbosity(orte_state_base_output)) {
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
    if (5 < opal_output_get_verbosity(orte_state_base_output)) {
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

static void report_progress(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;

    opal_output(orte_clean_output, "App launch reported: %d (out of %d) daemons - %d (out of %d) procs",
                (int)jdata->num_daemons_reported, (int)orte_process_info.num_procs,
                (int)jdata->num_launched, (int)jdata->num_procs);
    OBJ_RELEASE(caddy);
}

static void track_procs(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;
    orte_job_t *jdata;
    orte_proc_t *pdata;

    OPAL_OUTPUT_VERBOSE((5, orte_state_base_output,
                         "%s state:hnp:track_procs called for proc %s state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state)));

    /* get the job object for this proc */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto cleanup;
    }
    pdata = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);

    if (ORTE_PROC_STATE_RUNNING == state) {
        /* update the proc state */
        pdata->state = state;
        jdata->num_launched++;
        if (jdata->num_launched == jdata->num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_RUNNING);
        }
    } else if (ORTE_PROC_STATE_REGISTERED == state) {
        /* update the proc state */
        pdata->state = state;
        jdata->num_reported++;
        if (jdata->num_reported == jdata->num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_REGISTERED);
        }
    } else if (ORTE_PROC_STATE_IOF_COMPLETE == state) {
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
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* return the allocated slot for reuse */
            cleanup_node(pdata);
            /* Clean up the session directory as if we were the process
             * itself.  This covers the case where the process died abnormally
             * and didn't cleanup its own session directory.
             */
            orte_session_dir_finalize(proc);
            /* track job status */
            jdata->num_terminated++;
            if (jdata->num_terminated == jdata->num_procs) {
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
            }
        }
    } else if (ORTE_PROC_STATE_WAITPID_FIRED == state) {
        /* update the proc state */
        pdata->state = state;
        pdata->waitpid_recvd = true;
        if (pdata->iof_complete) {
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* return the allocated slot for reuse */
            cleanup_node(pdata);
            /* Clean up the session directory as if we were the process
             * itself.  This covers the case where the process died abnormally
             * and didn't cleanup its own session directory.
             */
            orte_session_dir_finalize(proc);
            /* track job status */
            jdata->num_terminated++;
            if (jdata->num_terminated == jdata->num_procs) {
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
            }
        }
    } else if (ORTE_PROC_STATE_TERMINATED == state) {
        /* update the proc state */
        pdata->state = state;
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
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
	}
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static void cleanup_job(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;

    OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                         "%s state:hnp:cleanup on job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid)));

    /* flag that we were notified */
    jdata->state = ORTE_JOB_STATE_NOTIFIED;
    /* send us back thru job complete */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
    OBJ_RELEASE(caddy);
}

static void check_all_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;

    orte_proc_t *proc;
    int i;
    orte_std_cntr_t j;
    orte_job_t *job;
    orte_node_t *node;
    orte_job_map_t *map;
    orte_std_cntr_t index;
    bool one_still_alive;
    orte_vpid_t lowest=0;

    OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                         "%s state:hnp:check_job_complete on job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid)));

    if (NULL == jdata || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        /* just check to see if the daemons are complete */
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                             "%s state:hnp:check_job_complete - received NULL job, checking daemons",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CHECK_DAEMONS;
    } else {
        /* mark the job as terminated, but don't override any
         * abnormal termination flags
         */
        if (jdata->state < ORTE_JOB_STATE_UNTERMINATED) {
            jdata->state = ORTE_JOB_STATE_TERMINATED;
        }
    }

    /* turn off any sensor monitors on this job */
    orte_sensor.stop(jdata->jobid);

    /* tell the IOF that the job is complete */
    if (NULL != orte_iof.complete) {
        orte_iof.complete(jdata);
    }

    if (0 < jdata->num_non_zero_exit && !orte_abort_non_zero_exit) {
        if (!orte_report_child_jobs_separately || 1 == ORTE_LOCAL_JOBID(jdata->jobid)) {
            /* update the exit code */
            ORTE_UPDATE_EXIT_STATUS(lowest);
        }

        /* warn user */
        opal_output(orte_clean_output,
                    "-------------------------------------------------------\n"
                    "While %s job %s terminated normally, %d %s. Further examination may be required.\n"
                    "-------------------------------------------------------",
                    (1 == ORTE_LOCAL_JOBID(jdata->jobid)) ? "the primary" : "child",
                    (1 == ORTE_LOCAL_JOBID(jdata->jobid)) ? "" : ORTE_LOCAL_JOBID_PRINT(jdata->jobid),
                    jdata->num_non_zero_exit,
                    (1 == jdata->num_non_zero_exit) ? "process returned\na non-zero exit code." :
                    "processes returned\nnon-zero exit codes.");
    }

    OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                         "%s state:hnp:check_job_completed declared job %s normally terminated - checking all jobs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* if this job is a continuously operating one, then don't do
     * anything further - just return here
     */
    if (NULL != jdata &&
        (ORTE_JOB_CONTROL_CONTINUOUS_OP & jdata->controls ||
         ORTE_JOB_CONTROL_RECOVERABLE & jdata->controls)) {
        goto CHECK_ALIVE;
    }
    
    /* if the job that is being checked is the HNP, then we are
     * trying to terminate the orteds. In that situation, we
     * do -not- check all jobs - we simply notify the HNP
     * that the orteds are complete. Also check special case
     * if jdata is NULL - we want
     * to definitely declare the job done if the orteds
     * have completed, no matter what else may be happening.
     * This can happen if a ctrl-c hits in the "wrong" place
     * while launching
     */
 CHECK_DAEMONS:
    if (jdata == NULL || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        if (0 == orte_routed.num_routes()) {
            /* orteds are done! */
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                 "%s orteds complete - exiting",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (NULL == jdata) {
                jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            }
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_DAEMONS_TERMINATED);
            OBJ_RELEASE(caddy);
            return;
        }
        OBJ_RELEASE(caddy);
        return;
    }
    
    /* Release the resources used by this job. Since some errmgrs may want
     * to continue using resources allocated to the job as part of their
     * fault recovery procedure, we only do this once the job is "complete".
     * Note that an aborted/killed job -is- flagged as complete and will
     * therefore have its resources released. We need to do this after
     * we call the errmgr so that any attempt to restart the job will
     * avoid doing so in the exact same place as the current job
     */
    if (NULL != jdata->map  && jdata->state == ORTE_JOB_STATE_TERMINATED) {
        map = jdata->map;
        for (index = 0; index < map->nodes->size; index++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, index))) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                 "%s releasing procs from node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            for (i = 0; i < node->procs->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                    continue;
                }
                if (proc->name.jobid != jdata->jobid) {
                    /* skip procs from another job */
                    continue;
                }
                node->slots_inuse--;
                node->num_procs--;
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                     "%s releasing proc %s from node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name), node->name));
                /* set the entry in the node array to NULL */
                opal_pointer_array_set_item(node->procs, i, NULL);
                /* release the proc once for the map entry */
                OBJ_RELEASE(proc);
            }
        }
        OBJ_RELEASE(map);
        jdata->map = NULL;
    }
    
 CHECK_ALIVE:
    /* now check to see if all jobs are done - trigger notification of this jdata
     * object when we find it
     */
    one_still_alive = false;
    for (j=1; j < orte_job_data->size; j++) {
        if (NULL == (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, j))) {
            /* since we are releasing jdata objects as we
             * go, we can no longer assume that the job_data
             * array is left justified
             */
            continue;
        }
        /* if this is the job we are checking AND it normally terminated,
         * then activate the "notify_completed" state - this will release
         * the job state, but is provided so that the HNP main code can
         * take alternative actions if desired. If the state is killed_by_cmd,
         * then go ahead and release it. We cannot release it if it
         * abnormally terminated as mpirun needs the info so it can
         * report appropriately to the user
         *
         * NOTE: do not release the primary job (j=1) so we
         * can pretty-print completion message
         */
        if (NULL != jdata && job->jobid == jdata->jobid) {
            opal_output(0, "CHECKING JOB %s", ORTE_JOBID_PRINT(jdata->jobid));
            if (jdata->state == ORTE_JOB_STATE_TERMINATED) {
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                     "%s state:hnp:check_job_completed state is terminated - activating notify",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_NOTIFY_COMPLETED);
                one_still_alive = true;
            } else if (jdata->state == ORTE_JOB_STATE_KILLED_BY_CMD ||
                       jdata->state == ORTE_JOB_STATE_NOTIFIED) {
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                     "%s state:hnp:check_job_completed state is killed or notified - cleaning up",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                /* release this object, ensuring that the
                 * pointer array internal accounting
                 * is maintained!
                 */
                if (1 < j) {
                    opal_pointer_array_set_item(orte_job_data, j, NULL);  /* ensure the array has a NULL */
                    OBJ_RELEASE(jdata);
                }
            } else {
                opal_output(0, "STATE WAS %s", orte_job_state_to_str(jdata->state));
            }
            continue;
        }
        /* if the job is flagged to not be monitored, skip it */
        if (ORTE_JOB_CONTROL_DO_NOT_MONITOR & job->controls) {
            continue;
        }
        /* when checking for job termination, we must be sure to NOT check
         * our own job as it - rather obviously - has NOT terminated!
         */
        if (job->num_terminated < job->num_procs) {
            /* we have at least one job that is not done yet - we cannot
             * just return, though, as we need to ensure we cleanout the
             * job data for the job that just completed
             */
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                 "%s state:hnp:check_job_completed job %s is not terminated (%d:%d)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs));
            one_still_alive = true;
        }
        else {
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                                 "%s state:hnp:check_job_completed job %s is terminated (%d vs %d [%s])",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs,
                                 (NULL == jdata) ? "UNKNOWN" : orte_job_state_to_str(jdata->state) ));
        }
    }
    /* if a job is still alive, we just return */
    if (one_still_alive) {
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                             "%s state:hnp:check_job_completed at least one job is not terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_RELEASE(caddy);
        return;
    }
    /* if we get here, then all jobs are done, so terminate */
    OPAL_OUTPUT_VERBOSE((2, orte_state_base_output,
                         "%s state:hnp:check_job_completed all jobs terminated",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* set the exit status to 0 - this will only happen if it
     * wasn't already set by an error condition
     */
    ORTE_UPDATE_EXIT_STATUS(0);

    /* order daemon termination - this tells us to cleanup
     * our local procs as well as telling remote daemons
     * to die
     */
    orte_plm.terminate_orteds();

    OBJ_RELEASE(caddy);
}
