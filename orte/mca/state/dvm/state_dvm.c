/*
 * Copyright (c) 2015      Intel, Inc. All rights reserved
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
#include "orte/mca/filem/filem.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_dvm.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/* local functions */
static void vm_ready(int fd, short args, void *cbata);
void check_complete(int fd, short args, void *cbdata);

/******************
 * DVM module - used when mpirun is persistent
 ******************/
orte_state_base_module_t orte_state_dvm_module = {
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

/* defined default state machine sequence - individual
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
    ORTE_JOB_STATE_ALL_JOBS_COMPLETE
};
static orte_state_cbfunc_t launch_callbacks[] = {
    orte_plm_base_setup_job,
    orte_plm_base_setup_job_complete,
    orte_ras_base_allocate,
    orte_plm_base_allocation_complete,
    orte_plm_base_daemons_launched,
    orte_plm_base_daemons_reported,
    vm_ready,
    orte_rmaps_base_map_job,
    orte_plm_base_mapping_complete,
    orte_plm_base_complete_setup,
    orte_plm_base_launch_apps,
    orte_state_base_local_launch_complete,
    orte_plm_base_post_launch,
    orte_plm_base_registered,
    check_complete,
    orte_state_base_cleanup_job,
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
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs
};

static void force_quit(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* give us a chance to stop the orteds */
    orte_plm.terminate_orteds();
    OBJ_RELEASE(caddy);
}

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
    /* add the termination response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_DAEMONS_TERMINATED,
                                                       orte_quit, ORTE_SYS_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add a default error response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_FORCED_EXIT,
                                                       force_quit, ORTE_ERROR_PRI))) {
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

static void files_ready(int status, void *cbdata)
{
    orte_job_t *jdata = (orte_job_t*)cbdata;

    if (ORTE_SUCCESS != status) {
        ORTE_FORCED_TERMINATE(status);
    } else {
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP);
    }
}

static void vm_ready(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* if this is my job, then we are done */
    if (ORTE_PROC_MY_NAME->jobid == caddy->jdata->jobid) {
        /* notify that the vm is ready */
        fprintf(stdout, "DVM ready\n");
        OBJ_RELEASE(caddy);
        return;
    }
    
    /* progress the job */
    caddy->jdata->state = ORTE_JOB_STATE_VM_READY;

    /* position any required files */
    if (ORTE_SUCCESS != orte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void check_complete(int fd, short args, void *cbdata)
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
    int32_t i32, *i32ptr;

    opal_output_verbose(2, orte_state_base_framework.framework_output,
                        "%s state:base:check_job_complete on job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid));

    if (NULL == jdata || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        /* just check to see if the daemons are complete */
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                             "%s state:base:check_job_complete - received NULL job, checking daemons",
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

    /* tell the IOF that the job is complete */
    if (NULL != orte_iof.complete) {
        orte_iof.complete(jdata);
    }

    i32ptr = &i32;
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_NUM_NONZERO_EXIT, (void**)&i32ptr, OPAL_INT32) && !orte_abort_non_zero_exit) {
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
                    i32, (1 == i32) ? "process returned\na non-zero exit code." :
                    "processes returned\nnon-zero exit codes.");
    }

    OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                         "%s state:base:check_job_completed declared job %s terminated with state %s - checking all jobs",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         orte_job_state_to_str(jdata->state)));
    
    /* if this job is a continuously operating one, then don't do
     * anything further - just return here
     */
    if (NULL != jdata &&
        (orte_get_attribute(&jdata->attributes, ORTE_JOB_CONTINUOUS_OP, NULL, OPAL_BOOL) ||
         ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_RECOVERABLE))) {
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
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
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
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
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
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                     "%s releasing proc %s from node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name), node->name));
                /* set the entry in the node array to NULL */
                opal_pointer_array_set_item(node->procs, i, NULL);
                /* release the proc once for the map entry */
                OBJ_RELEASE(proc);
            }
            /* set the node location to NULL */
            opal_pointer_array_set_item(map->nodes, index, NULL);
            /* maintain accounting */
            OBJ_RELEASE(node);
            /* flag that the node is no longer in a map */
            ORTE_FLAG_UNSET(node, ORTE_NODE_FLAG_MAPPED);
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
            if (jdata->state == ORTE_JOB_STATE_TERMINATED) {
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                     "%s state:base:check_job_completed state is terminated - activating notify",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_NOTIFY_COMPLETED);
                one_still_alive = true;
            } else if (jdata->state == ORTE_JOB_STATE_KILLED_BY_CMD ||
                       jdata->state == ORTE_JOB_STATE_NOTIFIED) {
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                     "%s state:base:check_job_completed state is killed or notified - cleaning up",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                /* release this object, ensuring that the
                 * pointer array internal accounting
                 * is maintained!
                 */
                if (1 < j) {
		    if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_DEBUGGER_DAEMON)) {
			/* this was a debugger daemon. notify that a debugger has detached */
			ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_DEBUGGER_DETACH);
		    }
                    opal_pointer_array_set_item(orte_job_data, j, NULL);  /* ensure the array has a NULL */
                    OBJ_RELEASE(jdata);
                }
            }
            continue;
        }
        /* if the job is flagged to not be monitored, skip it */
        if (ORTE_FLAG_TEST(job, ORTE_JOB_FLAG_DO_NOT_MONITOR)) {
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
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                 "%s state:base:check_job_completed job %s is not terminated (%d:%d)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs));
            one_still_alive = true;
        }
        else {
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                 "%s state:base:check_job_completed job %s is terminated (%d vs %d [%s])",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs,
                                 (NULL == jdata) ? "UNKNOWN" : orte_job_state_to_str(jdata->state) ));
        }
    }
    /* if a job is still alive, we just return */
    if (one_still_alive) {
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                             "%s state:base:check_job_completed at least one job is not terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_RELEASE(caddy);
        return;
    }
    /* if we get here, then all jobs are done, so terminate */
    OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                         "%s state:base:check_job_completed all jobs terminated",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* stop the job timeout event, if set */
    if (NULL != orte_mpiexec_timeout) {
        OBJ_RELEASE(orte_mpiexec_timeout);
        orte_mpiexec_timeout = NULL;
    }

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
