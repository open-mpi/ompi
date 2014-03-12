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
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_orted.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/******************
 * ORTED module
 ******************/
orte_state_base_module_t orte_state_orted_module = {
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

/* Local functions */
static void track_jobs(int fd, short argc, void *cbdata);
static void track_procs(int fd, short argc, void *cbdata);
static int pack_state_update(opal_buffer_t *buf, orte_job_t *jdata);
static int pack_child_contact_info(orte_jobid_t jobid, opal_buffer_t *buf);

/* defined default state machines */
static orte_job_state_t job_states[] = {
    ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE,
};
static orte_state_cbfunc_t job_callbacks[] = {
    track_jobs
};

static orte_proc_state_t proc_states[] = {
    ORTE_PROC_STATE_RUNNING,
    ORTE_PROC_STATE_REGISTERED,
    ORTE_PROC_STATE_IOF_COMPLETE,
    ORTE_PROC_STATE_WAITPID_FIRED
};
static orte_state_cbfunc_t proc_callbacks[] = {
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
    int num_states, i, rc;

    /* setup the state machine */
    OBJ_CONSTRUCT(&orte_job_states, opal_list_t);
    OBJ_CONSTRUCT(&orte_proc_states, opal_list_t);

    num_states = sizeof(job_states) / sizeof(orte_job_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_job_state(job_states[i],
                                                           job_callbacks[i],
                                                           ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* add a default error response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_FORCED_EXIT,
                                                       orte_quit, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add a state for when we are ordered to terminate */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_DAEMONS_TERMINATED,
                                                       orte_quit, ORTE_SYS_PRI))) {
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

    /* cleanup the state machines */
    while (NULL != (item = opal_list_remove_first(&orte_job_states))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_job_states);
    while (NULL != (item = opal_list_remove_first(&orte_proc_states))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_proc_states);

    return ORTE_SUCCESS;
}

static void track_jobs(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    opal_buffer_t *alert;
    orte_plm_cmd_flag_t cmd;
    int rc;

    if (ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE == caddy->job_state) {
        OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                             "%s state:orted:track_jobs sending local launch complete for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(caddy->jdata->jobid)));
        /* update the HNP with all proc states for this job */
        alert = OBJ_NEW(opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(alert);
            goto cleanup;
        }
        /* pack the job info */
        if (ORTE_SUCCESS != (rc = pack_state_update(alert, caddy->jdata))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(alert);
            goto cleanup;
        }
        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                              ORTE_RML_TAG_PLM,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(alert);
        }
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static void track_procs(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;
    orte_job_t *jdata;
    orte_proc_t *pdata, *pptr;
    opal_buffer_t *alert;
    int rc, i;
    orte_plm_cmd_flag_t cmd;
    orte_vpid_t null=ORTE_VPID_INVALID;
    int8_t flag;

    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                         "%s state:orted:track_procs called for proc %s state %s",
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
        /* don't update until we are told that all are done */
    } else if (ORTE_PROC_STATE_REGISTERED == state) {
        /* update the proc state */
        pdata->state = state;
        jdata->num_reported++;
        if (jdata->num_reported == jdata->num_local_procs) {
            /* once everyone registers, send their contact info to
             * the HNP so it is available to debuggers and anyone
             * else that needs it
             */

            OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                                 "%s state:orted: sending contact info to HNP",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

            alert = OBJ_NEW(opal_buffer_t);
            /* pack init routes command */
            cmd = ORTE_PLM_INIT_ROUTES_CMD;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* pack the jobid */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &proc->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* pack all the local child vpids */
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                    continue;
                }
                if (pptr->name.jobid == proc->jobid) {
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &pptr->name.vpid, 1, ORTE_VPID))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                    if (pptr->mpi_proc) {
                        flag = 1;
                    } else {
                        flag = 0;
                    }
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &flag, 1, OPAL_INT8))) {
                        ORTE_ERROR_LOG(rc);
                        goto cleanup;
                    }
                }
            }
            /* pack an invalid marker */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            /* add in contact info for all procs in the job */
            if (ORTE_SUCCESS != (rc = pack_child_contact_info(proc->jobid, alert))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&alert);
                goto cleanup;
            }
            /* send it */
            if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                                  ORTE_RML_TAG_PLM,
                                                  orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(rc);
            } else {
                rc = ORTE_SUCCESS;
            }
        }
    } else if (ORTE_PROC_STATE_IOF_COMPLETE == state) {
        /* do NOT update the proc state as this can hit
         * while we are still trying to notify the HNP of
         * successful launch for short-lived procs
         */
        pdata->iof_complete = true;
        if (pdata->waitpid_recvd) {
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* Clean up the session directory as if we were the process
             * itself.  This covers the case where the process died abnormally
             * and didn't cleanup its own session directory.
             */
            orte_session_dir_finalize(proc);
            /* track job status */
            jdata->num_terminated++;
            if (jdata->num_terminated == jdata->num_local_procs) {
                /* pack update state command */
                cmd = ORTE_PLM_UPDATE_PROC_STATE;
                alert = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* pack the job info */
                if (ORTE_SUCCESS != (rc = pack_state_update(alert, jdata))) {
                    ORTE_ERROR_LOG(rc);
                }
                /* send it */
                OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                                     "%s SENDING PROC TERMINATION UPDATE FOR JOB %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(jdata->jobid)));
                if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                                      ORTE_RML_TAG_PLM,
                                                      orte_rml_send_callback, NULL))) {
                    ORTE_ERROR_LOG(rc);
                }
                /* if we are trying to terminate and our routes are
                 * gone, then terminate ourselves IF no local procs
                 * remain (might be some from another job)
                 */
                if (orte_orteds_term_ordered &&
                    0 == orte_routed.num_routes()) {
                    for (i=0; i < orte_local_children->size; i++) {
                        if (NULL != (pptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i)) &&
                            pptr->alive) {
                            /* at least one is still alive */
                            goto moveon;
                        }
                    }
                    /* call our appropriate exit procedure */
                    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                                         "%s orted:state: all routes and children gone - exiting",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
                }
            }
        }
    moveon:
        /* Release the stdin IOF file descriptor for this child, if one
         * was defined. File descriptors for the other IOF channels - stdout,
         * stderr, and stddiag - were released when their associated pipes
         * were cleared and closed due to termination of the process
         * Do this after we handle termination in case the IOF needs
         * to check to see if all procs from the job are actually terminated
         */
        if (NULL != orte_iof.close) {
            orte_iof.close(proc, ORTE_IOF_STDIN);
        }
    } else if (ORTE_PROC_STATE_WAITPID_FIRED == state) {
        /* do NOT update the proc state as this can hit
         * while we are still trying to notify the HNP of
         * successful launch for short-lived procs
         */
        pdata->waitpid_recvd = true;
        if (pdata->iof_complete) {
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* Clean up the session directory as if we were the process
             * itself.  This covers the case where the process died abnormally
             * and didn't cleanup its own session directory.
             */
            orte_session_dir_finalize(proc);
            /* track job status */
            jdata->num_terminated++;
            if (jdata->num_terminated == jdata->num_local_procs) {
                /* pack update state command */
                cmd = ORTE_PLM_UPDATE_PROC_STATE;
                alert = OBJ_NEW(opal_buffer_t);
                if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                /* pack the job info */
                if (ORTE_SUCCESS != (rc = pack_state_update(alert, jdata))) {
                    ORTE_ERROR_LOG(rc);
                }
                /* send it */
                OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                                     "%s SENDING PROC TERMINATION UPDATE FOR JOB %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(jdata->jobid)));
                if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                                      ORTE_RML_TAG_PLM,
                                                      orte_rml_send_callback, NULL))) {
                    ORTE_ERROR_LOG(rc);
                }
                /* if we are trying to terminate and our routes are
                 * gone, then terminate ourselves IF no local procs
                 * remain (might be some from another job)
                 */
                if (orte_orteds_term_ordered &&
                    0 == orte_routed.num_routes()) {
                    for (i=0; i < orte_local_children->size; i++) {
                        if (NULL != (pptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i)) &&
                            pptr->alive) {
                            /* at least one is still alive */
                            goto moveon;
                        }
                    }
                    /* call our appropriate exit procedure */
                    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                                         "%s orted:state: all routes and children gone - exiting",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
                }
            }
        }
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static int pack_state_for_proc(opal_buffer_t *alert, orte_proc_t *child)
{
    int rc;

    /* pack the child's vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &(child->name.vpid), 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack the pid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack its state */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->state, 1, ORTE_PROC_STATE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack its exit code */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->exit_code, 1, ORTE_EXIT_CODE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

static int pack_state_update(opal_buffer_t *alert, orte_job_t *jdata)
{
    int i, rc;
    orte_proc_t *child;
    orte_vpid_t null=ORTE_VPID_INVALID;

    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &jdata->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* if this child is part of the job... */
        if (child->name.jobid == jdata->jobid) {
            if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    /* flag that this job is complete so the receiver can know */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}

static int pack_child_contact_info(orte_jobid_t jobid, opal_buffer_t *buf)
{
    int i, rc;
    orte_proc_t *pptr;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        if (jobid == pptr->name.jobid) {
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &pptr->name.vpid, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &pptr->rml_uri, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}
