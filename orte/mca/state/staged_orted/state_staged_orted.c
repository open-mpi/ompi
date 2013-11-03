/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
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
#include "orte/mca/rml/rml.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_staged_orted.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/******************
 * STAGED_ORTED module
 ******************/
orte_state_base_module_t orte_state_staged_orted_module = {
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
static int pack_state_update(opal_buffer_t *buf,
                             orte_job_t *jdata,
                             orte_proc_t *proc);

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
                                                       orte_quit, ORTE_ERROR_PRI))) {
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

    /* ignore this */
    OBJ_RELEASE(caddy);
}

static void send_fms(opal_buffer_t *bptr, void *cbdata)
{
    orte_proc_t *pdata = (orte_proc_t*)cbdata;
    orte_proc_t *pptr;
    orte_job_t *jdata;
    opal_buffer_t *xfer, *alert;
    orte_dfs_cmd_t cmd = ORTE_DFS_RELAY_POSTS_CMD;
    int rc, i;
    orte_plm_cmd_flag_t cmd2;

    /* we will get a NULL buffer if there are no maps, so check and
     * ignore sending an update if that's the case
     */
    if (NULL != bptr) {
        opal_output_verbose(1, orte_state_base_framework.framework_output,
                            "%s SENDING FILE MAPS FOR %s OF SIZE %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&pdata->name), (int)bptr->bytes_used);
        xfer = OBJ_NEW(opal_buffer_t);
        if (OPAL_SUCCESS != (rc = opal_dss.pack(xfer, &cmd, 1, ORTE_DFS_CMD_T))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(xfer, &pdata->name, 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            return;
        }
        if (OPAL_SUCCESS != (rc = opal_dss.pack(xfer, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            return;
        }
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, xfer,
                                              ORTE_RML_TAG_DFS_CMD,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(xfer);
            return;
        }
    }

    /* Clean up the session directory as if we were the process
     * itself.  This covers the case where the process died abnormally
     * and didn't cleanup its own session directory.
     */
    orte_session_dir_finalize(&pdata->name);
    /* alert the HNP */
    cmd2 = ORTE_PLM_UPDATE_PROC_STATE;
    alert = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd2, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(alert);
        return;
    }
    /* get the job object for this proc */
    if (NULL == (jdata = orte_get_job_data_object(pdata->name.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return;
    }
    /* pack the info */
    if (ORTE_SUCCESS != (rc = pack_state_update(alert, jdata, pdata))) {
        ORTE_ERROR_LOG(rc);
    }
    /* send it */
    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                         "%s SENDING TERMINATION UPDATE FOR PROC %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&pdata->name)));
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                          ORTE_RML_TAG_PLM,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    /* find this proc in the children array and remove it so
     * we don't keep telling the HNP that it died
     */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        if (pptr == pdata) {
            opal_pointer_array_set_item(orte_local_children, i, NULL);
            OBJ_RELEASE(pdata);
            break;
        }
    }
}

    
static void track_procs(int fd, short argc, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;
    orte_job_t *jdata;
    orte_proc_t *pdata;

    OPAL_OUTPUT_VERBOSE((5, orte_state_base_framework.framework_output,
                         "%s state:staged_orted:track_procs called for proc %s state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state)));

    /* get the job object for this proc */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto cleanup;
    }
    pdata = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);

    switch (state) {
    case ORTE_PROC_STATE_RUNNING:
        /* update the proc state */
        pdata->state = state;
        jdata->num_launched++;
        /* we don't really care - nothing further to do */
        break;

    case ORTE_PROC_STATE_REGISTERED:
        /* update the proc state */
        pdata->state = state;
        /* if this proc registered as an MPI proc, and
         * MPI is not allowed, then that is an error
         */
        if (!jdata->gang_launched && pdata->mpi_proc) {
            /* abort the proc */
            /* notify the HNP of the error */
        }
        break;

    case ORTE_PROC_STATE_IOF_COMPLETE:
        /* do NOT update the proc state as this can hit
         * while we are still trying to notify the HNP of
         * successful launch for short-lived procs
         */
        pdata->iof_complete = true;
        if (pdata->waitpid_recvd) {
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* retrieve any file maps posted by this process and forward them
             * to the HNP for collection
             */
            orte_dfs.get_file_map(proc, send_fms, pdata);
         }
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
        break;

    case ORTE_PROC_STATE_WAITPID_FIRED:
        /* do NOT update the proc state as this can hit
         * while we are still trying to notify the HNP of
         * successful launch for short-lived procs
         */
        pdata->waitpid_recvd = true;
        if (pdata->iof_complete) {
            /* the proc has terminated */
            pdata->alive = false;
            pdata->state = ORTE_PROC_STATE_TERMINATED;
            /* retrieve any file maps posted by this process and forward them
             * to the HNP for collection
             */
            orte_dfs.get_file_map(proc, send_fms, pdata);
        }
        break;

    default:
        /* ignore */
        break;
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static int pack_state_update(opal_buffer_t *alert,
                             orte_job_t *jdata,
                             orte_proc_t *child)
{
    int rc;
    orte_vpid_t null=ORTE_VPID_INVALID;

    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &jdata->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
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

    /* flag that this job is complete so the receiver can know */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
