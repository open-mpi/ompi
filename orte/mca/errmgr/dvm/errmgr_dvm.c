/*
 * Copyright (c) 2009-2011 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011      Oracle and/or all its affiliates.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
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
#include <string.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/util/nidmap.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_dvm.h"

static int init(void);
static int finalize(void);

static int predicted_fault(opal_list_t *proc_list,
                           opal_list_t *node_list,
                           opal_list_t *suggested_map);

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list);

static int ft_event(int state);


/******************
 * dvm module
 ******************/
orte_errmgr_base_module_t orte_errmgr_dvm_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    orte_errmgr_base_abort_peers,
    predicted_fault,
    suggest_map_targets,
    ft_event,
    orte_errmgr_base_register_migration_warning,
    NULL,
    orte_errmgr_base_execute_error_callbacks
};


/*
 * Local functions
 */
static void job_errors(int fd, short args, void *cbdata);
static void proc_errors(int fd, short args, void *cbdata);

static int init(void)
{
    /* setup state machine to trap job errors */
    orte_state.add_job_state(ORTE_JOB_STATE_ERROR, job_errors, ORTE_ERROR_PRI);

    /* set the lost connection state to run at MSG priority so
     * we can process any last messages from the proc
     */
    orte_state.add_proc_state(ORTE_PROC_STATE_COMM_FAILED, proc_errors, ORTE_MSG_PRI);

    /* setup state machine to trap proc errors */
    orte_state.add_proc_state(ORTE_PROC_STATE_ERROR, proc_errors, ORTE_ERROR_PRI);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static void _terminate_job(orte_jobid_t jobid)
{
    opal_pointer_array_t procs;
    orte_proc_t pobj;

    OBJ_CONSTRUCT(&procs, opal_pointer_array_t);
    opal_pointer_array_init(&procs, 1, 1, 1);
    OBJ_CONSTRUCT(&pobj, orte_proc_t);
    pobj.name.jobid = jobid;
    pobj.name.vpid = ORTE_VPID_WILDCARD;
    opal_pointer_array_add(&procs, &pobj);
    orte_plm.terminate_procs(&procs);
    OBJ_DESTRUCT(&procs);
    OBJ_DESTRUCT(&pobj);
}

static void job_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_job_state_t jobstate;
    orte_exit_code_t sts;
    orte_proc_t *aborted_proc;
    opal_buffer_t *answer;
    int32_t rc, ret;
    int room, *rmptr;

    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        return;
    }

    /* if the jdata is NULL, then we ignore it as this
     * is reporting an unrecoverable error
     */
    if (NULL == caddy->jdata) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        OBJ_RELEASE(caddy);
        return;
    }

    /* update the state */
    jdata = caddy->jdata;
    jobstate = caddy->job_state;
    jdata->state = jobstate;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:dvm: job %s reported state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         orte_job_state_to_str(jobstate)));

    if (ORTE_JOB_STATE_NEVER_LAUNCHED == jobstate ||
        ORTE_JOB_STATE_ALLOC_FAILED == jobstate ||
        ORTE_JOB_STATE_MAP_FAILED == jobstate ||
        ORTE_JOB_STATE_CANNOT_LAUNCH == jobstate) {
        /* disable routing as we may not have performed the daemon
         * wireup - e.g., in a managed environment, all the daemons
         * "phone home", but don't actually wireup into the routed
         * network until they receive the launch message
         */
        orte_routing_is_enabled = false;
        jdata->num_terminated = jdata->num_procs;
        ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_TERMINATED);
        /* if it was a dynamic spawn, then we better tell them this didn't work */
        if (ORTE_JOBID_INVALID != jdata->originator.jobid) {
            rc = jobstate;
            answer = OBJ_NEW(opal_buffer_t);
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &rc, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(caddy);
                return;
            }
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jdata->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(caddy);
                return;
            }
            /* pack the room number */
            rmptr = &room;
            if (orte_get_attribute(&jdata->attributes, ORTE_JOB_ROOM_NUM, (void**)&rmptr, OPAL_INT)) {
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &room, 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(caddy);
                    return;
                }
            }
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                 "%s errmgr:dvm sending dyn error release of job %s to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(jdata->jobid),
                                 ORTE_NAME_PRINT(&jdata->originator)));
            if (0 > (ret = orte_rml.send_buffer_nb(&jdata->originator, answer,
                                                   ORTE_RML_TAG_LAUNCH_RESP,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
            }
        }
        OBJ_RELEASE(caddy);
        return;
    }

    if (ORTE_JOB_STATE_FAILED_TO_START == jobstate ||
        ORTE_JOB_STATE_FAILED_TO_LAUNCH == jobstate) {
        /* the job object for this job will have been NULL'd
         * in the array if the job was solely local. If it isn't
         * NULL, then we need to tell everyone else to die
         */
        aborted_proc = NULL;
        if (orte_get_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, (void**)&aborted_proc, OPAL_PTR)) {
            sts = aborted_proc->exit_code;
            if (ORTE_PROC_MY_NAME->jobid == jdata->jobid) {
                if (WIFSIGNALED(sts)) { /* died on signal */
#ifdef WCOREDUMP
                    if (WCOREDUMP(sts)) {
                        orte_show_help("help-plm-base.txt", "daemon-died-signal-core", true,
                                       WTERMSIG(sts));
                        sts = WTERMSIG(sts);
                    } else {
                        orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                                       WTERMSIG(sts));
                        sts = WTERMSIG(sts);
                    }
#else
                    orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                                   WTERMSIG(sts));
                    sts = WTERMSIG(sts);
#endif /* WCOREDUMP */
                } else {
                    orte_show_help("help-plm-base.txt", "daemon-died-no-signal", true,
                                   WEXITSTATUS(sts));
                    sts = WEXITSTATUS(sts);
                }
            }
        }
        /* if this is the daemon job, then we need to ensure we
         * output an error message indicating we couldn't launch the
         * daemons */
        if (jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
            orte_show_help("help-errmgr-base.txt", "failed-daemon-launch", true);
        }
    }

    /* if the daemon job aborted and we haven't heard from everyone yet,
     * then this could well have been caused by a daemon not finding
     * a way back to us. In this case, output a message indicating a daemon
     * died without reporting. Otherwise, say nothing as we
     * likely already output an error message */
    if (ORTE_JOB_STATE_ABORTED == jobstate &&
        jdata->jobid == ORTE_PROC_MY_NAME->jobid &&
        jdata->num_procs != jdata->num_reported) {
        orte_show_help("help-errmgr-base.txt", "failed-daemon", true);
    }

    OBJ_RELEASE(caddy);
}

static void proc_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_proc_t *pptr, *proct;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;
    int i;
    int32_t i32, *i32ptr;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:dvm: for proc %s state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state)));

    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        goto cleanup;
    }

    /* get the job object */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        /* could be a race condition */
        goto cleanup;
    }
    pptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);

    /* we MUST handle a communication failure before doing anything else
     * as it requires some special care to avoid normal termination issues
     * for local application procs
     */
    if (ORTE_PROC_STATE_COMM_FAILED == state) {
        /* is this to a daemon? */
        if (ORTE_PROC_MY_NAME->jobid != proc->jobid) {
            /* nope - ignore it */
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                 "%s Comm failure to non-daemon proc - ignoring it",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            goto cleanup;
        }
        /* if this is my own connection, ignore it */
        if (ORTE_PROC_MY_NAME->vpid == proc->vpid) {
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                 "%s Comm failure on my own connection - ignoring it",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            goto cleanup;
        }
        /* mark the daemon as gone */
        ORTE_FLAG_UNSET(pptr, ORTE_PROC_FLAG_ALIVE);
        /* if we have ordered orteds to terminate or abort
         * is in progress, record it */
        if (orte_orteds_term_ordered || orte_abnormal_term_ordered) {
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                 "%s Comm failure: daemons terminating - recording daemon %s as gone",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(proc)));
            /* remove from dependent routes, if it is one */
            orte_routed.route_lost(proc);
            /* if all my routes and local children are gone, then terminate ourselves */
            if (0 == orte_routed.num_routes()) {
                for (i=0; i < orte_local_children->size; i++) {
                    if (NULL != (proct = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i)) &&
                        ORTE_FLAG_TEST(pptr, ORTE_PROC_FLAG_ALIVE) && proct->state < ORTE_PROC_STATE_UNTERMINATED) {
                        /* at least one is still alive */
                        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                             "%s Comm failure: at least one proc (%s) still alive",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_NAME_PRINT(&proct->name)));
                        goto cleanup;
                    }
                }
                /* call our appropriate exit procedure */
                OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                     "%s errmgr_dvm: all routes and children gone - ordering exit",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
        } else {
                OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                     "%s Comm failure: %d routes remain alive",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     (int)orte_routed.num_routes()));
            }
            goto cleanup;
        }
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s Comm failure: daemon %s - aborting",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(proc)));
        /* record the first one to fail */
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            /* output an error message so the user knows what happened */
            orte_show_help("help-errmgr-base.txt", "node-died", true, pptr->node->name);
            /* mark the daemon job as failed */
            jdata->state = ORTE_JOB_STATE_COMM_FAILED;
            /* point to the lowest rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            /* update our exit code */
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* just in case the exit code hadn't been set, do it here - this
             * won't override any reported exit code */
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERR_COMM_FAILURE);
        }
        goto cleanup;
    }

    /* update the proc state - can get multiple reports on a proc
     * depending on circumstances, so ensure we only do this once
     */
    if (pptr->state < ORTE_PROC_STATE_TERMINATED) {
        pptr->state = state;
    }

    /* if we were ordered to terminate, mark this proc as dead and see if
     * any of our routes or local  children remain alive - if not, then
     * terminate ourselves. */
    if (orte_orteds_term_ordered) {
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL != (proct = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                if (ORTE_FLAG_TEST(proct, ORTE_PROC_FLAG_ALIVE)) {
                    goto keep_going;
                }
            }
        }
        /* if all my routes and children are gone, then terminate
           ourselves nicely (i.e., this is a normal termination) */
        if (0 == orte_routed.num_routes()) {
            OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                                 "%s errmgr:default:dvm all routes gone - exiting",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
        }
    }

 keep_going:
    /* ensure we record the failed proc properly so we can report
     * the error once we terminate
     */
    switch (state) {
    case ORTE_PROC_STATE_KILLED_BY_CMD:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s killed by cmd",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        /* we ordered this proc to die, so it isn't an abnormal termination
         * and we don't flag it as such
         */
        if (jdata->num_terminated >= jdata->num_procs) {
            /* this job has terminated */
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
        }
        /* don't abort the job as this isn't an abnormal termination */
        break;

    case ORTE_PROC_STATE_ABORTED:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s aborted",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            jdata->state = ORTE_JOB_STATE_ABORTED;
            /* point to the first rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* kill the job */
            _terminate_job(jdata->jobid);
        }
        break;

    case ORTE_PROC_STATE_ABORTED_BY_SIG:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s aborted by signal",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            jdata->state = ORTE_JOB_STATE_ABORTED_BY_SIG;
            /* point to the first rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* kill the job */
            _terminate_job(jdata->jobid);
        }
        break;

    case ORTE_PROC_STATE_TERM_WO_SYNC:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s terminated without sync",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            jdata->state = ORTE_JOB_STATE_ABORTED_WO_SYNC;
            /* point to the first rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* now treat a special case - if the proc exit'd without a required
             * sync, it may have done so with a zero exit code. We want to ensure
             * that the user realizes there was an error, so in this -one- case,
             * we overwrite the process' exit code with the default error code
             */
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
             /* kill the job */
            _terminate_job(jdata->jobid);
       }
        break;

    case ORTE_PROC_STATE_FAILED_TO_START:
    case ORTE_PROC_STATE_FAILED_TO_LAUNCH:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc),
                             orte_proc_state_to_str(state)));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            opal_buffer_t *answer;
            int id, *idptr, ret;

            if (ORTE_PROC_STATE_FAILED_TO_START) {
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
            } else {
                jdata->state = ORTE_JOB_STATE_FAILED_TO_LAUNCH;
            }
            /* point to the first rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            /* send a notification to the requestor - indicate that this is a spawn response */
            answer = OBJ_NEW(opal_buffer_t);
            /* pack the return status */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &pptr->exit_code, 1, OPAL_INT32))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            /* pack the jobid to be returned */
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jdata->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                goto CLEANUP;
            }
            idptr = &id;
            if (orte_get_attribute(&jdata->attributes, ORTE_JOB_ROOM_NUM, (void**)&idptr, OPAL_INT)) {
                /* pack the sender's index to the tracking object */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, idptr, 1, OPAL_INT))) {
                    ORTE_ERROR_LOG(ret);
                    OBJ_RELEASE(answer);
                    goto CLEANUP;
                }
            }
            if (orte_get_attribute(&jdata->attributes, ORTE_JOB_FIXED_DVM, NULL, OPAL_BOOL)) {
                /* we need to send the requestor more info about what happened */
                opal_dss.pack(answer, &jdata->state, 1, ORTE_JOB_STATE_T);
                opal_dss.pack(answer, &pptr, 1, ORTE_PROC);
                opal_dss.pack(answer, &pptr->node, 1, ORTE_NODE);
            }
            /* return response */
            if (0 > (ret = orte_rml.send_buffer_nb(&jdata->originator, answer,
                                                   ORTE_RML_TAG_LAUNCH_RESP,
                                                   orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
            }
            /* record that we notified about this job */
            jdata->state = ORTE_JOB_STATE_NOTIFIED;
          CLEANUP:
            /* kill the job */
            _terminate_job(jdata->jobid);
        }
        /* if this was a daemon, report it */
        if (jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
            /* output a message indicating we failed to launch a daemon */
            orte_show_help("help-errmgr-base.txt", "failed-daemon-launch", true);
        }
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
        break;

    case ORTE_PROC_STATE_CALLED_ABORT:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s called abort with exit code %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc), pptr->exit_code));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            jdata->state = ORTE_JOB_STATE_CALLED_ABORT;
            /* point to the first proc to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* kill the job */
            _terminate_job(jdata->jobid);
        }
        break;

    case ORTE_PROC_STATE_TERM_NON_ZERO:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s exited with non-zero status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc),
                             pptr->exit_code));
        ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
        /* track the number of non-zero exits */
        i32 = 0;
        i32ptr = &i32;
        orte_get_attribute(&jdata->attributes, ORTE_JOB_NUM_NONZERO_EXIT, (void**)&i32ptr, OPAL_INT32);
        ++i32;
        orte_set_attribute(&jdata->attributes, ORTE_JOB_NUM_NONZERO_EXIT, ORTE_ATTR_LOCAL, i32ptr, OPAL_INT32);
        if (orte_abort_non_zero_exit) {
            if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
                jdata->state = ORTE_JOB_STATE_NON_ZERO_TERM;
                /* point to the first rank to cause the problem */
                orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(pptr);
                ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
                /* kill the job */
                _terminate_job(jdata->jobid);
            }
        } else {
            /* user requested we consider this normal termination */
            if (jdata->num_terminated >= jdata->num_procs) {
                /* this job has terminated */
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
            }
        }
        break;

    case ORTE_PROC_STATE_HEARTBEAT_FAILED:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s heartbeat failed",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_ABORTED)) {
            jdata->state = ORTE_JOB_STATE_HEARTBEAT_FAILED;
            /* point to the first rank to cause the problem */
            orte_set_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, ORTE_ATTR_LOCAL, pptr, OPAL_PTR);
            /* retain the object so it doesn't get free'd */
            OBJ_RETAIN(pptr);
            ORTE_FLAG_SET(jdata, ORTE_JOB_FLAG_ABORTED);
            ORTE_UPDATE_EXIT_STATUS(pptr->exit_code);
            /* kill the job */
            _terminate_job(jdata->jobid);
        }
        /* remove from dependent routes, if it is one */
        orte_routed.route_lost(proc);
        break;

    case ORTE_PROC_STATE_UNABLE_TO_SEND_MSG:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: unable to send message to proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        /* if this proc is one of my daemons, then we are truly
         * hosed - so just exit out
         */
        if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
            ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
            break;
        }
        break;

    default:
        /* shouldn't get this, but terminate job if required */
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s default error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc),
                             orte_proc_state_to_str(state)));
        if (jdata->num_terminated == jdata->num_procs) {
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_TERMINATED);
        }
        break;
    }
    /* if the waitpid fired, be sure to let the state machine know */
    if (ORTE_FLAG_TEST(pptr, ORTE_PROC_FLAG_WAITPID)) {
        ORTE_ACTIVATE_PROC_STATE(&pptr->name, ORTE_PROC_STATE_WAITPID_FIRED);
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static int predicted_fault(opal_list_t *proc_list,
                           opal_list_t *node_list,
                           opal_list_t *suggested_map)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int ft_event(int state)
{
    return ORTE_SUCCESS;
}
