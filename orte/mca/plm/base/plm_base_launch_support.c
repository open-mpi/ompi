/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <ctype.h>

#include "opal/hash_string.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orte/util/dash_host/dash_host.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/odls/odls.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/rtc/rtc.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/util/regex.h"
#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

void orte_plm_base_daemons_reported(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    
#if OPAL_HAVE_HWLOC
    {
        hwloc_topology_t t;
        orte_node_t *node;
        int i;

        /* if we got back topology info from the first node, then we use
         * it as the "standard" for all other nodes unless they sent
         * back their own topology */
        if (1 < orte_process_info.num_procs) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 1)) ||
                NULL == (t = node->topology)) {
                /* something is wrong */
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                ORTE_FORCED_TERMINATE(ORTE_ERR_NOT_FOUND);
                OBJ_RELEASE(caddy);
                return;
            }
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:setting topo to that from node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name));
            for (i=2; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                if (NULL == node->topology) {
                    node->topology = t;
                }
            }
        }
        /* if this is an unmanaged allocation, then set the default
         * slots on each node as directed or using default
         */
        if (!orte_managed_allocation) {
            if (NULL != orte_set_slots &&
                0 != strncmp(orte_set_slots, "none", strlen(orte_set_slots))) {
                for (i=0; i < orte_node_pool->size; i++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                        continue;
                    }
                    if (!ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_SLOTS_GIVEN)) {
                        if (0 == strncmp(orte_set_slots, "cores", strlen(orte_set_slots))) {
                            node->slots = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                                             HWLOC_OBJ_CORE, 0,
                                                                             OPAL_HWLOC_LOGICAL);
                        } else if (0 == strncmp(orte_set_slots, "sockets", strlen(orte_set_slots))) {
                            if (0 == (node->slots = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                                                       HWLOC_OBJ_SOCKET, 0,
                                                                                       OPAL_HWLOC_LOGICAL))) {
                                /* some systems don't report sockets - in this case,
                                 * use numanodes
                                 */
                                node->slots = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                                                 HWLOC_OBJ_NODE, 0,
                                                                                 OPAL_HWLOC_LOGICAL);
                            }
                        } else if (0 == strncmp(orte_set_slots, "numas", strlen(orte_set_slots))) {
                            node->slots = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                                             HWLOC_OBJ_NODE, 0,
                                                                             OPAL_HWLOC_LOGICAL);
                        } else if (0 == strncmp(orte_set_slots, "hwthreads", strlen(orte_set_slots))) {
                            node->slots = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                                             HWLOC_OBJ_PU, 0,
                                                                             OPAL_HWLOC_LOGICAL);
                        } else {
                            /* must be a number */
                            node->slots = strtol(orte_set_slots, NULL, 10);
                        }
                    }
                }
            }
        }
    }
#endif

    if (orte_display_allocation) {
        orte_ras_base_display_alloc();
    }

    /* progress the job */
    caddy->jdata->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_VM_READY);

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_allocation_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* move the state machine along */
    caddy->jdata->state = ORTE_JOB_STATE_ALLOCATION_COMPLETE;
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_LAUNCH_DAEMONS);

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_daemons_launched(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* do NOT increment the state - we wait for the
     * daemons to report that they have actually
     * started before moving to the right state
     */
    /* cleanup */
    OBJ_RELEASE(caddy);
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

void orte_plm_base_vm_ready(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* progress the job */
    caddy->jdata->state = ORTE_JOB_STATE_VM_READY;

    /* position any required files */
    if (ORTE_SUCCESS != orte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_mapping_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* move the state machine along */
    caddy->jdata->state = ORTE_JOB_STATE_MAP_COMPLETE;
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_SYSTEM_PREP);

    /* cleanup */
    OBJ_RELEASE(caddy);
}


void orte_plm_base_setup_job(int fd, short args, void *cbdata)
{
    int rc;
    int i;
    orte_app_context_t *app;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:setup_job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (ORTE_JOB_STATE_INIT != caddy->job_state) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* start by getting a jobid */
    if (ORTE_JOBID_INVALID == caddy->jdata->jobid) {
        if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(caddy->jdata))) {
            ORTE_ERROR_LOG(rc);
            ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            OBJ_RELEASE(caddy);
            return;
        }

        /* store it on the global job data pool - this is the key
         * step required before we launch the daemons. It allows
         * the orte_rmaps_base_setup_virtual_machine routine to
         * search all apps for any hosts to be used by the vm
         */
        opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(caddy->jdata->jobid), caddy->jdata);
    }
    
    /* if job recovery is not enabled, set it to default */
    if (!ORTE_FLAG_TEST(caddy->jdata, ORTE_JOB_FLAG_RECOVERABLE) &&
        orte_enable_recovery) {
        ORTE_FLAG_SET(caddy->jdata, ORTE_JOB_FLAG_RECOVERABLE);
    }

    /* if app recovery is not defined, set apps to defaults */
    for (i=0; i < caddy->jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(caddy->jdata->apps, i))) {
            continue;
        }
        if (!orte_get_attribute(&app->attributes, ORTE_APP_RECOV_DEF, NULL, OPAL_BOOL)) {
            orte_set_attribute(&app->attributes, ORTE_APP_MAX_RESTARTS, ORTE_ATTR_LOCAL, &orte_max_restarts, OPAL_INT32);
        }
    }

    /* set the job state to the next position */
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_INIT_COMPLETE);

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_setup_job_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* nothing to do here but move along */
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_ALLOCATE);
    OBJ_RELEASE(caddy);
}

void orte_plm_base_complete_setup(int fd, short args, void *cbdata)
{
    orte_job_t *jdata, *jdatorted;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    opal_output_verbose(5, orte_plm_base_framework.framework_output,
                        "%s complete_setup on job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(caddy->jdata->jobid));

    /* if we don't want to launch the apps, now is the time to leave */
    if (orte_do_not_launch) {
        orte_never_launched = true;
        ORTE_FORCED_TERMINATE(0);
        OBJ_RELEASE(caddy);
        return;
    }

    /* bozo check */
    if (ORTE_JOB_STATE_SYSTEM_PREP != caddy->job_state) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* convenience */
    jdata = caddy->jdata;

    /* quick sanity check - is the stdin target within range
     * of the job?
     */
    if (ORTE_VPID_WILDCARD != jdata->stdin_target &&
        ORTE_VPID_INVALID != jdata->stdin_target &&
        jdata->num_procs <= jdata->stdin_target) {
        /* this request cannot be met */
        orte_show_help("help-plm-base.txt", "stdin-target-out-of-range", true,
                       ORTE_VPID_PRINT(jdata->stdin_target),
                       ORTE_VPID_PRINT(jdata->num_procs));
        orte_never_launched = true;
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    orte_process_info.num_procs = jdatorted->num_procs;

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* ensure our routing plan is up-to-date */
    orte_routed.update_routing_plan();
    
    /* If this job is being started by me, then there is nothing
     * further we need to do as any user directives (e.g., to tie
     * off IO to /dev/null) will have been included in the launch
     * message and the IOF knows how to handle any default situation.
     * However, if this is a proxy spawn request, then the spawner
     * might be a tool that wants IO forwarded to it. If that's the
     * situation, then the job object will contain an attribute
     * indicating that request */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_FWDIO_TO_TOOL, NULL, OPAL_BOOL)) {
        /* send a message to our IOF containing the requested pull */
        ORTE_IOF_PROXY_PULL(jdata, &jdata->originator);
        /* the tool will PUSH its stdin, so nothing we need to do here
         * about stdin */
    }
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Notify the Global SnapC component regarding new job (even if it was restarted)
     */
    {
        int rc;
        if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(jdata->jobid) ) ) {
            /* Silent Failure :/ JJH */
            ORTE_ERROR_LOG(rc);
        }
    }
#endif

#if OPAL_HAVE_HWLOC
    {
        orte_node_t *node;
        uint32_t h;
        orte_vpid_t *vptr;
        int i, rc;
        char *serial_number;

        /* if coprocessors were detected, now is the time to
         * identify who is attached to what host - this info
         * will be shipped to the daemons in the nidmap. Someday,
         * there may be a direct way for daemons on coprocessors
         * to detect their hosts - but not today.
         */
        if (orte_coprocessors_detected) {
            /* cycle thru the nodes looking for coprocessors */
            for (i=0; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                /* if we don't have a serial number, then we are not a coprocessor */
                serial_number = NULL;
                if (!orte_get_attribute(&node->attributes, ORTE_NODE_SERIAL_NUMBER, (void**)&serial_number, OPAL_STRING)) {
                    continue;
                }
                if (NULL != serial_number) {
                    /* if we have a serial number, then we are a coprocessor - so
                     * compute our hash and lookup our hostid
                     */
                    OPAL_HASH_STR(serial_number, h);
                    free(serial_number);
                    if (OPAL_SUCCESS != (rc = opal_hash_table_get_value_uint32(orte_coprocessors, h,
                                                                               (void**)&vptr))) {
                        ORTE_ERROR_LOG(rc);
                        break;
                    }
                    orte_set_attribute(&node->attributes, ORTE_NODE_HOSTID, ORTE_ATTR_LOCAL, vptr, ORTE_VPID);
                }
            }
        }
        /* done with the coprocessor mapping at this time */
        if (NULL != orte_coprocessors) {
            OBJ_RELEASE(orte_coprocessors);
        }
    }
#endif

    /* load any controls into the system */
    orte_rtc.assign(jdata);

    /* set the job state to the next position */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_LAUNCH_APPS);

    /* cleanup */
    OBJ_RELEASE(caddy);
}

/* catch timeout to allow cmds to progress */
static void timer_cb(int fd, short event, void *cbdata)
{
    orte_job_t *jdata = (orte_job_t*)cbdata;
    orte_timer_t *timer=NULL;

    /* declare launch failed */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_FAILED_TO_START);

    /* free event */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_FAILURE_TIMER_EVENT, (void**)&timer, OPAL_PTR)) {
        /* timer is an orte_timer_t object */
        OBJ_RELEASE(timer);
        orte_remove_attribute(&jdata->attributes, ORTE_JOB_FAILURE_TIMER_EVENT);
    }
}

void orte_plm_base_launch_apps(int fd, short args, void *cbdata)
{
    orte_job_t *jdata;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_timer_t *timer;
    orte_grpcomm_signature_t *sig;

    /* convenience */
    jdata = caddy->jdata;

    if (ORTE_JOB_STATE_LAUNCH_APPS != caddy->job_state) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:launch_apps for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* setup the buffer */
    buffer = OBJ_NEW(opal_buffer_t);

    /* pack the add_local_procs command */
    command = ORTE_DAEMON_ADD_LOCAL_PROCS;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* get the local launcher's required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(buffer, jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    
    /* goes to all daemons */
    sig = OBJ_NEW(orte_grpcomm_signature_t);
    sig->signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    sig->signature[0].jobid = ORTE_PROC_MY_NAME->jobid;
    sig->signature[0].vpid = ORTE_VPID_WILDCARD;
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(sig, ORTE_RML_TAG_DAEMON, buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        OBJ_RELEASE(sig);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    OBJ_RELEASE(buffer);
    /* maintain accounting */
    OBJ_RELEASE(sig);

    /* track that we automatically are considered to have reported - used
     * only to report launch progress
     */
    caddy->jdata->num_daemons_reported++;

    /* if requested, setup a timer - if we don't launch within the
     * defined time, then we know things have failed
     */
    if (0 < orte_startup_timeout) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:launch defining timeout for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        timer = OBJ_NEW(orte_timer_t);
        timer->payload = jdata;
        opal_event_evtimer_set(orte_event_base,
                               timer->ev, timer_cb, jdata);
        opal_event_set_priority(timer->ev, ORTE_ERROR_PRI);
        timer->tv.tv_sec = orte_startup_timeout;
        timer->tv.tv_usec = 0;
        orte_set_attribute(&jdata->attributes, ORTE_JOB_FAILURE_TIMER_EVENT, ORTE_ATTR_LOCAL, timer, OPAL_PTR);
        opal_event_evtimer_add(timer->ev, &timer->tv);
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_post_launch(int fd, short args, void *cbdata)
{
    int32_t rc;
    orte_job_t *jdata;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_process_name_t name;
    orte_timer_t *timer=NULL;

    /* convenience */
    jdata = caddy->jdata;

    /* if a timer was defined, cancel it */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_FAILURE_TIMER_EVENT, (void**)&timer, OPAL_PTR)) {
        opal_event_evtimer_del(timer->ev);
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:launch deleting timeout for job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        OBJ_RELEASE(timer);
        orte_remove_attribute(&jdata->attributes, ORTE_JOB_FAILURE_TIMER_EVENT);
    }

    if (ORTE_JOB_STATE_RUNNING != caddy->job_state) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* complete wiring up the iof */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:launch wiring up iof for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* push stdin - the IOF will know what to do with the specified target */
    name.jobid = jdata->jobid;
    name.vpid = jdata->stdin_target;
    
    if (ORTE_SUCCESS != (rc = orte_iof.push(&name, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

void orte_plm_base_registered(int fd, short args, void *cbdata)
{
    int ret;
    int32_t rc;
    orte_job_t *jdata;
    opal_buffer_t *answer;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* convenience */
    jdata = caddy->jdata;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:launch %s registered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    if (ORTE_JOB_STATE_REGISTERED != caddy->job_state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:launch job %s not registered - state %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             orte_job_state_to_str(caddy->job_state)));
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* if this isn't a dynamic spawn, just cleanup */
    if (ORTE_JOBID_INVALID == jdata->originator.jobid) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:launch job %s is not a dynamic spawn",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        goto cleanup;
    }
    /* if it was a dynamic spawn, send the response */
    rc = ORTE_SUCCESS;
    answer = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &rc, 1, OPAL_INT32))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jdata->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:launch sending dyn release of job %s to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         ORTE_NAME_PRINT(&jdata->originator)));
    if (0 > (ret = orte_rml.send_buffer_nb(&jdata->originator, answer,
                                           ORTE_RML_TAG_PLM_PROXY,
                                           orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(answer);
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

 cleanup:
    /* need to init_after_spawn for debuggers */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_READY_FOR_DEBUGGERS);

    OBJ_RELEASE(caddy);
}

/* daemons callback when they start - need to listen for them */
static bool orted_failed_launch;
static orte_job_t *jdatorted=NULL;

void orte_plm_base_daemon_callback(int status, orte_process_name_t* sender,
                                   opal_buffer_t *buffer,
                                   orte_rml_tag_t tag, void *cbdata)
{
    char *rml_uri = NULL, *ptr;
    int rc, idx;
    orte_proc_t *daemon=NULL;
    char *nodename;
    orte_node_t *node;
    orte_job_t *jdata;
    orte_process_name_t dname;
    opal_buffer_t *relay;
    
    /* get the daemon job, if necessary */
    if (NULL == jdatorted) {
        jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    }

    /* multiple daemons could be in this buffer, so unpack until we exhaust the data */
    idx = 1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(buffer, &dname, &idx, ORTE_NAME))) {
        /* unpack its contact info */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &idx, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        
        /* set the contact info into the hash table */
        orte_rml.set_contact_info(rml_uri);

        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch from daemon %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&dname)));
        
        /* update state and record for this daemon contact info */
        if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdatorted->procs, dname.vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        daemon->state = ORTE_PROC_STATE_RUNNING;
        daemon->rml_uri = rml_uri;
        /* record that this daemon is alive */
        ORTE_FLAG_SET(daemon, ORTE_PROC_FLAG_ALIVE);

        /* unpack the node name */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nodename, &idx, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        if (!orte_have_fqdn_allocation) {
            /* remove any domain info */
            if (NULL != (ptr = strchr(nodename, '.'))) {
                *ptr = '\0';
                ptr = strdup(nodename);
                free(nodename);
                nodename = ptr;
            }
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch from daemon %s on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&dname), nodename));
        
        /* look this node up, if necessary */
        if (!orte_plm_globals.daemon_nodes_assigned_at_launch) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:orted_report_launch attempting to assign daemon %s to node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&dname), nodename));
            for (idx=0; idx < orte_node_pool->size; idx++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, idx))) {
                    continue;
                }
                if (ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_LOC_VERIFIED)) {
                    /* already assigned */
                    continue;
                }
                if (0 == strcmp(nodename, node->name)) {
                    /* flag that we verified the location */
                    ORTE_FLAG_SET(node, ORTE_NODE_FLAG_LOC_VERIFIED);
                    if (node == daemon->node) {
                        /* it wound up right where it should */
                        break;
                    }
                    /* remove the prior association */
                    if (NULL != daemon->node) {
                        OBJ_RELEASE(daemon->node);
                    }
                    if (NULL != node->daemon) {
                        OBJ_RELEASE(node->daemon);
                    }
                    /* associate this daemon with the node */
                    node->daemon = daemon;
                    OBJ_RETAIN(daemon);
                    /* associate this node with the daemon */
                    daemon->node = node;
                    OBJ_RETAIN(node);
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                         "%s plm:base:orted_report_launch assigning daemon %s to node %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&daemon->name), node->name));
                    break;
                }
            }
        }

        node = daemon->node;
        if (NULL == node) {
            /* this shouldn't happen - it indicates an error in the
             * prior node matching logic, so report it and error out
             */
            orte_show_help("help-plm-base.txt", "daemon-no-assigned-node", true,
                           ORTE_NAME_PRINT(&daemon->name), nodename);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        
        /* mark the daemon as launched */
        ORTE_FLAG_SET(node, ORTE_NODE_FLAG_DAEMON_LAUNCHED);

        if (orte_retain_aliases) {
            char *alias, **atmp=NULL;
            uint8_t naliases, ni;
            /* first, store the nodename itself as an alias. We do
             * this in case the nodename isn't the same as what we
             * were given by the allocation. For example, a hostfile
             * might contain an IP address instead of the value returned
             * by gethostname, yet the daemon will have returned the latter
             * and apps may refer to the host by that name
             */
            opal_argv_append_nosize(&atmp, nodename);
            /* unpack and store the provided aliases */
            idx = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &naliases, &idx, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            for (ni=0; ni < naliases; ni++) {
                idx = 1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &alias, &idx, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    orted_failed_launch = true;
                    goto CLEANUP;
                }
                opal_argv_append_nosize(&atmp, alias);
                free(alias);
            }
            alias = opal_argv_join(atmp, ',');
            opal_argv_free(atmp);
            orte_set_attribute(&node->attributes, ORTE_NODE_ALIAS, ORTE_ATTR_LOCAL, alias, OPAL_STRING);
            free(alias);
        }

#if OPAL_HAVE_HWLOC
        {
            char *coprocessors, **sns, *sig;
            uint32_t h;
            hwloc_topology_t topo;
            orte_topology_t *t;
            int i;
            bool found;
            uint8_t tflag;

            /* store the local resources for that node */
            idx=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &tflag, &idx, OPAL_UINT8))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            if (1 == tflag) {
                idx=1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &sig, &idx, OPAL_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    orted_failed_launch = true;
                    goto CLEANUP;
                }
                idx=1;
                if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &topo, &idx, OPAL_HWLOC_TOPO))) {
                    ORTE_ERROR_LOG(rc);
                    orted_failed_launch = true;
                    goto CLEANUP;
                }
                OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                     "%s RECEIVED TOPOLOGY FROM NODE %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename));
                if (10 < opal_output_get_verbosity(orte_plm_base_framework.framework_output)) {
                    opal_dss.dump(0, topo, OPAL_HWLOC_TOPO);
                }
                if (1 == dname.vpid || orte_hetero_nodes) {
                    /* the user has told us that something is different, so just store it */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                         "%s ADDING TOPOLOGY PER USER REQUEST",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    t = OBJ_NEW(orte_topology_t);
                    /* filter the topology as we'll need it that way later */
                    opal_hwloc_base_filter_cpus(topo);
                    t->topo = topo;
                    t->sig = sig;
                    opal_pointer_array_add(orte_node_topologies, t);
                    node->topology = topo;
                } else {
                    /* do we already have this topology from some other node? */
                    found = false;
                    for (i=0; i < orte_node_topologies->size; i++) {
                        if (NULL == (t = (orte_topology_t*)opal_pointer_array_get_item(orte_node_topologies, i))) {
                            continue;
                        }
                        /* just check the signature */
                        if (0 == strcmp(sig, t->sig)) {
                            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                                 "%s TOPOLOGY ALREADY RECORDED",
                                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                            found = true;
                            node->topology = t->topo;
                            hwloc_topology_destroy(topo);
                            free(sig);
                            break;
                        }
                    }
                    if (!found) {
                        /* nope - add it */
                        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                             "%s NEW TOPOLOGY - ADDING",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                        t = OBJ_NEW(orte_topology_t);
                        /* filter the topology as we'll need it that way later */
                        opal_hwloc_base_filter_cpus(topo);
                        t->topo = topo;
                        t->sig = sig;
                        opal_pointer_array_add(orte_node_topologies, t);
                        node->topology = topo;
                    }
                }
            }
        
            /* unpack any coprocessors */
            idx=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &coprocessors, &idx, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            if (NULL != coprocessors) {
                /* init the hash table, if necessary */
                if (NULL == orte_coprocessors) {
                    orte_coprocessors = OBJ_NEW(opal_hash_table_t);
                    opal_hash_table_init(orte_coprocessors, orte_process_info.num_procs);
                }
                /* separate the serial numbers of the coprocessors
                 * on this host
                 */
                sns = opal_argv_split(coprocessors, ',');
                for (idx=0; NULL != sns[idx]; idx++) {
                    /* compute the hash */
                    OPAL_HASH_STR(sns[idx], h);
                    /* mark that this coprocessor is hosted by this node */
                    opal_hash_table_set_value_uint32(orte_coprocessors, h, (void*)&node->daemon->name.vpid);
                }
                opal_argv_free(sns);
                free(coprocessors);
                orte_coprocessors_detected = true;
            }
            /* see if this daemon is on a coprocessor */
            idx=1;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &coprocessors, &idx, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            if (NULL != coprocessors) {
                if (orte_get_attribute(&node->attributes, ORTE_NODE_SERIAL_NUMBER, NULL, OPAL_STRING)) {
                    /* this is not allowed - a coprocessor cannot be host
                     * to another coprocessor at this time
                     */
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
                    orted_failed_launch = true;
                    free(coprocessors);
                    goto CLEANUP;
                }
                orte_set_attribute(&node->attributes, ORTE_NODE_SERIAL_NUMBER, ORTE_ATTR_LOCAL, coprocessors, OPAL_STRING);
                free(coprocessors);
                orte_coprocessors_detected = true;
            }
        }
#endif

    CLEANUP:
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch %s for daemon %s at contact %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orted_failed_launch ? "failed" : "completed",
                             ORTE_NAME_PRINT(&dname),
                             (NULL == daemon) ? "UNKNOWN" : daemon->rml_uri));
        
        if (orted_failed_launch) {
            ORTE_ACTIVATE_JOB_STATE(jdatorted, ORTE_JOB_STATE_FAILED_TO_START);
            return;
        } else {
            jdatorted->num_reported++;
            if (jdatorted->num_procs == jdatorted->num_reported) {
                jdatorted->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
                /* activate the daemons_reported state for all jobs
                 * whose daemons were launched
                 */
                for (idx=1; idx < orte_job_data->size; idx++) {
                    if (NULL == (jdata = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, idx))) {
                        continue;
                    }
                    if (ORTE_JOB_STATE_DAEMONS_LAUNCHED == jdata->state) {
                        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_DAEMONS_REPORTED);
                    }
                }
            }
        }
        if (NULL != nodename) {
            free(nodename);
            nodename = NULL;
        }
        idx = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        ORTE_ACTIVATE_JOB_STATE(jdatorted, ORTE_JOB_STATE_FAILED_TO_START);
    } else if (NULL != orte_tree_launch_cmd) {
        /* if a tree-launch is underway, send the cmd back */
        relay = OBJ_NEW(opal_buffer_t);
        opal_dss.copy_payload(relay, orte_tree_launch_cmd);
        orte_rml.send_buffer_nb(sender, relay,
                                ORTE_RML_TAG_DAEMON,
                                orte_rml_send_callback, NULL);
    }

}

void orte_plm_base_daemon_failed(int st, orte_process_name_t* sender,
                                 opal_buffer_t *buffer,
                                 orte_rml_tag_t tag, void *cbdata)
{
    int status, rc;
    int32_t n;
    orte_vpid_t vpid;
    orte_proc_t *daemon=NULL;

    /* get the daemon job, if necessary */
    if (NULL == jdatorted) {
        jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    }

    /* unpack the daemon that failed */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &vpid, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto finish;
    }

    /* unpack the exit status */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &status, &n, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        status = ORTE_ERROR_DEFAULT_EXIT_CODE;
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
    } else {
        ORTE_UPDATE_EXIT_STATUS(WEXITSTATUS(status));
    }

    /* find the daemon and update its state/status */
    if (NULL == (daemon = (orte_proc_t*)opal_pointer_array_get_item(jdatorted->procs, vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto finish;
    }
    daemon->state = ORTE_PROC_STATE_FAILED_TO_START;
    daemon->exit_code = status;

 finish:
    if (NULL == daemon) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        return;
    }
    ORTE_ACTIVATE_PROC_STATE(&daemon->name, ORTE_PROC_STATE_FAILED_TO_START);
}

int orte_plm_base_setup_orted_cmd(int *argc, char ***argv)
{
    int i, loc;
    char **tmpv;
    
    /* set default location to be 0, indicating that
     * only a single word is in the cmd
     */
    loc = 0;
    /* split the command apart in case it is multi-word */
    tmpv = opal_argv_split(orte_launch_agent, ' ');
    for (i = 0; NULL != tmpv && NULL != tmpv[i]; ++i) {
        if (0 == strcmp(tmpv[i], "orted")) {
            loc = i;
        }
        opal_argv_append(argc, argv, tmpv[i]);
    }
    opal_argv_free(tmpv);
    
    return loc;
}


/* pass all options as MCA params so anything we pickup
 * from the environment can be checked for duplicates
 */
int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                          char *ess,
                                          int *proc_vpid_index,
                                          char *nodes)
{
    char *param = NULL;
    const char **tmp_value;
    int loc_id;
    char *tmp_force = NULL;
    int i, j, cnt, rc;
    orte_job_t *jdata;
    char *rml_uri;
    unsigned long num_procs;
    bool ignore;

    /* check for debug flags */
    if (orte_debug_flag) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_debug");
        opal_argv_append(argc, argv, "1");
    }
    if (orte_debug_daemons_flag) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_debug_daemons");
        opal_argv_append(argc, argv, "1");
    }
    if (orte_debug_daemons_file_flag) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_debug_daemons_file");
        opal_argv_append(argc, argv, "1");
    }
    if (orte_leave_session_attached) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_leave_session_attached");
        opal_argv_append(argc, argv, "1");
    }

    if (orted_spin_flag) {
        opal_argv_append(argc, argv, "--spin");
    }
#if OPAL_HAVE_HWLOC
    if (opal_hwloc_report_bindings) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_report_bindings");
        opal_argv_append(argc, argv, "1");
    }
    /* pass our topology signature */
    opal_argv_append(argc, argv, "--hnp-topo-sig");
    opal_argv_append(argc, argv, orte_topo_signature);
    if (orte_hetero_nodes) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_hetero_nodes");
        opal_argv_append(argc, argv, "1");
    }
#endif
    
    if (orte_map_reduce) {
        opal_argv_append(argc, argv, "--mapreduce");
    }
    if (orte_map_stddiag_to_stderr) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_map_stddiag_to_stderr");
        opal_argv_append(argc, argv, "1");
    }

    /* the following two are not mca params */
    if ((int)ORTE_VPID_INVALID != orted_debug_failure) {
        opal_argv_append(argc, argv, "--debug-failure");
        asprintf(&param, "%d", orted_debug_failure);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    if (0 < orted_debug_failure_delay) {
        opal_argv_append(argc, argv, "--debug-failure-delay");
        asprintf(&param, "%d", orted_debug_failure_delay);
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    /* tell the orted what ESS component to use */
    if (NULL != ess) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "ess");
        opal_argv_append(argc, argv, ess);
    }
    
    /* pass the daemon jobid */
    opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
    opal_argv_append(argc, argv, "orte_ess_jobid");
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* setup to pass the vpid */
    if (NULL != proc_vpid_index) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_ess_vpid");
        *proc_vpid_index = *argc;
        opal_argv_append(argc, argv, "<template>");
    }
    
    /* pass the total number of daemons that will be in the system */
    if (ORTE_PROC_IS_HNP) {
        jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        num_procs = jdata->num_procs;
    } else {
        num_procs = orte_process_info.num_procs;
    }
    opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
    opal_argv_append(argc, argv, "orte_ess_num_procs");
    asprintf(&param, "%lu", num_procs);
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass the uri of the hnp */
    if (ORTE_PROC_IS_HNP) {
        rml_uri = orte_rml.get_contact_info();
    } else {
        rml_uri = orte_rml.get_contact_info();
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_parent_uri");
        opal_argv_append(argc, argv, rml_uri);
        free(rml_uri);
    
        rml_uri = strdup(orte_process_info.my_hnp_uri);
    }
    opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
    opal_argv_append(argc, argv, "orte_hnp_uri");
    opal_argv_append(argc, argv, rml_uri);
    free(rml_uri);

    /* if we have static ports, pass the node list */
    if (orte_static_ports && NULL != nodes) {
        /* convert the nodes to a regex */
        if (ORTE_SUCCESS != (rc = orte_regex_create(nodes, &param))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_node_regex");
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    /* warn the daemons if we are using a tree spawn pattern so they
     * know they shouldn't do a rollup on their callback
     */
    if (NULL != orte_tree_launch_cmd) {
        opal_argv_append(argc, argv, "--tree-spawn");
    }

    /* if output-filename was specified, pass that along */
    if (NULL != orte_output_filename) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_output_filename");
        opal_argv_append(argc, argv, orte_output_filename);
    }
    
    /* if --xterm was specified, pass that along */
    if (NULL != orte_xterm) {
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "orte_xterm");
        opal_argv_append(argc, argv, orte_xterm);
    }
    
    /* 
     * Pass along the Aggregate MCA Parameter Sets
     */
    /* Add the 'prefix' param */
    tmp_value = NULL;
    loc_id = mca_base_var_find("opal", "mca", "base", "param_file_prefix");
    if (loc_id < 0) {
        rc = OPAL_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = mca_base_var_get_value(loc_id, &tmp_value, NULL, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if( NULL != tmp_value && NULL != tmp_value[0] ) {
        /* Could also use the short version '-am'
         * but being verbose has some value
         */
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "mca_base_param_file_prefix");
        opal_argv_append(argc, argv, tmp_value[0]);
    
        /* Add the 'path' param */
        tmp_value = NULL;
        loc_id = mca_base_var_find("opal", "mca", "base", "param_file_path");
        if (loc_id < 0) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = mca_base_var_get_value(loc_id, &tmp_value, NULL, NULL);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if( NULL != tmp_value && NULL != tmp_value[0] ) {
            opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
            opal_argv_append(argc, argv, "mca_base_param_file_path");
            opal_argv_append(argc, argv, tmp_value[0]);
        }
    
        /* Add the 'path' param */
        opal_argv_append(argc, argv, "-"OPAL_MCA_CMD_LINE_ID);
        opal_argv_append(argc, argv, "mca_base_param_file_path_force");

        tmp_value = NULL;
        loc_id = mca_base_var_find("opal", "mca", "base", "param_file_path_force");
        if (loc_id < 0) {
            rc = OPAL_ERR_NOT_FOUND;
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = mca_base_var_get_value(loc_id, &tmp_value, NULL, NULL);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if( NULL == tmp_value || NULL == tmp_value[0] ) {
            /* Get the current working directory */
            tmp_force = (char *) malloc(sizeof(char) * OPAL_PATH_MAX);
            if (NULL == getcwd(tmp_force, OPAL_PATH_MAX)) {
                free(tmp_force);
                tmp_force = strdup("");
            }

            opal_argv_append(argc, argv, tmp_force);
            free(tmp_force);
        } else {
            opal_argv_append(argc, argv, tmp_value[0]);
        }
    }

    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes and ignoring all duplicates
     */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        cnt = opal_argv_count(orted_cmd_line);    
        for (i=0; i < cnt; i+=3) {
            /* if the specified option is more than one word, we don't
             * have a generic way of passing it as some environments ignore
             * any quotes we add, while others don't - so we ignore any
             * such options. In most cases, this won't be a problem as
             * they typically only apply to things of interest to the HNP.
             * Individual environments can add these back into the cmd line
             * as they know if it can be supported
             */
            if (NULL != strchr(orted_cmd_line[i+2], ' ')) {
                continue;
            }
            /* The daemon will attempt to open the PLM on the remote
             * end. Only a few environments allow this, so the daemon
             * only opens the PLM -if- it is specifically told to do
             * so by giving it a specific PLM module. To ensure we avoid
             * confusion, do not include any directives here
             */
            if (0 == strcmp(orted_cmd_line[i+1], "plm")) {
                continue;
            }
            /* check for duplicate */
            ignore = false;
            for (j=0; j < *argc; j++) {
                if (0 == strcmp((*argv)[j], orted_cmd_line[i+1])) {
                    ignore = true;
                    break;
                }
            }
            if (!ignore) {
                /* pass it along */
                opal_argv_append(argc, argv, orted_cmd_line[i]);
                opal_argv_append(argc, argv, orted_cmd_line[i+1]);
                opal_argv_append(argc, argv, orted_cmd_line[i+2]);
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_plm_base_setup_virtual_machine(orte_job_t *jdata)
{
    orte_node_t *node, *nptr;
    orte_proc_t *proc, *pptr;
    orte_job_map_t *map=NULL;
    int rc, i;
    orte_job_t *daemons;
    opal_list_t nodes, tnodes;
    opal_list_item_t *item, *next;
    orte_app_context_t *app;
    bool one_filter = false;
    int num_nodes;
    bool default_hostfile_used;
    char *hosts;
    bool singleton=false;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:setup_vm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    map = daemons->map;

    /* if this is a dynamic spawn, then we don't make any changes to
     * the virtual machine unless specifically requested to do so
     */
    if (ORTE_JOBID_INVALID != jdata->originator.jobid) {
        OBJ_CONSTRUCT(&nodes, opal_list_t);
        if (NULL == daemons->map) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm creating map",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* this is the first time thru, so the vm is just getting
             * defined - create a map for it and put us in as we
             * are obviously already here! The ess will already
             * have assigned our node to us.
             */
            daemons->map = OBJ_NEW(orte_job_map_t);
            node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
            opal_pointer_array_add(daemons->map->nodes, (void*)node);
            ++(daemons->map->num_nodes);
            /* maintain accounting */
            OBJ_RETAIN(node);
            /* mark that this is from a singleton */
            singleton = true;
        }
        map = daemons->map;
        for (i=1; i < orte_node_pool->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            /* only add in nodes marked as "added" */
            if (!singleton && ORTE_NODE_STATE_ADDED != node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "%s plm_base:setup_vm NODE %s WAS NOT ADDED",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name));
                continue;
            }
            OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                 "%s plm_base:setup_vm ADDING NODE %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name));
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(node);
            opal_list_append(&nodes, &node->super);
            /* reset the state so it can be used for mapping */
            node->state = ORTE_NODE_STATE_UP;
        }
        map->num_new_daemons = 0;
        /* if we didn't get anything, then there is nothing else to
         * do as no other daemons are to be launched
         */
        if (0 == opal_list_get_size(&nodes)) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm no new daemons required",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            OBJ_DESTRUCT(&nodes);
            /* mark that the daemons have reported so we can proceed */
            daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
            ORTE_FLAG_UNSET(daemons, ORTE_JOB_FLAG_UPDATED);
            return ORTE_SUCCESS;
        }
        /* if we got some new nodes to launch, we need to handle it */
        goto process;
    }

    /* if we are not working with a virtual machine, then we
     * look across all jobs and ensure that the "VM" contains
     * all nodes with application procs on them
     */
    if (orte_get_attribute(&daemons->attributes, ORTE_JOB_NO_VM, NULL, OPAL_BOOL)) {
        OBJ_CONSTRUCT(&nodes, opal_list_t);
        if (NULL == daemons->map) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm creating map",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* this is the first time thru, so the vm is just getting
             * defined - create a map for it
             */
            daemons->map = OBJ_NEW(orte_job_map_t);
        }
        map = daemons->map;
        /* loop across all nodes and include those that have
         * num_procs > 0 && no daemon already on them
         */
        for (i=1; i < orte_node_pool->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                continue;
            }
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_USE", node->name));
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED DOWN", node->name));
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_INCLUDE", node->name));
                /* not to be used */
                continue;
            }
            if (0 < node->num_procs) {
                /* retain a copy for our use in case the item gets
                 * destructed along the way
                 */
                OBJ_RETAIN(node);
                opal_list_append(&nodes, &node->super);
            }
        }
        /* see if anybody had procs */
        if (0 == opal_list_get_size(&nodes)) {
            /* if the HNP has some procs, then we are still good */
            node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
            if (0 < node->num_procs) {
                OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                     "%s plm:base:setup_vm only HNP in use",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                OBJ_DESTRUCT(&nodes);
                /* mark that the daemons have reported so we can proceed */
                daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
                return ORTE_SUCCESS;
            }
            /* well, if the HNP doesn't have any procs, and neither did
             * anyone else...then we have a big problem
             */
            ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            return ORTE_ERR_FATAL;
        }
        goto process;
    }

    if (NULL == daemons->map) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm creating map",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* this is the first time thru, so the vm is just getting
         * defined - create a map for it and put us in as we
         * are obviously already here! The ess will already
         * have assigned our node to us.
         */
        daemons->map = OBJ_NEW(orte_job_map_t);
        node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        opal_pointer_array_add(daemons->map->nodes, (void*)node);
        ++(daemons->map->num_nodes);
        /* maintain accounting */
        OBJ_RETAIN(node);
    }
    map = daemons->map;
    
    /* zero-out the number of new daemons as we will compute this
     * each time we are called
     */
    map->num_new_daemons = 0;

    /* setup the list of nodes */
    OBJ_CONSTRUCT(&nodes, opal_list_t);

    /* if this is an unmanaged allocation, then we use
     * the nodes that were specified for the union of
     * all apps - there is no need to collect all
     * available nodes and "filter" them
     */
    if (!orte_managed_allocation) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s setup:vm: working unmanaged allocation",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        default_hostfile_used = false;
        OBJ_CONSTRUCT(&tnodes, opal_list_t);
        for (i=0; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
            }
            /* if the app provided a dash-host, and we are not treating
             * them as requested or "soft" locations, then use those nodes
             */
            if (!orte_soft_locations &&
                orte_get_attribute(&app->attributes, ORTE_APP_DASH_HOST, (void**)&hosts, OPAL_STRING)) {
                OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                     "%s using dash_host",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&tnodes, hosts))) {
                    ORTE_ERROR_LOG(rc);
                    free(hosts);
                    return rc;
                }
                free(hosts);
            } else if (orte_get_attribute(&app->attributes, ORTE_APP_HOSTFILE, (void**)&hosts, OPAL_STRING)) {
                /* otherwise, if the app provided a hostfile, then use that */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                     "%s using hostfile %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hosts));
                if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&tnodes, hosts))) {
                    ORTE_ERROR_LOG(rc);
                    free(hosts);
                    return rc;
                }
                free(hosts);
            } else if (NULL != orte_rankfile) {
                /* use the rankfile, if provided */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                     "%s using rankfile %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     orte_rankfile));
                if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&tnodes,
                                                                       orte_rankfile))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            } else if (NULL != orte_default_hostfile) {
                if (!default_hostfile_used) {
                    /* fall back to the default hostfile, if provided */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                         "%s using default hostfile %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         orte_default_hostfile));
                    if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&tnodes,
                                                                           orte_default_hostfile))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                    /* only include it once */
                    default_hostfile_used = true;
                }
            }
        }
        /* cycle thru the resulting list, finding the nodes on
         * the node pool array while removing ourselves
         * and all nodes that are down or otherwise unusable
         */
        while (NULL != (item = opal_list_remove_first(&tnodes))) {
            nptr = (orte_node_t*)item;
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s checking node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 nptr->name));
            for (i=0; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                if (0 != strcmp(node->name, nptr->name)) {
                    continue;
                }
                /* have a match - now see if we want this node */
                /* ignore nodes that are marked as do-not-use for this mapping */
                if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                    OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_USE", node->name));
                    /* reset the state so it can be used another time */
                    node->state = ORTE_NODE_STATE_UP;
                    break;
                }
                if (ORTE_NODE_STATE_DOWN == node->state) {
                    OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED DOWN", node->name));
                    break;
                }
                if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
                    OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_INCLUDE", node->name));
                    break;
                }
                /* if this node is us, ignore it */
                if (0 == node->index) {
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                         "%s ignoring myself",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    break;
                }
                /* we want it - add it to list */
                OBJ_RETAIN(node);
                opal_list_append(&nodes, &node->super);
            }
            OBJ_RELEASE(nptr);
        }
        OBJ_DESTRUCT(&tnodes);
        /* if we didn't get anything, then we are the only node in the
         * allocation - so there is nothing else to do as no other
         * daemons are to be launched
         */
        if (0 == opal_list_get_size(&nodes)) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm only HNP in allocation",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            OBJ_DESTRUCT(&nodes);
            /* mark that the daemons have reported so we can proceed */
            daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
            ORTE_FLAG_UNSET(daemons, ORTE_JOB_FLAG_UPDATED);
            return ORTE_SUCCESS;
        }
        /* continue processing */
        goto process;
    }

    /* construct a list of available nodes */
    for (i=1; i < orte_node_pool->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_USE", node->name));
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED DOWN", node->name));
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
                OPAL_OUTPUT_VERBOSE((10, orte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_INCLUDE", node->name));
                /* not to be used */
                continue;
            }
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(node);
            opal_list_append(&nodes, &node->super);
            /* by default, mark these as not to be included
             * so the filtering logic works correctly
             */
            ORTE_FLAG_UNSET(node, ORTE_NODE_FLAG_MAPPED);
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * system - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == opal_list_get_size(&nodes)) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm only HNP in allocation",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* cleanup */
        OBJ_DESTRUCT(&nodes);
        /* mark that the daemons have reported so we can proceed */
        daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
        ORTE_FLAG_UNSET(daemons, ORTE_JOB_FLAG_UPDATED);
        return ORTE_SUCCESS;
    }

    /* filter across the union of all app_context specs - if the HNP
     * was allocated, then we have to include
     * ourselves in case someone has specified a -host or hostfile
     * that includes the head node. We will remove ourselves later
     * as we clearly already exist
     */
    if (orte_hnp_is_allocated) {
        node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        OBJ_RETAIN(node);
        opal_list_append(&nodes, &node->super);
    }
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_filter_nodes(app, &nodes, false)) &&
            rc != ORTE_ERR_TAKE_NEXT_OPTION) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS == rc) {
            /* we filtered something */
            one_filter = true;
        }
    }

    if (one_filter) {
        /* at least one filtering option was executed, so
         * remove all nodes that were not mapped
         */
        item = opal_list_get_first(&nodes);
        while (item != opal_list_get_end(&nodes)) {
            next = opal_list_get_next(item);
            node = (orte_node_t*)item;
            if (!ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_MAPPED)) {
                opal_list_remove_item(&nodes, item);
                OBJ_RELEASE(item);
            }
            item = next;
        }
    }

    /* ensure we are not on the list */
    for (item = opal_list_get_first(&nodes);
         item != opal_list_get_end(&nodes);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
        if (0 == node->index) {
            opal_list_remove_item(&nodes, item);
            OBJ_RELEASE(item);
            break;
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * allocation - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == opal_list_get_size(&nodes)) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm only HNP left",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_DESTRUCT(&nodes);
        /* mark that the daemons have reported so we can proceed */
        daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
        ORTE_FLAG_UNSET(daemons, ORTE_JOB_FLAG_UPDATED);
        return ORTE_SUCCESS;
    }

 process:
    /* cycle thru all available nodes and find those that do not already
     * have a daemon on them - no need to include our own as we are
     * obviously already here! If a max vm size was given, then limit
     * the overall number of active nodes to the given number. Only
     * count the HNP's node if it was included in the allocation
     */
    if (orte_hnp_is_allocated) {
        num_nodes = 1;
    } else {
        num_nodes = 0;
    }
    while (NULL != (item = opal_list_remove_first(&nodes))) {
        /* if a max size was given and we are there, then exit the loop */
        if (0 < orte_max_vm_size && num_nodes == orte_max_vm_size) {
            /* maintain accounting */
            OBJ_RELEASE(item);
            break;
        }
        node = (orte_node_t*)item;
        /* if this node is already in the map, skip it */
        if (NULL != node->daemon) {
            num_nodes++;
            /* maintain accounting */
            OBJ_RELEASE(item);
            continue;
        }
        /* add the node to the map - we retained it
         * when adding it to the list, so we don't need
         * to retain it again
         */
        opal_pointer_array_add(map->nodes, (void*)node);
        ++(map->num_nodes);
        num_nodes++;
        /* create a new daemon object for this node */
        proc = OBJ_NEW(orte_proc_t);
        if (NULL == proc) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
        if (ORTE_VPID_MAX-1 <= daemons->num_procs) {
            /* no more daemons available */
            orte_show_help("help-orte-rmaps-base.txt", "out-of-vpids", true);
            OBJ_RELEASE(proc);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        proc->name.vpid = daemons->num_procs;  /* take the next available vpid */
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm add new daemon %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        /* add the daemon to the daemon job object */
        if (0 > (rc = opal_pointer_array_set_item(daemons->procs, proc->name.vpid, (void*)proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        ++daemons->num_procs;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm assigning new daemon %s to node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name),
                             node->name));
        /* point the node to the daemon */
        node->daemon = proc;
        OBJ_RETAIN(proc);  /* maintain accounting */
        /* point the proc to the node and maintain accounting */
        proc->node = node;
        OBJ_RETAIN(node);
        if (orte_plm_globals.daemon_nodes_assigned_at_launch) {
            ORTE_FLAG_SET(node, ORTE_NODE_FLAG_LOC_VERIFIED);
        } else {
            ORTE_FLAG_UNSET(node, ORTE_NODE_FLAG_LOC_VERIFIED);
        }
        /* track number of daemons to be launched */
        ++map->num_new_daemons;
        /* and their starting vpid */
        if (ORTE_VPID_INVALID == map->daemon_vpid_start) {
            map->daemon_vpid_start = proc->name.vpid;
        }
        /* loop across all app procs on this node and update their parent */
        for (i=0; i < node->procs->size; i++) {
            if (NULL != (pptr = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                pptr->parent = proc->name.vpid;
            }
        }
    }
    
    if (orte_process_info.num_procs != daemons->num_procs) {
        /* more daemons are being launched - update the routing tree to
         * ensure that the HNP knows how to route messages via
         * the daemon routing tree - this needs to be done
         * here to avoid potential race conditions where the HNP
         * hasn't unpacked its launch message prior to being
         * asked to communicate.
         */
        orte_process_info.num_procs = daemons->num_procs;

        if (orte_process_info.max_procs < orte_process_info.num_procs) {
            orte_process_info.max_procs = orte_process_info.num_procs;
        }

        /* ensure our routing plan is up-to-date */
        orte_routed.update_routing_plan();
    }

    /* mark that the daemon job changed */
    ORTE_FLAG_SET(daemons, ORTE_JOB_FLAG_UPDATED);

    /* if new daemons are being launched, mark that this job
     * caused it to happen */
    if (0 < map->num_new_daemons) {
        if (ORTE_SUCCESS != (rc = orte_set_attribute(&jdata->attributes, ORTE_JOB_LAUNCHED_DAEMONS,
                                                     true, NULL, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}
