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
 * Copyright (c) 2007-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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

#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/hwloc.h"

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
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/odls/odls.h"
#if OPAL_ENABLE_FT_CR == 1
#include "orte/mca/snapc/base/base.h"
#endif
#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "orte/mca/grpcomm/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/sensor/sensor.h"
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

        /* if the user didn't indicate that the node topologies were
         * different, then set the nodes to point to the topology
         * of the first node.
         *
         * NOTE: We do -not- point the nodes at the topology of
         * mpirun because many "homogeneous" clusters have a head
         * node that differs from all the compute nodes!
         */
        if (!orte_hetero_nodes) {
            if (NULL == (t = (hwloc_topology_t)opal_pointer_array_get_item(orte_node_topologies, 1))) {
                /* all collapsed down into mpirun's topology */
                t = (hwloc_topology_t)opal_pointer_array_get_item(orte_node_topologies, 0);
            }
            for (i=1; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                if (NULL == node->topology) {
                    node->topology = t;
                }
            }
        }
    }
#endif

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

void orte_plm_base_vm_ready(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* progress the job */
    caddy->jdata->state = ORTE_JOB_STATE_VM_READY;
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_MAP);

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
    char *modx_par, *modx_val;
    char *bar1_par, *bar1_val;
    char *bar2_par, *bar2_val;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:setup_job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (ORTE_JOB_STATE_INIT != caddy->job_state) {
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* start by getting a jobid */
    if (ORTE_SUCCESS != (rc = orte_plm_base_create_jobid(caddy->jdata))) {
        ORTE_ERROR_LOG(rc);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* store it on the global job data pool - this is the key
     * step required before we launch the daemons. It allows
     * the orte_rmaps_base_setup_virtual_machine routine to
     * search all apps for any hosts to be used by the vm
     */
    opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(caddy->jdata->jobid), caddy->jdata);

    /* if job recovery is not defined, set it to default */
    if (!caddy->jdata->recovery_defined) {
        /* set to system default */
        caddy->jdata->enable_recovery = orte_enable_recovery;
    }

    /* get collective ids for the std MPI operations */
    caddy->jdata->peer_modex = orte_grpcomm_base_get_coll_id();
    modx_par = mca_base_param_environ_variable("orte", NULL, "peer_modex_id");
    asprintf(&modx_val, "%d", caddy->jdata->peer_modex);
    caddy->jdata->peer_init_barrier = orte_grpcomm_base_get_coll_id();
    bar1_par = mca_base_param_environ_variable("orte", NULL, "peer_init_barrier_id");
    asprintf(&bar1_val, "%d", caddy->jdata->peer_init_barrier);
    caddy->jdata->peer_fini_barrier = orte_grpcomm_base_get_coll_id();
    bar2_par = mca_base_param_environ_variable("orte", NULL, "peer_fini_barrier_id");
    asprintf(&bar2_val, "%d", caddy->jdata->peer_fini_barrier);

    /* if app recovery is not defined, set apps to defaults */
    for (i=0; i < caddy->jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(caddy->jdata->apps, i))) {
            continue;
        }
        if (!app->recovery_defined) {
            app->max_restarts = orte_max_restarts;
        }
        /* set the envars for the collective ids */
        opal_setenv(modx_par, modx_val, true, &app->env);
        opal_setenv(bar1_par, bar1_val, true, &app->env);
        opal_setenv(bar2_par, bar2_val, true, &app->env);
    }
    free(modx_par);
    free(modx_val);
    free(bar1_par);
    free(bar1_val);
    free(bar2_par);
    free(bar2_val);

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
#if OPAL_ENABLE_FT_CR == 1
    int rc;
#endif
    orte_job_t *jdata, *jdatorted;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* if we don't want to launch the apps, now is the time to leave */
    if (orte_do_not_launch) {
        orte_never_launched = true;
        ORTE_TERMINATE(0);
        OBJ_RELEASE(caddy);
        return;
    }

    /* bozo check */
    if (ORTE_JOB_STATE_SYSTEM_PREP != caddy->job_state) {
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* get the orted job data object */
    if (NULL == (jdatorted = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
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
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    orte_process_info.num_procs = jdatorted->num_procs;

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }

    /* ensure our routing plan is up-to-date */
    orte_routed.update_routing_plan();
    
    /*** RHC: USER REQUEST TO TIE-OFF STDXXX TO /DEV/NULL
     *** WILL BE SENT IN LAUNCH MESSAGE AS PART OF CONTROLS FIELD.
     *** SO IF USER WANTS NO IO BEING SENT AROUND, THE ORTEDS
     *** WILL TIE IT OFF AND THE IOF WILL NEVER RECEIVE ANYTHING.
     *** THE IOF AUTOMATICALLY KNOWS TO OUTPUT ANY STDXXX
     *** DATA IT -DOES- RECEIVE TO THE APPROPRIATE FD, SO THERE
     *** IS NOTHING WE NEED DO HERE TO SETUP IOF
     ***/
    
#if OPAL_ENABLE_FT_CR == 1
    /*
     * Notify the Global SnapC component regarding new job (even if it was restarted)
     */
    if( ORTE_SUCCESS != (rc = orte_snapc.setup_job(jdata->jobid) ) ) {
        /* Silent Failure :/ JJH */
        ORTE_ERROR_LOG(rc);
    }
#endif
    /* set the job state to the next position */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_LAUNCH_APPS);

    /* cleanup */
    OBJ_RELEASE(caddy);
}

/* catch timeout to allow cmds to progress */
static void timer_cb(int fd, short event, void *cbdata)
{
    orte_timer_t *tm = (orte_timer_t*)cbdata;
    orte_job_t *jdata = (orte_job_t*)tm->payload;

    if (NULL == jdata || jdata->state < ORTE_JOB_STATE_RUNNING) {
        /* declare launch failed */
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_FAILED_TO_START);
    }

    /* free event */
    OBJ_RELEASE(tm);
}

void orte_plm_base_launch_apps(int fd, short args, void *cbdata)
{
    orte_job_t *jdata;
    orte_daemon_cmd_flag_t command;
    opal_buffer_t *buffer;
    int rc;
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* convenience */
    jdata = caddy->jdata;

    if (ORTE_JOB_STATE_LAUNCH_APPS != caddy->job_state) {
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* get the local launcher's required data */
    if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(buffer, jdata->jobid))) {
        ORTE_ERROR_LOG(rc);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    
    /* send the command to the daemons */
    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid,
                                                 buffer, ORTE_RML_TAG_DAEMON))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* track that we automatically are considered to have reported - used
     * only to report out launch progress
     */
    caddy->jdata->num_daemons_reported++;

    /* setup a timer - if we don't launch within the
     * defined time, then we know things have failed
     */
    if (0 < orte_startup_timeout) {
        ORTE_DETECT_TIMEOUT(orte_startup_timeout, 1000, 10000000, timer_cb, jdata);
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

    /* convenience */
    jdata = caddy->jdata;

    if (ORTE_JOB_STATE_RUNNING != caddy->job_state) {
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* complete wiring up the iof */
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch wiring up iof for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* push stdin - the IOF will know what to do with the specified target */
    name.jobid = jdata->jobid;
    name.vpid = jdata->stdin_target;
    
    if (ORTE_SUCCESS != (rc = orte_iof.push(&name, ORTE_IOF_STDIN, 0))) {
        ORTE_ERROR_LOG(rc);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

    /* complete debugger interface */
    ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_READY_FOR_DEBUGGERS);

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

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch registered event",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (ORTE_JOB_STATE_REGISTERED != caddy->job_state) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:launch job %s not registered - state %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             orte_job_state_to_str(caddy->job_state)));
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* if this isn't a dynamic spawn, just cleanup */
    if (ORTE_JOBID_INVALID == jdata->originator.jobid) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &jdata->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:launch sending dyn release of job %s to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         ORTE_NAME_PRINT(&jdata->originator)));
    if (0 > (ret = orte_rml.send_buffer_nb(&jdata->originator, answer,
                                           ORTE_RML_TAG_PLM_PROXY, 0,
                                           orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(answer);
        ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        OBJ_RELEASE(caddy);
        return;
    }

 cleanup:
    /* RHC: need to init_after_spawn for debuggers */
    /* no state to activate - this ends the launch sequence */
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
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }

        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
        
        /* unpack the node name */
        idx = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &nodename, &idx, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            orted_failed_launch = true;
            goto CLEANUP;
        }
        
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:orted_report_launch from daemon %s on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&dname), nodename));
        
        /* look this node up, if necessary */
        if (!orte_plm_globals.daemon_nodes_assigned_at_launch) {
            if (!orte_have_fqdn_allocation) {
                /* remove any domain info */
                if (NULL != (ptr = strchr(nodename, '.'))) {
                    *ptr = '\0';
                    ptr = strdup(nodename);
                    free(nodename);
                    nodename = ptr;
                }
            }
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s plm:base:orted_report_launch attempting to assign daemon %s to node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&dname), nodename));
            for (idx=0; idx < orte_node_pool->size; idx++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, idx))) {
                    continue;
                }
                if (node->location_verified) {
                    /* already assigned */
                    continue;
                }
                if (0 == strcmp(nodename, node->name)) {
                    /* flag that we verified the location */
                    node->location_verified = true;
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
                    daemon->nodename = node->name;
                    OBJ_RETAIN(node);
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:orted_report_launch assigning daemon %s to node %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(&daemon->name), node->name));
                    break;
                }
            }
        }
        
#if OPAL_HAVE_HWLOC
        /* store the local resources for that node */
        if (1 == dname.vpid || orte_hetero_nodes) {
            hwloc_topology_t topo, t;
            int i;
            bool found;
            
            idx=1;
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
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &topo, &idx, OPAL_HWLOC_TOPO))) {
                ORTE_ERROR_LOG(rc);
                orted_failed_launch = true;
                goto CLEANUP;
            }
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                 "%s RECEIVED TOPOLOGY FROM NODE %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), nodename));
            if (10 < opal_output_get_verbosity(orte_plm_globals.output)) {
                opal_dss.dump(0, topo, OPAL_HWLOC_TOPO);
            }
            /* do we already have this topology from some other node? */
            found = false;
            for (i=0; i < orte_node_topologies->size; i++) {
                if (NULL == (t = (hwloc_topology_t)opal_pointer_array_get_item(orte_node_topologies, i))) {
                    continue;
                }
                if (OPAL_EQUAL == opal_dss.compare(topo, t, OPAL_HWLOC_TOPO)) {
                    /* yes - just point to it */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s TOPOLOGY MATCHES - DISCARDING",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    found = true;
                    node->topology = t;
                    hwloc_topology_destroy(topo);
                    break;
                }
            }
            if (!found) {
                /* nope - add it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s NEW TOPOLOGY - ADDING",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    
                opal_pointer_array_add(orte_node_topologies, topo);
                node->topology = topo;
            }
        }
#endif
        
    CLEANUP:
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
        idx = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        ORTE_ACTIVATE_JOB_STATE(jdatorted, ORTE_JOB_STATE_FAILED_TO_START);
    } else if (NULL != orte_tree_launch_cmd) {
        /* if a tree-launch is underway, send the cmd back */
        OBJ_RETAIN(orte_tree_launch_cmd);
        orte_rml.send_buffer_nb(sender, orte_tree_launch_cmd,
                                ORTE_RML_TAG_DAEMON, 0,
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
    orte_proc_t *daemon;

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
    int loc_id;
    char * amca_param_path = NULL;
    char * amca_param_prefix = NULL;
    char * tmp_force = NULL;
    int i, cnt, rc;
    orte_job_t *jdata;
    char *rml_uri;
    unsigned long num_procs;
    
    /* check for debug flags */
    if (orte_debug_flag) {
        opal_argv_append(argc, argv, "--debug");
    }
    if (orte_debug_daemons_flag) {
        opal_argv_append(argc, argv, "--debug-daemons");
    }
    if (orte_debug_daemons_file_flag) {
        opal_argv_append(argc, argv, "--debug-daemons-file");
    }
    if (orted_spin_flag) {
        opal_argv_append(argc, argv, "--spin");
    }
#if OPAL_HAVE_HWLOC
    if (opal_hwloc_report_bindings) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_report_bindings");
        opal_argv_append(argc, argv, "1");
    }
    if (orte_hetero_nodes) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_hetero_nodes");
        opal_argv_append(argc, argv, "1");
    }
#endif
    if (orte_map_reduce) {
        opal_argv_append(argc, argv, "--mapreduce");
    }
    if (orte_map_stddiag_to_stderr) {
        opal_argv_append(argc, argv, "-mca");
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
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "ess");
        opal_argv_append(argc, argv, ess);
    }
    
    /* pass the daemon jobid */
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_jobid");
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* setup to pass the vpid */
    if (NULL != proc_vpid_index) {
        opal_argv_append(argc, argv, "-mca");
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
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_ess_num_procs");
    asprintf(&param, "%lu", num_procs);
    opal_argv_append(argc, argv, param);
    free(param);
    
    /* pass the uri of the hnp */
    if (ORTE_PROC_IS_HNP) {
        rml_uri = orte_rml.get_contact_info();
    } else {
        asprintf(&param, "\"%s\"", orte_rml.get_contact_info() );
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_parent_uri");
        opal_argv_append(argc, argv, param);
        free(param);
    
        rml_uri = orte_process_info.my_hnp_uri;
    }
    asprintf(&param, "\"%s\"", rml_uri);
    opal_argv_append(argc, argv, "-mca");
    opal_argv_append(argc, argv, "orte_hnp_uri");
    opal_argv_append(argc, argv, param);
    free(param);

    /* if given and we have static ports or are using a common port, pass the node list */
    if ((orte_static_ports || orte_use_common_port) && NULL != nodes) {
        /* convert the nodes to a regex */
        if (ORTE_SUCCESS != (rc = orte_regex_create(nodes, &param))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_node_regex");
        opal_argv_append(argc, argv, param);
        free(param);
    }
    
    if (orte_use_common_port) {
        /* tell the daemon to use the common port */
        opal_argv_append(argc, argv, "--use-common-port");
    } else {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_use_common_port");
        opal_argv_append(argc, argv, "0");
    }

    /* warn the daemons if we are using a tree spawn pattern so they
     * know they shouldn't do a rollup on their callback
     */
    if (NULL != orte_tree_launch_cmd) {
        opal_argv_append(argc, argv, "--tree-spawn");
    }

    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes
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
            /* must be okay - pass it along */
            opal_argv_append(argc, argv, orted_cmd_line[i]);
            opal_argv_append(argc, argv, orted_cmd_line[i+1]);
            opal_argv_append(argc, argv, orted_cmd_line[i+2]);
        }
    }

    /* if output-filename was specified, pass that along */
    if (NULL != orte_output_filename) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_output_filename");
        opal_argv_append(argc, argv, orte_output_filename);
    }
    
    /* if --xterm was specified, pass that along */
    if (NULL != orte_xterm) {
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "orte_xterm");
        opal_argv_append(argc, argv, orte_xterm);
    }
    
    /* 
     * Pass along the Aggregate MCA Parameter Sets
     */
    /* Add the 'prefix' param */
    loc_id = mca_base_param_find("mca", NULL, "base_param_file_prefix");
    mca_base_param_lookup_string(loc_id, &amca_param_prefix);
    if( NULL != amca_param_prefix ) {
        /* Could also use the short version '-am'
         * but being verbose has some value
         */
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_prefix");
        opal_argv_append(argc, argv, amca_param_prefix);
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path");
        mca_base_param_lookup_string(loc_id, &amca_param_path);
        if( NULL != amca_param_path ) {
            opal_argv_append(argc, argv, "-mca");
            opal_argv_append(argc, argv, "mca_base_param_file_path");
            opal_argv_append(argc, argv, amca_param_path);
        }
    
        /* Add the 'path' param */
        loc_id = mca_base_param_find("mca", NULL, "base_param_file_path_force");
        mca_base_param_lookup_string(loc_id, &tmp_force);
        if( NULL == tmp_force ) {
            /* Get the current working directory */
            tmp_force = (char *) malloc(sizeof(char) * OPAL_PATH_MAX);
            if( NULL == (tmp_force = getcwd(tmp_force, OPAL_PATH_MAX) )) {
                tmp_force = strdup("");
            }
        }
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "mca_base_param_file_path_force");
        opal_argv_append(argc, argv, tmp_force);
    
        free(tmp_force);
    
        if( NULL != amca_param_path ) {
            free(amca_param_path);
            amca_param_path = NULL;
        }

        if( NULL != amca_param_prefix ) {
            free(amca_param_prefix);
            amca_param_prefix = NULL;
        }
    }

    if (NULL != orte_selected_oob_component) {
        /* ensure we all use the same OOB component */
        opal_argv_append(argc, argv, "-mca");
        opal_argv_append(argc, argv, "oob");
        opal_argv_append(argc, argv, orte_selected_oob_component);
    }

    return ORTE_SUCCESS;
}

int orte_plm_base_setup_virtual_machine(orte_job_t *jdata)
{
    orte_node_t *node;
    orte_proc_t *proc;
    orte_job_map_t *map=NULL;
    int rc, i;
    orte_job_t *daemons;
    opal_list_t nodes;
    opal_list_item_t *item, *next;
    orte_app_context_t *app;
    bool one_filter = false;
    int num_nodes;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:setup_vm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* if we are not working with a virtual machine, then we
     * look across all jobs and ensure that the "VM" contains
     * all nodes with application procs on them
     */
    if (ORTE_JOB_CONTROL_NO_VM & daemons->controls) {
        OBJ_CONSTRUCT(&nodes, opal_list_t);
        if (NULL == daemons->map) {
            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
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
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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
            ORTE_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
            return ORTE_ERR_FATAL;
        }
        goto process;
    }

    if (NULL == daemons->map) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
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

    /* construct a list of available nodes - don't need ours as
     * we already exist
     */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    for (i=1; i < orte_node_pool->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (ORTE_NODE_STATE_DO_NOT_USE == node->state) {
                /* reset the state so it can be used another time */
                node->state = ORTE_NODE_STATE_UP;
                continue;
            }
            if (ORTE_NODE_STATE_DOWN == node->state) {
                continue;
            }
            if (ORTE_NODE_STATE_NOT_INCLUDED == node->state) {
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
            node->mapped = false;
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * allocation - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == opal_list_get_size(&nodes)) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:setup_vm only HNP in allocation",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_DESTRUCT(&nodes);
        /* mark that the daemons have reported so we can proceed */
        daemons->state = ORTE_JOB_STATE_DAEMONS_REPORTED;
        return ORTE_SUCCESS;
    }

    /* is there a default hostfile? */
    if (NULL != orte_default_hostfile) {
        /* yes - filter the node list through the file, marking
         * any nodes not in the file -or- excluded via ^
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(&nodes, orte_default_hostfile, false)) &&
            ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (ORTE_SUCCESS == rc) {
            /* we filtered something */
            one_filter = true;
        }
    }
 
    /* filter across the union of all app_context specs */
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
            if (!node->mapped) {
                opal_list_remove_item(&nodes, item);
                OBJ_RELEASE(item);
            }
            item = next;
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * allocation - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == opal_list_get_size(&nodes)) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:setup_vm only HNP left",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        OBJ_DESTRUCT(&nodes);
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
        /* add the node to the map */
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
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:setup_vm add new daemon %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name)));
        /* add the daemon to the daemon job object */
        if (0 > (rc = opal_pointer_array_set_item(daemons->procs, proc->name.vpid, (void*)proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        ++daemons->num_procs;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:setup_vm assigning new daemon %s to node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name),
                             node->name));
        /* point the node to the daemon */
        node->daemon = proc;
        OBJ_RETAIN(proc);  /* maintain accounting */
        /* point the proc to the node and maintain accounting */
        proc->node = node;
        proc->nodename = node->name;
        OBJ_RETAIN(node);
        if (orte_plm_globals.daemon_nodes_assigned_at_launch) {
            node->location_verified = true;
        } else {
            node->location_verified = false;
        }
        /* track number of daemons to be launched */
        ++map->num_new_daemons;
        /* and their starting vpid */
        if (ORTE_VPID_INVALID == map->daemon_vpid_start) {
            map->daemon_vpid_start = proc->name.vpid;
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

    return ORTE_SUCCESS;
}
