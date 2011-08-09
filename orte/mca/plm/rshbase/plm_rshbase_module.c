/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/opal_sos.h"
#include "opal/mca/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/util/bit_ops.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/plm_base_rsh_support.h"
#include "orte/mca/plm/rshbase/plm_rshbase.h"

static int init(void);
static int spawn(orte_job_t *jdata);
static int terminate_orteds(void);
static int signal_job(orte_jobid_t jobid, int32_t signal);
static int finalize(void);

orte_plm_base_module_t orte_plm_rshbase_module = {
    init,
    orte_plm_base_set_hnp_name,
    spawn,
    NULL,
    orte_plm_base_orted_terminate_job,
    terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    signal_job,
    finalize
};

/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* local global storage */
static int num_in_progress=0;

/**
 * Init the module
 */
static int init(void)
{
    int rc;
    
    /* we were selected, so setup the launch agent */
    if (ORTE_SUCCESS != (rc = orte_plm_base_rsh_launch_agent_setup(NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
 * Callback on daemon exit.
 */

static void orte_plm_rsh_wait_daemon(pid_t pid, int status, void* cbdata)
{
    orte_std_cntr_t cnt=1;
    uint8_t flag;
    orte_job_t *jdata;
    
    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) { /* if abnormal exit */
        /* if we are not the HNP, send a message to the HNP alerting it
         * to the failure
         */
        if (!ORTE_PROC_IS_HNP) {
            opal_buffer_t buf;
            orte_vpid_t *vpid=(orte_vpid_t*)cbdata;
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)*vpid, WEXITSTATUS(status)));
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.pack(&buf, &cnt, 1, ORTE_STD_CNTR);
            flag = 1;
            opal_dss.pack(&buf, &flag, 1, OPAL_UINT8);
            opal_dss.pack(&buf, vpid, 1, ORTE_VPID);
            orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buf, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH, 0);
            OBJ_DESTRUCT(&buf);
        } else {
            orte_proc_t *daemon=(orte_proc_t*)cbdata;
            jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s daemon %d failed with status %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)daemon->name.vpid, WEXITSTATUS(status)));
            /* note that this daemon failed */
            daemon->state = ORTE_PROC_STATE_FAILED_TO_START;
            /* increment the #daemons terminated so we will exit properly */
            jdata->num_terminated++;
#if 0
            /* report that the daemon has failed so we can exit */
            orte_errmgr.update_state(ORTE_PROC_MY_NAME->jobid, ORTE_JOB_STATE_FAILED_TO_START,
                                     NULL, ORTE_PROC_STATE_UNDEF, status);
#else
            /* JJH: Look into a better way of doing this. If we let the daemon
             *      know, then it kills the job when we are trying to restart.. */
            opal_output(0, "%s daemon %s failed. SKIPPING orte_plm_base_launch_failed()",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&daemon->name));
#endif
        }
    }

    /* release any waiting threads */
    num_in_progress--;
    OPAL_THREAD_LOCK(&mca_plm_rshbase_component.lock);

    if (num_in_progress <= mca_plm_rshbase_component.num_concurrent) {
        opal_condition_signal(&mca_plm_rshbase_component.cond);
    }

    OPAL_THREAD_UNLOCK(&mca_plm_rshbase_component.lock);

}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int spawn(orte_job_t *jdata)
{
    int rc;
    orte_job_map_t *map;
    orte_app_context_t *app;
    orte_node_t *node;
    int nnode;
    int argc;
    char **argv=NULL, **nodes=NULL, *nodelist=NULL;
    char *prefix_dir;
    int node_name_index1;
    int proc_vpid_index;
    pid_t pid;
    bool failed_launch = true;
    orte_jobid_t active_job, failed_job;

    /* wait for the launch to complete */
    OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
    while (orte_plm_globals.spawn_in_progress) {
        opal_condition_wait(&orte_plm_globals.spawn_in_progress_cond, &orte_plm_globals.spawn_lock);
    }
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output, "released to spawn"));
    orte_plm_globals.spawn_in_progress = true;
    orte_plm_globals.spawn_status = ORTE_ERR_FATAL;
    OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
    
    if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
        /* if this is a request to launch a local slave,
         * then we will not be launching an orted - we will
         * directly ssh the slave process itself. No mapping
         * is performed to support this - the caller must
         * provide all the info required to launch the job,
         * including the target hosts
         */
        rc = orte_plm_base_local_slave_launch(jdata);
        OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
        orte_plm_globals.spawn_in_progress = false;
        OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
        return rc;
    }

    /* default to declaring the daemon launch as having failed */
    failed_job = ORTE_PROC_MY_NAME->jobid;
    
    /* if we are timing, record the start time */
    if (orte_timing) {
        gettimeofday(&orte_plm_globals.daemonlaunchstart, NULL);
        joblaunchstart = orte_plm_globals.daemonlaunchstart;
    }
    
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:rshbase: launching job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
    
    /* set the active jobid */
    active_job = jdata->jobid;

    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(jdata->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshbase: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }
    
    if ((0 < opal_output_get_verbosity(orte_plm_globals.output) ||
         orte_leave_session_attached) &&
        mca_plm_rshbase_component.num_concurrent < map->num_new_daemons) {
        /**
         * If we are in '--debug-daemons' we keep the ssh connection 
         * alive for the span of the run. If we use this option 
         * AND we launch on more than "num_concurrent" machines
         * then we will deadlock. No connections are terminated 
         * until the job is complete, no job is started
         * since all the orteds are waiting for all the others
         * to come online, and the others ore not launched because
         * we are waiting on those that have started to terminate
         * their ssh tunnels. :(
         * As we cannot run in this situation, pretty print the error
         * and return an error code.
         */
        orte_show_help("help-plm-rsh.txt", "deadlock-params",
                       true, mca_plm_rshbase_component.num_concurrent, map->num_new_daemons);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }
    
    /*
     * After a discussion between Ralph & Jeff, we concluded that we
     * really are handling the prefix dir option incorrectly. It currently
     * is associated with an app_context, yet it really refers to the
     * location where OpenRTE/Open MPI is installed on a NODE. Fixing
     * this right now would involve significant change to orterun as well
     * as elsewhere, so we will intentionally leave this incorrect at this
     * point. The error, however, is identical to that seen in all prior
     * releases of OpenRTE/Open MPI, so our behavior is no worse than before.
     *
     * A note to fix this, along with ideas on how to do so, has been filed
     * on the project's Trac system under "feature enhancement".
     *
     * For now, default to the prefix_dir provided in the first app_context.
     * Since there always MUST be at least one app_context, we are safe in
     * doing this.
     */
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0);
    /* we also need at least one node name so we can check what shell is
     * being used, if we have to
     */
    node = NULL;
    for (nnode = 0; nnode < map->nodes->size; nnode++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
            break;
        }
    }
    if (NULL == node) {
        /* well, if there isn't even one node in the map, then we are hammered */
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }
    prefix_dir = app->prefix_dir;
    
    /* if we are using static ports, then setup a string showing the
     * nodes so we can use a regex to pass connection info
     */
    if (orte_static_ports) {
        for (nnode=0; nnode < map->nodes->size; nnode++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
                continue;
            }
            opal_argv_append_nosize(&nodes, node->name);
        }
        nodelist = opal_argv_join(nodes, ',');
        opal_argv_free(nodes);
    }

    /* setup the launch */
    if (ORTE_SUCCESS != (rc = orte_plm_base_rsh_setup_launch(&argc, &argv, node->name, &node_name_index1,
                                                             &proc_vpid_index, prefix_dir, nodelist))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
        if (NULL != nodelist) {
            free(nodelist);
            nodelist = NULL;
        }
    }
    if (NULL != nodelist) {
        free(nodelist);
        nodelist = NULL;
    }
    
    /* set the active jobid */
    active_job = jdata->jobid;

    /*
     * Iterate through each of the nodes
     */
    for (nnode=0; nnode < map->nodes->size; nnode++) {
        
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
            continue;
        }
        
        /* if this daemon already exists, don't launch it! */
        if (node->daemon_launched) {
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rshbase:launch daemon already exists on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            continue;
        }
        
        /* if the node's daemon has not been defined, then we
         * have an error!
         */
        if (NULL == node->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rshbase:launch daemon failed to be defined on node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            rc = ORTE_ERR_FATAL;
            goto cleanup;
        }
        
        /* setup node name */
        free(argv[node_name_index1]);
        if (NULL != node->username &&
            0 != strlen (node->username)) {
            asprintf (&argv[node_name_index1], "%s@%s",
                      node->username, node->name);
        } else {
            argv[node_name_index1] = strdup(node->name);
        }

        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshbase: launching on node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             node->name));

        /* fork a child to exec the rsh/ssh session */
        pid = fork();
        if (pid < 0) {
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            rc = ORTE_ERR_SYS_LIMITS_CHILDREN;
            goto cleanup;
        }

        /* child */
        if (pid == 0) {
            
            /* do the ssh launch - this will exit if it fails */
            orte_plm_base_ssh_child(argc, argv, node->daemon->name.vpid, proc_vpid_index);
            
            
        } else { /* father */
            /* indicate this daemon has been launched */
            node->daemon->state = ORTE_PROC_STATE_LAUNCHED;
            /* record the pid */
            node->daemon->pid = pid;
            
            OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                 "%s plm:rshbase: recording launch of daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name)));

            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_plm_rsh_wait_daemon, (void*)node->daemon);

            OPAL_THREAD_LOCK(&mca_plm_rshbase_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (num_in_progress++ >= mca_plm_rshbase_component.num_concurrent) {
                opal_condition_wait(&mca_plm_rshbase_component.cond, &mca_plm_rshbase_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_plm_rshbase_component.lock);
        }
    }

    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshbase: daemon launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    

 launch_apps:
    /* if we get here, then the daemons succeeded, so any failure would now be
     * for the application job
     */
    failed_job = active_job;
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(active_job))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rshbase: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(active_job), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

    /* wait for the launch to complete */
    OPAL_THREAD_LOCK(&orte_plm_globals.spawn_lock);
    while (!orte_plm_globals.spawn_complete) {
        opal_condition_wait(&orte_plm_globals.spawn_cond, &orte_plm_globals.spawn_lock);
    }
    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "completed spawn for job %s", ORTE_JOBID_PRINT(jdata->jobid)));
    orte_plm_globals.spawn_in_progress = false;
    opal_condition_broadcast(&orte_plm_globals.spawn_in_progress_cond);
    OPAL_THREAD_UNLOCK(&orte_plm_globals.spawn_lock);
    
    /* get here if launch went okay */
    failed_launch = false;
    
    if (orte_timing ) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "plm:rshbase: could not obtain job launch stop time");
        } else {
            opal_output(0, "plm:rshbase: total job launch time is %ld usec",
                        (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                        (joblaunchstop.tv_usec - joblaunchstart.tv_usec));
        }
    }

 cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_errmgr.update_state(failed_job, ORTE_JOB_STATE_FAILED_TO_START,
                                 NULL, ORTE_PROC_STATE_UNDEF,
                                 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
    }
    return rc;
}

/**
 * Terminate the orteds for a given job
 */
static int terminate_orteds(void)
{
    int rc;
    
    /* now tell them to die */
    if (orte_abnormal_term_ordered) {
        /* cannot know if a daemon is able to
         * tell us it died, so just ensure they
         * all terminate
         */
        if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_HALT_VM_CMD))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        /* we need them to "phone home", though,
         * so we can know that they have exited
         */
        if (ORTE_SUCCESS != (rc = orte_plm_base_orted_exit(ORTE_DAEMON_EXIT_CMD))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return rc;
}

static int signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

static int finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}
