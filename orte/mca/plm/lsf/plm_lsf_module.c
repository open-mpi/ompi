/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_lsf.h"


/*
 * Local functions
 */
static int plm_lsf_init(void);
static int plm_lsf_launch_job(orte_job_t *jdata);
static int plm_lsf_terminate_orteds(void);
static int plm_lsf_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_lsf_finalize(void);


/*
 * Global variable
 */
orte_plm_base_module_t orte_plm_lsf_module = {
    plm_lsf_init,
    orte_plm_base_set_hnp_name,
    plm_lsf_launch_job,
    NULL,
    orte_plm_base_orted_terminate_job,
    plm_lsf_terminate_orteds,
    orte_plm_base_orted_kill_local_procs,
    plm_lsf_signal_job,
    plm_lsf_finalize
};

/**
 * Init the module
 */
int plm_lsf_init(void)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }

    if (orte_do_not_launch) {
        /* must assign daemons as won't be launching them */
        orte_plm_globals.daemon_nodes_assigned_at_launch = true;
    } else {
        /* we do NOT assign daemons to nodes at launch - we will
         * determine that mapping when the daemon
         * calls back. This is required because lsf does
         * its own mapping of proc-to-node, and we cannot know
         * in advance which daemon will wind up on which node
         */
        orte_plm_globals.daemon_nodes_assigned_at_launch = false;
    }

    return rc;
}

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that orterun will be woken up and
 * the job can cleanly terminate
 */
static int plm_lsf_launch_job(orte_job_t *jdata)
{
    orte_job_map_t *map;
    size_t num_nodes;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char** env = NULL;
    char **nodelist_argv;
    char *nodelist;
    int nodelist_argc;
    char *vpid_string;
    int i;
    char *cur_prefix;
    struct timeval joblaunchstart, launchstart, launchstop;
    int proc_vpid_index = 0;
    bool failed_launch = true;
    orte_app_context_t *app;
    orte_node_t *node;
    orte_std_cntr_t nnode;
    orte_jobid_t failed_job;
    orte_job_state_t job_state = ORTE_JOB_STATE_NEVER_LAUNCHED;
    orte_job_t *daemons;

    /* default to declaring the daemons failed*/
    failed_job = ORTE_PROC_MY_NAME->jobid;

    if (orte_timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "plm_lsf: could not obtain job start time");
        }        
    }
    
    /* start by setting up the virtual machine */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_virtual_machine(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if we don't want to launch, then don't attempt to
     * launch the daemons - the user really wants to just
     * look at the proposed process map
     */
    if (orte_do_not_launch) {
        goto launch_apps;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                         "%s plm:slurm: launching vm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    
    /* Get the map for this job */
    if (NULL == (map = daemons->map)) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    num_nodes = map->num_new_daemons;
    if (num_nodes == 0) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:lsf: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto launch_apps;
    }

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (nnode=0; nnode < map->nodes->size; nnode++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, nnode))) {
            continue;
        }
        /* if the daemon already exists on this node, then
         * don't include it
         */
        if (node->daemon_launched) {
            continue;
        }
        
        /* otherwise, add it to the list of nodes upon which
         * we need to launch a daemon
         */
        opal_argv_append(&nodelist_argc, &nodelist_argv, node->name);
    }
    nodelist = opal_argv_join(nodelist_argv, ',');

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;
    
    /*
     * ORTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    orte_plm_base_setup_orted_cmd(&argc, &argv);

    /* Add basic orted command line options */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "lsf",
                                          &proc_vpid_index,
                                          nodelist);
    free(nodelist);

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    rc = orte_util_convert_vpid_to_string(&vpid_string, map->daemon_vpid_start);
    if (ORTE_SUCCESS != rc) {
        opal_output(0, "plm_lsf: unable to get daemon vpid as string");
        goto cleanup;
    }
    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(vpid_string);
    free(vpid_string);

    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "plm:lsf: final top-level argv:");
            opal_output(0, "plm:lsf:     %s", param);
            free(param);
        }
    }

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire lsf run -- we
       don't support different --prefix'es for different nodes in
       the LSF plm) */
    cur_prefix = NULL;
    for (i=0; i < jdata->apps->size; i++) {
        char *app_prefix_dir;
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, n))) {
            continue;
        }
        app_prefix_dir = app->prefix_dir;
        /* Check for already set cur_prefix_dir -- if different,
           complain */
        if (NULL != app_prefix_dir) {
            if (NULL != cur_prefix &&
                0 != strcmp (cur_prefix, app_prefix_dir)) {
                orte_show_help("help-plm-lsf.txt", "multiple-prefixes",
                               true, cur_prefix, app_prefix_dir);
                rc = ORTE_ERR_FAILED_TO_START;
                goto cleanup;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                                     "%s plm:lsf: Set prefix:%s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), cur_prefix));
            }
        }
    }

    /* setup environment */
    env = opal_argv_copy(orte_launch_environ);

    if (orte_timing) {
        if (0 != gettimeofday(&launchstart, NULL)) {
            opal_output(0, "plm_lsf: could not obtain start time");
        }        
    }
    
    /* set the job state to indicate we attempted to launch */
    job_state = ORTE_JOB_STATE_FAILED_TO_START;
    
    /* lsb_launch tampers with SIGCHLD.
     * After the call to lsb_launch, the signal handler for SIGCHLD is NULL.
     * So, we disable the SIGCHLD handler of libevent for the duration of 
     * the call to lsb_launch
     */
    orte_wait_disable();
    
    /* exec the daemon(s). Do NOT wait for lsb_launch to complete as
     * it only completes when the processes it starts - in this case,
     * the orteds - complete. We need to go ahead and return so
     * orterun can do the rest of its stuff. Instead, we'll catch any
     * failures and deal with them elsewhere
     */
    if (lsb_launch(nodelist_argv, argv, LSF_DJOB_REPLACE_ENV | LSF_DJOB_NOWAIT, env) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_FAILED_TO_START);
        opal_output(0, "lsb_launch failed: %d", rc);
        rc = ORTE_ERR_FAILED_TO_START;
        orte_wait_enable();  /* re-enable our SIGCHLD handler */
        goto cleanup;
    }
    orte_wait_enable();  /* re-enable our SIGCHLD handler */
    
    /* wait for daemons to callback */
    if (ORTE_SUCCESS != 
        (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:lsf: daemon launch failed on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

launch_apps:
    /* setup the job */
    if (ORTE_SUCCESS != (rc = orte_plm_base_setup_job(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    /* daemons succeeded - any failure now would be from apps */
    failed_job = jdata->jobid;
    if (ORTE_SUCCESS != (rc = orte_plm_base_launch_apps(jdata->jobid))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:lsf: launch of apps failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
    /* declare the launch a success */
    failed_launch = false;
    
    if (orte_timing) {
        if (0 != gettimeofday(&launchstop, NULL)) {
             opal_output(0, "plm_lsf: could not obtain stop time");
         } else {
             opal_output(0, "plm_lsf: daemon block launch time is %ld usec",
                         (launchstop.tv_sec - launchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - launchstart.tv_usec));
             opal_output(0, "plm_lsf: total job launch time is %ld usec",
                         (launchstop.tv_sec - joblaunchstart.tv_sec)*1000000 + 
                         (launchstop.tv_usec - joblaunchstart.tv_usec));
         }
    }

    if (ORTE_SUCCESS != rc) {
        opal_output(0, "plm:lsf: start_procs returned error %d", rc);
        goto cleanup;
    }

cleanup:
    if (NULL != argv) {
        opal_argv_free(argv);
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        orte_errmgr.update_state(failed_job, job_state,
                                 NULL, ORTE_PROC_STATE_UNDEF,
                                 0, ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    return rc;
}


/**
* Terminate the orteds for a given job
 */
static int plm_lsf_terminate_orteds(void)
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


/**
 * Signal all the processes in the job
 */
static int plm_lsf_signal_job(orte_jobid_t jobid, int32_t signal)
{
    int rc;
    
    /* order the orteds to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_orted_signal_local_procs(jobid, signal))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


static int plm_lsf_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_plm_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}
