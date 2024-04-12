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
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <signal.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/rmaps.h"
#include "src/mca/schizo/schizo.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_show_help.h"

#include "plm_lsf.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"

/*
 * Local functions
 */
static int plm_lsf_init(void);
static int plm_lsf_launch_job(prte_job_t *jdata);
static int plm_lsf_terminate_orteds(void);
static int plm_lsf_signal_job(pmix_nspace_t jobid, int32_t signal);
static int plm_lsf_finalize(void);

/*
 * Global variable
 */
prte_plm_base_module_t prte_plm_lsf_module = {
    .init = plm_lsf_init,
    .set_hnp_name = prte_plm_base_set_hnp_name,
    .spawn = plm_lsf_launch_job,
    .terminate_job = prte_plm_base_prted_terminate_job,
    .terminate_orteds = plm_lsf_terminate_orteds,
    .terminate_procs = prte_plm_base_prted_kill_local_procs,
    .signal_job = plm_lsf_signal_job,
    .finalize = plm_lsf_finalize
};

static void launch_daemons(int fd, short args, void *cbdata);

/**
 * Init the module
 */
int plm_lsf_init(void)
{
    int rc;
    prte_job_t *daemons;

    if (PRTE_SUCCESS != (rc = prte_plm_base_comm_start())) {
        PRTE_ERROR_LOG(rc);
    }

    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (prte_get_attribute(&daemons->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        /* must assign daemons as won't be launching them */
        prte_plm_globals.daemon_nodes_assigned_at_launch = true;
    } else {
        /* we do NOT assign daemons to nodes at launch - we will
         * determine that mapping when the daemon
         * calls back. This is required because lsf does
         * its own mapping of proc-to-node, and we cannot know
         * in advance which daemon will wind up on which node
         */
        prte_plm_globals.daemon_nodes_assigned_at_launch = false;
    }

    /* point to our launch command */
    if (PRTE_SUCCESS
        != (rc = prte_state.add_job_state(PRTE_JOB_STATE_LAUNCH_DAEMONS, launch_daemons))) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    return rc;
}

/* When working in this function, ALWAYS jump to "cleanup" if
 * you encounter an error so that prun will be woken up and
 * the job can cleanly terminate
 */
static int plm_lsf_launch_job(prte_job_t *jdata)
{
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        /* this is a restart situation - skip to the mapping stage */
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_MAP);
    } else {
        /* new job - set it up */
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_INIT);
    }
    return PRTE_SUCCESS;
}

static void launch_daemons(int fd, short args, void *cbdata)
{
    prte_job_map_t *map;
    size_t num_nodes;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char **env = NULL;
    char **nodelist_argv;
    int nodelist_argc;
    char *vpid_string;
    int i;
    char *cur_prefix;
    int proc_vpid_index = 0;
    bool failed_launch = true;
    prte_app_context_t *app;
    prte_node_t *node;
    int32_t nnode;
    prte_job_t *daemons;
    prte_state_caddy_t *state = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;

    PMIX_ACQUIRE_OBJECT(state);
    jdata = state->jdata;

    /* start by setting up the virtual machine */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (PRTE_SUCCESS != (rc = prte_plm_base_setup_virtual_machine(jdata))) {
        PRTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* if we don't want to launch, then don't attempt to
     * launch the daemons - the user really wants to just
     * look at the proposed process map
     */
    if (prte_get_attribute(&daemons->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        /* set the state to indicate the daemons reported - this
         * will trigger the daemons_reported event and cause the
         * job to move to the following step
         */
        state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        PMIX_RELEASE(state);
        return;
    }

    PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output, "%s plm:lsf: launching vm",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* Get the map for this job */
    if (NULL == (map = daemons->map)) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        rc = PRTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    num_nodes = map->num_new_daemons;
    if (0 == num_nodes) {
        /* set the state to indicate the daemons reported - this
         * will trigger the daemons_reported event and cause the
         * job to move to the following step
         */
        PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                             "%s plm:lsf: no new daemons to launch",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        PMIX_RELEASE(state);
        return;
    }

    /* create nodelist */
    nodelist_argv = NULL;
    nodelist_argc = 0;

    for (nnode = 0; nnode < map->nodes->size; nnode++) {
        if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, nnode))) {
            continue;
        }
        /* if the daemon already exists on this node, then
         * don't include it
         */
        if (PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_DAEMON_LAUNCHED)) {
            continue;
        }

        /* otherwise, add it to the list of nodes upon which
         * we need to launch a daemon
         */
        pmix_argv_append(&nodelist_argc, &nodelist_argv, node->name);
    }

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /*
     * PRTED OPTIONS
     */

    /* protect against launchers that forward the entire environment */
    if (NULL != getenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL")) {
        unsetenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL");
    }
    if (NULL != getenv("PMIX_LAUNCHER_RENDEZVOUS_FILE")) {
        unsetenv("PMIX_LAUNCHER_RENDEZVOUS_FILE");
    }

    /* add the daemon command (as specified by user) */
    prte_plm_base_setup_prted_cmd(&argc, &argv);

    /* Add basic orted command line options */
    prte_plm_base_prted_append_basic_args(&argc, &argv, "lsf", &proc_vpid_index);

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    rc = prte_util_convert_vpid_to_string(&vpid_string, map->daemon_vpid_start);
    if (PRTE_SUCCESS != rc) {
        pmix_output(0, "plm_lsf: unable to get daemon vpid as string");
        goto cleanup;
    }
    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(vpid_string);
    free(vpid_string);

    /* protect the args in case someone has a script wrapper */
    prte_plm_base_wrap_args(argv);

    if (0 < pmix_output_get_verbosity(prte_plm_base_framework.framework_output)) {
        param = PMIX_ARGV_JOIN_COMPAT(argv, ' ');
        if (NULL != param) {
            pmix_output(0, "plm:lsf: final top-level argv:");
            pmix_output(0, "plm:lsf:     %s", param);
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
    for (i = 0; i < jdata->apps->size; i++) {
        char *app_prefix_dir = NULL;
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (prte_get_attribute(&app->attributes, PRTE_APP_PREFIX_DIR, (void **) &app_prefix_dir,
                               PMIX_STRING)
            && NULL != app_prefix_dir) {
            /* Check for already set cur_prefix_dir -- if different,
               complain */
            if (NULL != cur_prefix && 0 != strcmp(cur_prefix, app_prefix_dir)) {
                pmix_show_help("help-plm-lsf.txt", "multiple-prefixes", true, cur_prefix,
                               app_prefix_dir);
                rc = PRTE_ERR_FAILED_TO_START;
                goto cleanup;
            }

            /* If not yet set, copy it; iff set, then it's the
               same anyway */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                                     "%s plm:lsf: Set prefix:%s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), cur_prefix));
            }
            free(app_prefix_dir);
        }
    }
    if (NULL == cur_prefix) {
        // see if it is in the environment
        if (NULL != (param = getenv("PRTE_PREFIX"))) {
            cur_prefix = strdup(param);
        }
    }

    /* setup environment */
    env = PMIX_ARGV_COPY_COMPAT(prte_launch_environ);

    /* lsb_launch tampers with SIGCHLD.
     * After the call to lsb_launch, the signal handler for SIGCHLD is NULL.
     * So, we disable the SIGCHLD handler of libevent for the duration of
     * the call to lsb_launch
     */
    prte_wait_disable();

    /* exec the daemon(s). Do NOT wait for lsb_launch to complete as
     * it only completes when the processes it starts - in this case,
     * the orteds - complete. We need to go ahead and return so
     * prun can do the rest of its stuff. Instead, we'll catch any
     * failures and deal with them elsewhere
     */
    if ((rc = lsb_launch(nodelist_argv, argv, LSF_DJOB_REPLACE_ENV | LSF_DJOB_NOWAIT, env)) < 0) {
        PRTE_ERROR_LOG(PRTE_ERR_FAILED_TO_START);
        char *flattened_nodelist = NULL;
        flattened_nodelist = PMIX_ARGV_JOIN_COMPAT(nodelist_argv, '\n');
        pmix_show_help("help-plm-lsf.txt", "lsb_launch-failed", true, rc, lsberrno, lsb_sysmsg(),
                       PMIX_ARGV_COUNT_COMPAT(nodelist_argv), flattened_nodelist);
        free(flattened_nodelist);
        rc = PRTE_ERR_FAILED_TO_START;
        prte_wait_enable(); /* re-enable our SIGCHLD handler */
        goto cleanup;
    }
    prte_wait_enable(); /* re-enable our SIGCHLD handler */

    /* indicate that the daemons for this job were launched */
    state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
    daemons->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;

    /* flag that launch was successful, so far as we currently know */
    failed_launch = false;

cleanup:
    if (NULL != argv) {
        PMIX_ARGV_FREE_COMPAT(argv);
    }
    if (NULL != env) {
        PMIX_ARGV_FREE_COMPAT(env);
    }

    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_FAILED_TO_START);
    }

    /* cleanup the caddy */
    PMIX_RELEASE(state);
}

/**
 * Terminate the orteds for a given job
 */
static int plm_lsf_terminate_orteds(void)
{
    int rc;

    if (PRTE_SUCCESS != (rc = prte_plm_base_prted_exit(PRTE_DAEMON_EXIT_CMD))) {
        PRTE_ERROR_LOG(rc);
    }

    return rc;
}

/**
 * Signal all the processes in the job
 */
static int plm_lsf_signal_job(pmix_nspace_t jobid, int32_t signal)
{
    int rc;

    /* order the orteds to pass this signal to their local procs */
    if (PRTE_SUCCESS != (rc = prte_plm_base_prted_signal_local_procs(jobid, signal))) {
        PRTE_ERROR_LOG(rc);
    }
    return rc;
}

static int plm_lsf_finalize(void)
{
    int rc;

    /* cleanup any pending recvs */
    if (PRTE_SUCCESS != (rc = prte_plm_base_comm_stop())) {
        PRTE_ERROR_LOG(rc);
    }

    return PRTE_SUCCESS;
}
