/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
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
#include "src/runtime/prte_globals.h"

#include <string.h>
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

#include "src/mca/base/pmix_base.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"

#include "constants.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/schizo/schizo.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/prted/prted.h"

#include "plm_slurm.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"

/*
 * Local functions
 */
static int plm_slurm_init(void);
static int plm_slurm_launch_job(prte_job_t *jdata);
static int plm_slurm_terminate_prteds(void);
static int plm_slurm_signal_job(pmix_nspace_t jobid, int32_t signal);
static int plm_slurm_finalize(void);

static int plm_slurm_start_proc(int argc, char **argv, char *prefix);

/*
 * Global variable
 */
prte_plm_base_module_1_0_0_t prte_plm_slurm_module = {
    .init = plm_slurm_init,
    .set_hnp_name = prte_plm_base_set_hnp_name,
    .spawn = plm_slurm_launch_job,
    .terminate_job = prte_plm_base_prted_terminate_job,
    .terminate_orteds = plm_slurm_terminate_prteds,
    .terminate_procs = prte_plm_base_prted_kill_local_procs,
    .signal_job = plm_slurm_signal_job,
    .finalize = plm_slurm_finalize
};

/*
 * Local variables
 */
static pid_t primary_srun_pid = 0;
static bool primary_pid_set = false;
static void launch_daemons(int fd, short args, void *cbdata);

/**
 * Init the module
 */
static int plm_slurm_init(void)
{
    int rc;
    prte_job_t *jdata;

    if (PRTE_SUCCESS != (rc = prte_plm_base_comm_start())) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    /* if we don't want to launch (e.g., someone just wants
     * to test the mappers), then we assign vpids at "launch"
     * so the mapper has something to work with
     */
    jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        prte_plm_globals.daemon_nodes_assigned_at_launch = true;
    } else {
        /* we do NOT assign daemons to nodes at launch - we will
         * determine that mapping when the daemon
         * calls back. This is required because slurm does
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
static int plm_slurm_launch_job(prte_job_t *jdata)
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
    prte_app_context_t *app;
    prte_node_t *node;
    int32_t n;
    prte_job_map_t *map;
    char *param;
    char **argv = NULL;
    int argc;
    int rc;
    char *tmp;
    char *nodelist_flat;
    char **nodelist_argv;
    char *name_string;
    char **custom_strings;
    int num_args, i;
    char *cur_prefix = NULL;
    int proc_vpid_index;
    bool failed_launch = true;
    prte_job_t *daemons;
    prte_state_caddy_t *state = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(state);

    PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                         "%s plm:slurm: LAUNCH DAEMONS CALLED",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* start by setting up the virtual machine */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (PRTE_SUCCESS != (rc = prte_plm_base_setup_virtual_machine(state->jdata))) {
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

    /* Get the map for this job */
    if (NULL == (map = daemons->map)) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        rc = PRTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    if (0 == map->num_new_daemons) {
        /* set the state to indicate the daemons reported - this
         * will trigger the daemons_reported event and cause the
         * job to move to the following step
         */
        PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                             "%s plm:slurm: no new daemons to launch",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        PMIX_RELEASE(state);
        return;
    }

    /*
     * start building argv array
     */
    argv = NULL;
    argc = 0;

    /*
     * SLURM srun OPTIONS
     */

    /* add the srun command */
    pmix_argv_append(&argc, &argv, "srun");

    /* start one orted on each node */
    pmix_argv_append(&argc, &argv, "--ntasks-per-node=1");

    if (!prte_get_attribute(&state->jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL) &&
        !prte_get_attribute(&state->jdata->attributes, PRTE_JOB_CONTINUOUS, NULL, PMIX_BOOL)) {
        /* kill the job if any prteds die */
        pmix_argv_append(&argc, &argv, "--kill-on-bad-exit");
    }

    /* our daemons are not an MPI task */
    pmix_argv_append(&argc, &argv, "--mpi=none");

    /* ensure the orteds are not bound to a single processor,
     * just in case the TaskAffinity option is set by default.
     * This will *not* release the orteds from any cpu-set
     * constraint, but will ensure it doesn't get
     * bound to only one processor
     */
    pmix_argv_append(&argc, &argv, "--cpu-bind=none");

    /* protect against launchers that forward the entire environment */
    if (NULL != getenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL")) {
        unsetenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL");
    }
    if (NULL != getenv("PMIX_LAUNCHER_RENDEZVOUS_FILE")) {
        unsetenv("PMIX_LAUNCHER_RENDEZVOUS_FILE");
    }

#if SLURM_CRAY_ENV
    /*
     * If in a SLURM/Cray env. make sure that Cray PMI is not pulled in,
     * neither as a constructor run when orteds start, nor selected
     * when pmix components are registered
     */

    setenv("PMI_NO_PREINITIALIZE", "1", false);
    setenv("PMI_NO_FORK", "1", false);
    setenv("OMPI_NO_USE_CRAY_PMI", "1", false);
#endif

    /* Append user defined arguments to srun */
    if (NULL != prte_mca_plm_slurm_component.custom_args) {
        custom_strings = PMIX_ARGV_SPLIT_COMPAT(prte_mca_plm_slurm_component.custom_args, ' ');
        num_args = PMIX_ARGV_COUNT_COMPAT(custom_strings);
        for (i = 0; i < num_args; ++i) {
            pmix_argv_append(&argc, &argv, custom_strings[i]);
        }
        PMIX_ARGV_FREE_COMPAT(custom_strings);
    }

    /* create nodelist */
    nodelist_argv = NULL;

    for (n = 0; n < map->nodes->size; n++) {
        if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, n))) {
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
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&nodelist_argv, node->name);
    }
    if (0 == PMIX_ARGV_COUNT_COMPAT(nodelist_argv)) {
        pmix_show_help("help-plm-slurm.txt", "no-hosts-in-list", true);
        rc = PRTE_ERR_FAILED_TO_START;
        goto cleanup;
    }
    nodelist_flat = PMIX_ARGV_JOIN_COMPAT(nodelist_argv, ',');
    PMIX_ARGV_FREE_COMPAT(nodelist_argv);

    /* if we are using all allocated nodes, then srun doesn't
     * require any further arguments
     */
    if (map->num_new_daemons < prte_num_allocated_nodes) {
        pmix_asprintf(&tmp, "--nodes=%lu", (unsigned long) map->num_new_daemons);
        pmix_argv_append(&argc, &argv, tmp);
        free(tmp);

        pmix_asprintf(&tmp, "--nodelist=%s", nodelist_flat);
        pmix_argv_append(&argc, &argv, tmp);
        free(tmp);
    }

    /* tell srun how many tasks to run */
    pmix_asprintf(&tmp, "--ntasks=%lu", (unsigned long) map->num_new_daemons);
    pmix_argv_append(&argc, &argv, tmp);
    free(tmp);

    PMIX_OUTPUT_VERBOSE((2, prte_plm_base_framework.framework_output,
                         "%s plm:slurm: launching on nodes %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         nodelist_flat));
    free(nodelist_flat);

    /*
     * PRTED OPTIONS
     */

    /* add the daemon command (as specified by user) */
    prte_plm_base_setup_prted_cmd(&argc, &argv);

    /* Add basic orted command line options, including debug flags */
    prte_plm_base_prted_append_basic_args(&argc, &argv, "slurm", &proc_vpid_index);

    /* tell the new daemons the base of the name list so they can compute
     * their own name on the other end
     */
    rc = prte_util_convert_vpid_to_string(&name_string, map->daemon_vpid_start);
    if (PRTE_SUCCESS != rc) {
        pmix_output(0, "plm_slurm: unable to get daemon vpid as string");
        goto cleanup;
    }

    free(argv[proc_vpid_index]);
    argv[proc_vpid_index] = strdup(name_string);
    free(name_string);

    /* Copy the prefix-directory specified in the
       corresponding app_context.  If there are multiple,
       different prefix's in the app context, complain (i.e., only
       allow one --prefix option for the entire slurm run -- we
       don't support different --prefix'es for different nodes in
       the SLURM plm) */
    cur_prefix = NULL;
    for (n = 0; n < state->jdata->apps->size; n++) {
        char *app_prefix_dir;
        app = (prte_app_context_t *) pmix_pointer_array_get_item(state->jdata->apps, n);
        if (NULL == app) {
            continue;
        }
        app_prefix_dir = NULL;
        prte_get_attribute(&app->attributes, PRTE_APP_PREFIX_DIR, (void **) &app_prefix_dir, PMIX_STRING);
        /* Check for already set cur_prefix_dir -- if different,
           complain */
        if (NULL != app_prefix_dir) {
            if (NULL != cur_prefix && 0 != strcmp(cur_prefix, app_prefix_dir)) {
                pmix_show_help("help-plm-slurm.txt", "multiple-prefixes", true, cur_prefix,
                               app_prefix_dir);
                goto cleanup;
            }

            /* If not yet set, copy it; iff set, then it's the
             * same anyway
             */
            if (NULL == cur_prefix) {
                cur_prefix = strdup(app_prefix_dir);
                PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                                     "%s plm:slurm: Set prefix:%s",
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

    /* protect the args in case someone has a script wrapper around srun */
    prte_plm_base_wrap_args(argv);

    if (0 < pmix_output_get_verbosity(prte_plm_base_framework.framework_output)) {
        param = PMIX_ARGV_JOIN_COMPAT(argv, ' ');
        pmix_output(prte_plm_base_framework.framework_output,
                    "%s plm:slurm: final top-level argv:\n\t%s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                    (NULL == param) ? "NULL" : param);
        if (NULL != param)
            free(param);
    }

    /* exec the daemon(s) */
    if (PRTE_SUCCESS != (rc = plm_slurm_start_proc(argc, argv, cur_prefix))) {
        PRTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* indicate that the daemons for this job were launched */
    state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
    daemons->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;

    /* flag that launch was successful, so far as we currently know */
    failed_launch = false;

cleanup:
    if (NULL != argv) {
        PMIX_ARGV_FREE_COMPAT(argv);
    }
    if (NULL != cur_prefix) {
        free(cur_prefix);
    }
    /* check for failed launch - if so, force terminate */
    if (failed_launch) {
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
    }

    /* cleanup the caddy */
    PMIX_RELEASE(state);
}

/**
 * Terminate the orteds for a given job
 */
static int plm_slurm_terminate_prteds(void)
{
    int rc = PRTE_SUCCESS;
    prte_job_t *jdata;

    /* check to see if the primary pid is set. If not, this indicates
     * that we never launched any additional daemons, so we cannot
     * not wait for a waitpid to fire and tell us it's okay to
     * exit. Instead, we simply trigger an exit for ourselves
     */
    if (primary_pid_set) {
        if (PRTE_SUCCESS != (rc = prte_plm_base_prted_exit(PRTE_DAEMON_EXIT_CMD))) {
            PRTE_ERROR_LOG(rc);
        }
    } else {
        PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                             "%s plm:slurm: primary daemons complete!",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        /* need to set the #terminated value to avoid an incorrect error msg */
        jdata->num_terminated = jdata->num_procs;
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
    }

    return rc;
}

/**
 * Signal all the processes in the child srun by sending the signal directly to it
 */
static int plm_slurm_signal_job(pmix_nspace_t jobid, int32_t signal)
{
    int rc = PRTE_SUCCESS;

    /* order them to pass this signal to their local procs */
    if (PRTE_SUCCESS != (rc = prte_plm_base_prted_signal_local_procs(jobid, signal))) {
        PRTE_ERROR_LOG(rc);
    }

    return rc;
}

static int plm_slurm_finalize(void)
{
    int rc;

    /* cleanup any pending recvs */
    if (PRTE_SUCCESS != (rc = prte_plm_base_comm_stop())) {
        PRTE_ERROR_LOG(rc);
    }

    return PRTE_SUCCESS;
}

static void srun_wait_cb(int sd, short fd, void *cbdata)
{
    prte_wait_tracker_t *t2 = (prte_wait_tracker_t *) cbdata;
    prte_proc_t *proc = t2->child;
    prte_job_t *jdata;
    FILE *fp;
    char version[1024], *cptr;
    int major=0, minor=0;
    PRTE_HIDE_UNUSED_PARAMS(sd, fd);

    jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);

    /* need to check that we are at least version 17.11 */
    fp = popen("sinfo -V", "r");
    if (NULL == fp) {
        /* default to printing the error advice */
        pmix_show_help("help-plm-slurm.txt", "ancient-version", true, major, minor);
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        PMIX_RELEASE(t2);
        return;
    }
    memset(version, 0, sizeof(version));
    while (NULL != fgets(version, sizeof(version), fp)) {
        /* if the line doesn't start with "slurm", then ignore it */
        if (0 != strncasecmp(version, "slurm", strlen("slurm"))) {
            continue;
        }
        cptr = &version[strlen("slurm")+1];
        major = strtoul(cptr, &cptr, 10);
        ++cptr;
        minor = strtoul(cptr, NULL, 10);
        if (major < 17) {
            pclose(fp);
            pmix_show_help("help-plm-slurm.txt", "ancient-version", true, major, minor);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            PMIX_RELEASE(t2);
            return;
        }
        if (17 == major && minor < 11) {
            pclose(fp);
            pmix_show_help("help-plm-slurm.txt", "ancient-version", true, major, minor);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            PMIX_RELEASE(t2);
            return;
        }
        /* version was not the issue */
        break;
    }
    pclose(fp);

    /* According to the SLURM folks, srun always returns the highest exit
     code of our remote processes. Thus, a non-zero exit status doesn't
     necessarily mean that srun failed - it could be that an orted returned
     a non-zero exit status. Of course, that means the orted failed(!), so
     the end result is the same - the job didn't start.

     As a result, we really can't do much with the exit status itself - it
     could be something in errno (if srun itself failed), or it could be
     something returned by an orted, or it could be something returned by
     the OS (e.g., couldn't find the orted binary). Somebody is welcome
     to sort out all the options and pretty-print a better error message. For
     now, though, the only thing that really matters is that
     srun failed. Report the error and make sure that prun
     wakes up - otherwise, do nothing!

     Unfortunately, the pid returned here is the srun pid, not the pid of
     the proc that actually died! So, to avoid confusion, just use -1 as the
     pid so nobody thinks this is real
     */


    /* abort only if the status returned is non-zero - i.e., if
     * the orteds exited with an error
     */
    if (0 != proc->exit_code) {
        /* an orted must have died unexpectedly - report
         * that the daemon has failed so we exit
         */
        PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                             "%s plm:slurm: srun returned non-zero exit status (%d) from launching "
                             "the per-node daemon",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), proc->exit_code));
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
    } else {
        /* otherwise, check to see if this is the primary pid */
        if (primary_srun_pid == proc->pid) {
            /* in this case, we just want to fire the proper trigger so
             * mpirun can exit
             */
            PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                                 "%s plm:slurm: primary daemons complete!",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            /* need to set the #terminated value to avoid an incorrect error msg */
            jdata->num_terminated = jdata->num_procs;
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        }
    }

    /* done with this dummy */
    PMIX_RELEASE(t2);
}

static int plm_slurm_start_proc(int argc, char **argv, char *prefix)
{
    int fd;
    int srun_pid;
    int n;
    char **tmp = NULL, *p;
    char *exec_argv = pmix_path_findv(argv[0], 0, environ, NULL);
    prte_proc_t *dummy;
    PRTE_HIDE_UNUSED_PARAMS(argc);

    if (NULL == exec_argv) {
        pmix_show_help("help-plm-slurm.txt", "no-srun", true);
        return PRTE_ERR_SILENT;
    }

    srun_pid = fork();
    if (-1 == srun_pid) {
        PRTE_ERROR_LOG(PRTE_ERR_SYS_LIMITS_CHILDREN);
        free(exec_argv);
        return PRTE_ERR_SYS_LIMITS_CHILDREN;
    }
    /* if this is the primary launch - i.e., not a comm_spawn of a
     * child job - then save the pid
     */
    if (0 < srun_pid && !primary_pid_set) {
        primary_srun_pid = srun_pid;
        primary_pid_set = true;
    }

    /* setup a dummy proc object to track the srun */
    dummy = PMIX_NEW(prte_proc_t);
    dummy->pid = srun_pid;
    /* be sure to mark it as alive so we don't instantly fire */
    PRTE_FLAG_SET(dummy, PRTE_PROC_FLAG_ALIVE);
    /* setup the waitpid so we can find out if srun succeeds! */
    prte_wait_cb(dummy, srun_wait_cb, NULL);

    if (0 == srun_pid) { /* child */
        char *bin_base = NULL, *lib_base = NULL;

        /* Slurm forwards the entire environment, which we
         * REALLY don't want them to do as it might contain
         * envars pertaining to tool connections. So purge
         * the environment of anything PMIx/PRRTE related -
         * we will have put anything we need on the cmd line */
        for (n=0; NULL != environ[n]; n++) {
            if (0 == strncmp(environ[n], "PMIX_", 5) ||
                0 == strncmp(environ[n], "PRTE_", 5)) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&tmp, environ[n]);
            }
        }
        if (NULL != tmp) {
            for (n=0; NULL != tmp[n]; n++) {
                p = strchr(tmp[n], '=');
                *p = '\0';
                unsetenv(tmp[n]);
            }
            PMIX_ARGV_FREE_COMPAT(tmp);
        }

        /* Figure out the basenames for the libdir and bindir.  There
           is a lengthy comment about this in plm_rsh_module.c
           explaining all the rationale for how / why we're doing
           this. */

        lib_base = pmix_basename(prte_install_dirs.libdir);
        bin_base = pmix_basename(prte_install_dirs.bindir);

        /* If we have a prefix, then modify the PATH and
           LD_LIBRARY_PATH environment variables.  */
        if (NULL != prefix) {
            char *oldenv, *newenv;

            /* Reset PATH */
            oldenv = getenv("PATH");
            if (NULL != oldenv) {
                pmix_asprintf(&newenv, "%s/%s:%s", prefix, bin_base, oldenv);
            } else {
                pmix_asprintf(&newenv, "%s/%s", prefix, bin_base);
            }
            setenv("PATH", newenv, true);
            PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                                 "%s plm:slurm: reset PATH: %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 newenv));
            free(newenv);

            /* Reset LD_LIBRARY_PATH */
            oldenv = getenv("LD_LIBRARY_PATH");
            if (NULL != oldenv) {
                pmix_asprintf(&newenv, "%s/%s:%s", prefix, lib_base, oldenv);
            } else {
                pmix_asprintf(&newenv, "%s/%s", prefix, lib_base);
            }
            setenv("LD_LIBRARY_PATH", newenv, true);
            PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                                 "%s plm:slurm: reset LD_LIBRARY_PATH: %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), newenv));
            free(newenv);
        }

        fd = open("/dev/null", O_CREAT | O_RDWR | O_TRUNC, 0666);
        if (fd >= 0) {
            dup2(fd, 0);
            /* When not in debug mode and --debug-daemons was not passed,
             * tie stdout/stderr to dev null so we don't see messages from orted
             * EXCEPT if the user has requested that we leave sessions attached
             */
            if (0 > pmix_output_get_verbosity(prte_plm_base_framework.framework_output)
                && !prte_debug_daemons_flag && !prte_leave_session_attached) {
                dup2(fd, 1);
                dup2(fd, 2);
            }

            /* Don't leave the extra fd to /dev/null open */
            if (fd > 2) {
                close(fd);
            }
        }

        /* get the srun process out of prun's process group so that
           signals sent from the shell (like those resulting from
           cntl-c) don't get sent to srun */
        setpgid(0, 0);

        execvp(exec_argv, argv);

        pmix_output(0, "plm:slurm:start_proc: exec failed");
        /* don't return - need to exit - returning would be bad -
           we're not in the calling process anymore */
        exit(1);
    } else { /* parent */
        /* just in case, make sure that the srun process is not in our
           process group any more.  Stevens says always do this on both
           sides of the fork... */
        setpgid(srun_pid, srun_pid);

        free(exec_argv);
    }

    return PRTE_SUCCESS;
}
