/* -*- C -*-
 *
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/base.h"
#include "opal/threads/condition.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/version.h"
#include "opal/runtime/opal.h"

#include "orte/orte_constants.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/util/pre_condition_transports.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"
#include "orte/runtime/orte_wait.h"

#include "orterun.h"
#include "totalview.h"

/*
 * Globals
 */
static struct opal_event term_handler;
static struct opal_event int_handler;
#ifndef __WINDOWS__
static struct opal_event sigusr1_handler;
static struct opal_event sigusr2_handler;
#endif  /* __WINDOWS__ */
static orte_jobid_t jobid = ORTE_JOBID_INVALID;
static orte_pointer_array_t *apps_pa;
static bool wait_for_job_completion = true;
static char *orterun_basename = NULL;
static int max_display_aborted = 1;
static int num_aborted = 0;
static int num_killed = 0;
static char **global_mca_env = NULL;
static bool have_zero_np = false;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;

/*
 * setup globals for catching orterun command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool quiet;
    bool exit;
    bool no_wait_for_job_completion;
    bool by_node;
    bool by_slot;
    bool do_not_launch;
    bool debugger;
    int num_procs;
    int exit_status;
    char *hostfile;
    char *env_val;
    char *appfile;
    char *wdir;
    char *path;
    opal_mutex_t lock;
    opal_condition_t cond;
} orterun_globals;
static bool globals_init = false;


opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orterun_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &orterun_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &orterun_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &orterun_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &orterun_globals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    
    /* Set a hostfile */
    { "rds", "hostfile", "path", '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "rds", "hostfile", "path", '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    /* Don't wait for the process to finish before exiting */
#if 0
    { NULL, NULL, NULL, '\0', "nw", "nw", 0,
      &orterun_globals.no_wait_for_job_completion, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch the processes and do not wait for their completion (i.e., let orterun complete as soon a successful launch occurs)" },
#endif
    
    /* Set the max number of aborted processes to show */
    { NULL, NULL, NULL, '\0', "aborted", "aborted", 1,
      &max_display_aborted, OPAL_CMD_LINE_TYPE_INT,
      "The maximum number of aborted processes to display" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Specific mapping (C, cX, N, nX) */
#if 0
    /* JJH --map is not currently implemented so don't advertise it until it is */
    { NULL, NULL, NULL, '\0', NULL, "map", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping of processes to nodes / CPUs" },
#endif
    { NULL, NULL, NULL, '\0', "bynode", "bynode", 0,
      &orterun_globals.by_node, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by node" },
    { NULL, NULL, NULL, '\0', "byslot", "byslot", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by slot (the default)" },
    { "rmaps", "base", "pernode", '\0', "pernode", "pernode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node on the specified number of nodes [no -np => use all allocated nodes]" },
    { "rmaps", "base", "n_pernode", '\0', "npernode", "npernode", 1,
        NULL, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes" },
    { "rmaps", "base", "no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { "rmaps", "base", "display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch"},
    
    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "wd", "wd", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &orterun_globals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },
    /* These arguments can be specified multiple times */
#if 0
    /* JMS: Removed because it's not really implemented */
    { NULL, NULL, NULL, '\0', "arch", "arch", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Architecture to start processes on" },
#endif
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

    /* OSC mpiexec-like arguments */
    { "rmaps", "base", "no_schedule_local", '\0', "nolocal", "nolocal", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },

    /* User-level debugger arguments */
    { NULL, NULL, NULL, '\0', "tv", "tv", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"" },
    { NULL, NULL, NULL, '\0', "debug", "debug", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter" },
    { "orte", "base", "user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', NULL, "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    
    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not detach OpenRTE daemons used by this application" },
    
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

    { NULL, NULL, NULL, '\0', NULL, "do-not-launch", 0,
        &orterun_globals.do_not_launch, OPAL_CMD_LINE_TYPE_BOOL,
        "Perform all necessary operations to prepare to launch the application, but do not actually launch it" },
    
    { "pls", "base", "reuse_daemons", '\0', "reuse-daemons", "reuse-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "If set, reuse daemons to launch dynamically spawned processes"},

    { NULL, NULL, NULL, '\0', NULL, "prefix", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Prefix where Open MPI is installed on remote nodes" },
    { NULL, NULL, NULL, '\0', NULL, "noprefix", 0,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Disable automatic --prefix behavior" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Local functions
 */
static void exit_callback(int fd, short event, void *arg);
static void abort_signal_callback(int fd, short event, void *arg);
static void signal_forward_callback(int fd, short event, void *arg);
static int create_app(int argc, char* argv[], orte_app_context_t **app,
                      bool *made_app, char ***app_env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[]);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename, char ***env);
static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);
static void dump_aborted_procs(orte_jobid_t jobid);


int orterun(int argc, char *argv[])
{
    orte_app_context_t **apps;
    int rc, ret, i, num_apps, array_size;
    orte_proc_state_t cb_states;
    orte_job_state_t exit_state;
    opal_list_t attributes;
    opal_list_item_t *item;
    uint8_t flow;

    /* Need to initialize OPAL so that install_dirs are filled in */

    opal_init_util();

    /* Setup MCA params */

    mca_base_param_init();
    orte_register_params(false);

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orterun_basename = opal_basename(argv[0]);

    /* Check for some "global" command line params */

    parse_globals(argc, argv);

    /* If we're still here, parse each app */

    parse_locals(argc, argv);

    /* Convert the list of apps to an array of orte_app_context_t
       pointers */
    array_size = orte_pointer_array_get_size(apps_pa);
    apps = (orte_app_context_t**)malloc(sizeof(orte_app_context_t *) * array_size);
    if (NULL == apps) {
        opal_show_help("help-orterun.txt", "orterun:call-failed",
                       true, orterun_basename, "system", "malloc returned NULL", errno);
        exit(1);
    }
    num_apps = 0;
    for (i = 0; i < array_size; ++i) {
        apps[num_apps] = (orte_app_context_t *)
            orte_pointer_array_get_item(apps_pa, i);
        if (NULL != apps[num_apps]) {
            num_apps++;
        }
    }
    if (0 == num_apps) {
        /* This should never happen -- this case should be caught in
           create_app(), but let's just double check... */
        opal_show_help("help-orterun.txt", "orterun:nothing-to-do",
                       true, orterun_basename);
        exit(1);
    }

    /* Intialize our Open RTE environment */
    /* Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        opal_show_help("help-orterun.txt", "orterun:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

    /* pre-condition any network transports that require it */
    if (ORTE_SUCCESS != (rc = orte_pre_condition_transports(apps, num_apps))) {
        ORTE_ERROR_LOG(rc);
        opal_show_help("help-orterun.txt", "orterun:precondition", false,
                       orterun_basename, NULL, NULL, rc);
        return rc;
    }


    /* Prep to start the application */
    /* construct the list of attributes */
    OBJ_CONSTRUCT(&attributes, opal_list_t);
    
    if (orterun_globals.do_not_launch) {
        flow = ORTE_RMGR_SETUP | ORTE_RMGR_RES_DISC | ORTE_RMGR_ALLOC | ORTE_RMGR_MAP | ORTE_RMGR_SETUP_TRIGS;
        orte_rmgr.add_attribute(&attributes, ORTE_RMGR_SPAWN_FLOW, ORTE_UINT8, &flow, ORTE_RMGR_ATTR_OVERRIDE);
    }

    /** setup callbacks for abort signals */
    opal_signal_set(&term_handler, SIGTERM,
                    abort_signal_callback, &term_handler);
    opal_signal_add(&term_handler, NULL);
    opal_signal_set(&int_handler, SIGINT,
                    abort_signal_callback, &int_handler);
    opal_signal_add(&int_handler, NULL);

#ifndef __WINDOWS__
    /** setup callbacks for signals we should foward */
    opal_signal_set(&sigusr1_handler, SIGUSR1,
                    signal_forward_callback, &sigusr1_handler);
    opal_signal_add(&sigusr1_handler, NULL);
    opal_signal_set(&sigusr2_handler, SIGUSR2,
                    signal_forward_callback, &sigusr2_handler);
    opal_signal_add(&sigusr2_handler, NULL);
#endif  /* __WINDOWS__ */
    orte_totalview_init_before_spawn();

    /* Spawn the job */

    cb_states = ORTE_PROC_STATE_TERMINATED | ORTE_PROC_STATE_AT_STG1;
    rc = orte_rmgr.spawn_job(apps, num_apps, &jobid, 0, NULL, job_state_callback, cb_states, &attributes);
    while (NULL != (item = opal_list_remove_first(&attributes))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attributes);
    
    if (ORTE_SUCCESS != rc) {
        /* JMS show_help unless it is ERR_SILENT */
        if (ORTE_ERR_SILENT != rc) {
            opal_output(0, "%s: spawn failed with errno=%d\n", orterun_basename, rc);
        }
    } else {

        if (orterun_globals.do_not_launch) {
            /* we are done! */
            goto DONE;
        }
        
        /* Wait for the app to complete */

        if (wait_for_job_completion) {
            OPAL_THREAD_LOCK(&orterun_globals.lock);
            while (!orterun_globals.exit) {
                opal_condition_wait(&orterun_globals.cond,
                                    &orterun_globals.lock);
            }
            /* check to see if the job was aborted */
            if (ORTE_JOBID_INVALID != jobid &&
                ORTE_SUCCESS != (rc = orte_smr.get_job_state(&exit_state, jobid))) {
                if (ORTE_SUCCESS != rc) {
                    ORTE_ERROR_LOG(rc);
                }
                /* define the exit state as abnormal by default */
                exit_state = ORTE_JOB_STATE_ABORTED;
            }
            if (ORTE_JOB_STATE_TERMINATED != exit_state) {
                /* abnormal termination of some kind */
                dump_aborted_procs(jobid);
                /* If we showed more abort messages than were allowed,
                show a followup message here */
                if (num_aborted > max_display_aborted) {
                    i = num_aborted - max_display_aborted;
                    printf("%d additional process%s aborted (not shown)\n",
                           i, ((i > 1) ? "es" : ""));
                }
                if (num_killed > 0) {
                    printf("%d process%s killed (possibly by Open MPI)\n",
                           num_killed, ((num_killed > 1) ? "es" : ""));
                }
            }
            /* Make sure we propagate the exit code */
            if (WIFEXITED(orterun_globals.exit_status)) {
                rc = WEXITSTATUS(orterun_globals.exit_status);
            } else {
                /* If a process was killed by a signal, then make the
                 * exit code of orterun be "signo + 128" so that "prog"
                 * and "orterun prog" will both set the same status
                 * value for the shell */
                rc = WTERMSIG(orterun_globals.exit_status) + 128;
            }
            
            /* the job is complete - now tell the orteds that it is
             * okay to finalize and exit, we are done with them
             * be sure to include any descendants so nothing is
             * left hanging
             */
            if (ORTE_JOBID_INVALID != jobid) {
                OBJ_CONSTRUCT(&attributes, opal_list_t);
                orte_rmgr.add_attribute(&attributes, ORTE_NS_INCLUDE_DESCENDANTS, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
                if (ORTE_SUCCESS != (ret = orte_pls.terminate_orteds(jobid, &orte_abort_timeout, &attributes))) {
                    opal_show_help("help-orterun.txt", "orterun:daemon-die", true,
                                   orterun_basename, ORTE_ERROR_NAME(ret));
                }
                while (NULL != (item = opal_list_remove_first(&attributes))) {
                    OBJ_RELEASE(item);
                }
                OBJ_DESTRUCT(&attributes);
            }
            OPAL_THREAD_UNLOCK(&orterun_globals.lock);

            /* If we were forcibly killed, print a warning that the
               user may still have some manual cleanup to do. */
            if (ORTE_JOBID_INVALID == jobid) {
                opal_show_help("help-orterun.txt", "orterun:abnormal-exit",
                               true, orterun_basename, orterun_basename);
            }
        }
    }

DONE:
    for (i = 0; i < num_apps; ++i) {
        OBJ_RELEASE(apps[i]);
    }
    free(apps);
    OBJ_RELEASE(apps_pa);
    
    orte_finalize();
    free(orterun_basename);
    return rc;
}

/*
 * On abnormal termination - dump the
 * exit status of the aborted procs.
 */

static void dump_aborted_procs(orte_jobid_t jobid)
{
    char *segment;
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, k, num_values = 0;
    int rc;
    int32_t exit_status = 0;
    bool exit_status_set;
    char *keys[] = {
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_LOCAL_PID_KEY,
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_EXIT_CODE_KEY,
        ORTE_NODE_NAME_KEY,
        NULL
    };

    OPAL_TRACE_ARG1(1, jobid);

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return;
    }

    for (i = 0; i < num_values; i++) {
        orte_gpr_value_t* value = values[i];
        orte_process_name_t name, *nptr;
        pid_t pid = 0, *pidptr;
        orte_std_cntr_t rank = 0, *sptr;
        bool rank_found=false;
        char* node_name = NULL;
        orte_exit_code_t *ecptr;

        exit_status = 0;
        exit_status_set = false;
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&nptr, keyval->value, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                name = *nptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                pid = *pidptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                rank_found = true;
                rank = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_EXIT_CODE_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&ecptr, keyval->value, ORTE_EXIT_CODE))) {
                    ORTE_ERROR_LOG(rc);
                    continue;
                }
                exit_status = *ecptr;
                exit_status_set = true;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                node_name = (char*)(keyval->value->data);
                continue;
            }
        }

        if (rank_found) {
            if (WIFSIGNALED(exit_status)) {
                if (9 == WTERMSIG(exit_status)) {
                    ++num_killed;
                } else {
                    if (num_aborted < max_display_aborted) {
#ifdef HAVE_STRSIGNAL
                        if (NULL != strsignal(WTERMSIG(exit_status))) {
                            opal_show_help("help-orterun.txt", "orterun:proc-aborted-strsignal", false,
                                       orterun_basename, (unsigned long)rank, (unsigned long)pid,
                                       node_name, WTERMSIG(exit_status), 
                                       strsignal(WTERMSIG(exit_status)));
                        } else {
#endif
                            opal_show_help("help-orterun.txt", "orterun:proc-aborted", false,
                                       orterun_basename, (unsigned long)rank, (unsigned long)pid,
                                       node_name, WTERMSIG(exit_status));
#ifdef HAVE_STRSIGNAL
                        }
#endif
                    }
                    ++num_aborted;
                }
            }
        }

        /* If we haven't done so already, hold the exit_status so we
           can return it when exiting.  Specifically, keep the first
           non-zero entry.  If they all return zero, we'll return
           zero.  We already have the globals.lock (from
           job_state_callback), so don't try to get it again. */

        if (0 == orterun_globals.exit_status && exit_status_set) {
            orterun_globals.exit_status = exit_status;
        }

        OBJ_RELEASE(value);
    }
    if (NULL != values) {
        free(values);
    }
    free(segment);
}


/*
 * signal main thread when application completes
 */

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state)
{
    OPAL_TRACE_ARG2(1, jobid, state);

    OPAL_THREAD_LOCK(&orterun_globals.lock);

    /* Note that there's only three states that we're interested in
       here:

       TERMINATED: which means that all the processes in the job have
                completed (normally and/or abnormally).

       AT_STG1: which means that everyone has hit stage gate 1, so we
                can do the parallel debugger startup stuff.

       Remember that the rmgr itself will also be called for the
       ABORTED state and call the pls.terminate_job, which will result
       in killing all the other processes. */

    if (orte_debug_flag) {
        opal_output(0, "spawn: in job_state_callback(jobid = %d, state = 0x%x)\n",
                    jobid, state);
    }

    switch(state) {
        case ORTE_PROC_STATE_TERMINATED:
            orterun_globals.exit_status = 0;  /* set the exit status to indicate normal termination */
            orterun_globals.exit = true;
            opal_condition_signal(&orterun_globals.cond);
            break;

        case ORTE_PROC_STATE_AT_STG1:
            orte_totalview_init_after_spawn(jobid);
            break;

        default:
            opal_output(0, "orterun: job state callback in unexpected state - jobid %lu, state 0x%04x\n", jobid, state);
            break;
    }
    OPAL_THREAD_UNLOCK(&orterun_globals.lock);
}

/*
 * Fail-safe in the event the job hangs and doesn't
 * cleanup correctly.
 */

static void exit_callback(int fd, short event, void *arg)
{
    OPAL_TRACE(1);

    /* Remove the TERM and INT signal handlers */
    opal_signal_del(&term_handler);
    opal_signal_del(&int_handler);
#ifndef __WINDOWS__
    /** Remove the USR signal handlers */
    opal_signal_del(&sigusr1_handler);
    opal_signal_del(&sigusr2_handler);
#endif  /* __WINDOWS__ */

    /* Trigger the normal exit conditions */
    orterun_globals.exit = true;
    orterun_globals.exit_status = 1;
    opal_condition_signal(&orterun_globals.cond);
}


/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted.
 */

typedef enum {
    ABORT_SIGNAL_FIRST,
    ABORT_SIGNAL_PROCESSING,
    ABORT_SIGNAL_WARNED,
    ABORT_SIGNAL_DONE
} abort_signal_state_t;

static void abort_signal_callback(int fd, short flags, void *arg)
{
    int ret;
    opal_event_t* event;
    opal_list_t attrs;
    opal_list_item_t *item;
    static abort_signal_state_t state=ABORT_SIGNAL_FIRST;
    static struct timeval invoked, now;
    double a, b;
    
    OPAL_TRACE(1);
    
    /* If this whole process has already completed, then bail */
    switch (state) {
    case ABORT_SIGNAL_FIRST:
        /* This is the first time through */
        state = ABORT_SIGNAL_PROCESSING;
        break;
            
    case ABORT_SIGNAL_WARNED:
        gettimeofday(&now, NULL);
        a = invoked.tv_sec * 1000000 + invoked.tv_usec;
        b = now.tv_sec * 1000000 + invoked.tv_usec;
        if (b - a <= 1000000) {
            if (!orterun_globals.quiet){
                fprintf(stderr, "%s: forcibly killing job...\n", 
                        orterun_basename);
            }

            /* tell the pls to cancel the terminate request -
             * obviously, something is wrong at this point
             */
            if (ORTE_SUCCESS != (ret = orte_pls.cancel_operation())) {
                ORTE_ERROR_LOG(ret);
            }
            
            /* We are in an event handler; exit_callback() will delete
               the handler that is currently running (which is a Bad
               Thing), so we can't call it directly.  Instead, we have
               to exit this handler and setup to call exit_handler()
               after this. */
            if (NULL != (event = (opal_event_t*)
                         malloc(sizeof(opal_event_t)))) {
                opal_evtimer_set(event, exit_callback, NULL);
                now.tv_sec = 0;
                now.tv_usec = 0;
                opal_evtimer_add(event, &now);
                state = ABORT_SIGNAL_DONE;
            }
            return;
        } 
        /* Otherwise fall through to PROCESSING and warn again */
                
    case ABORT_SIGNAL_PROCESSING:
        opal_show_help("help-orterun.txt", "orterun:sigint-while-processing",
                       true, orterun_basename, orterun_basename, 
                       orterun_basename);
        gettimeofday(&invoked, NULL);
        state = ABORT_SIGNAL_WARNED;
        return;
        
    case ABORT_SIGNAL_DONE:
        /* Nothing to do -- return */
        return;
    }

    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: killing job...\n\n", orterun_basename);
    }
    
    /* terminate the job - this will also wakeup orterun so
     * it can kill all the orteds. Be sure to kill all the job's
     * descendants, if any, so nothing is left hanging
     */
    if (jobid != ORTE_JOBID_INVALID) {
        OBJ_CONSTRUCT(&attrs, opal_list_t);
        orte_rmgr.add_attribute(&attrs, ORTE_NS_INCLUDE_DESCENDANTS, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
        ret = orte_pls.terminate_job(jobid, &orte_abort_timeout, &attrs);
        while (NULL != (item = opal_list_remove_first(&attrs))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&attrs);
        if (ORTE_SUCCESS != ret) {
            /* If we failed the terminate_job() above, then the
               condition variable in the main loop in orterun won't
               wake up.  So signal it. */
            if (NULL != (event = (opal_event_t*)
                         malloc(sizeof(opal_event_t)))) {
                opal_evtimer_set(event, exit_callback, NULL);
                now.tv_sec = 0;
                now.tv_usec = 0;
                opal_evtimer_add(event, &now);
            } else {
                /* We really don't want to do this, but everything
                   else has failed... */
                orterun_globals.exit = true;
                orterun_globals.exit_status = 1;
                opal_condition_signal(&orterun_globals.cond);
            }

            jobid = ORTE_JOBID_INVALID;
        }
    }

    state = ABORT_SIGNAL_DONE;
}

/**
 * Pass user signals to the remote application processes
 */
static void  signal_forward_callback(int fd, short event, void *arg)
{
    struct opal_event *signal = (struct opal_event*)arg;
    int signum, ret;
    opal_list_t attrs;
    opal_list_item_t *item;

    OPAL_TRACE(1);

    signum = OPAL_EVENT_SIGNAL(signal);
    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: Forwarding signal %d to job",
                orterun_basename, signum);
    }

    /** send the signal out to the processes, including any descendants */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_INCLUDE_DESCENDANTS, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
    if (ORTE_SUCCESS != (ret = orte_pls.signal_job(jobid, signum, &attrs))) {
        fprintf(stderr, "Signal %d could not be sent to the job (returned %d)",
                signum, ret);
    }
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
}


static int init_globals(void)
{
    /* Only CONSTRUCT things once */
    if (!globals_init) {
        OBJ_CONSTRUCT(&orterun_globals.lock, opal_mutex_t);
        OBJ_CONSTRUCT(&orterun_globals.cond, opal_condition_t);
        orterun_globals.hostfile =    NULL;
        orterun_globals.env_val =     NULL;
        orterun_globals.appfile =     NULL;
        orterun_globals.wdir =        NULL;
        orterun_globals.path =        NULL;
    }

    /* Reset the other fields every time */

    orterun_globals.help                       = false;
    orterun_globals.version                    = false;
    orterun_globals.verbose                    = false;
    orterun_globals.quiet                      = false;
    orterun_globals.exit                       = false;
    orterun_globals.no_wait_for_job_completion = false;
    orterun_globals.by_node                    = false;
    orterun_globals.by_slot                    = false;
    orterun_globals.debugger                   = false;
    orterun_globals.do_not_launch              = false;
    orterun_globals.num_procs                  =  0;
    orterun_globals.exit_status                =  0;
    if( NULL != orterun_globals.hostfile )
        free( orterun_globals.hostfile );
    orterun_globals.hostfile =    NULL;
    if( NULL != orterun_globals.env_val )
        free( orterun_globals.env_val );
    orterun_globals.env_val =     NULL;
    if( NULL != orterun_globals.appfile )
        free( orterun_globals.appfile );
    orterun_globals.appfile =     NULL;
    if( NULL != orterun_globals.wdir )
        free( orterun_globals.wdir );
    orterun_globals.wdir =        NULL;
    if( NULL != orterun_globals.path )
        free( orterun_globals.path );
    orterun_globals.path =        NULL;

    /* All done */
    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[])
{
    opal_cmd_line_t cmd_line;
    int id, ret;

    /* Setup and parse the command line */

    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (ORTE_SUCCESS != (ret = opal_cmd_line_parse(&cmd_line, true,
                                                   argc, argv)) ) {
        return ret;
    }

    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (orterun_globals.version && 
        !(1 == argc || orterun_globals.help)) {
        char *project_name = NULL;
        if (0 == strcmp(orterun_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        opal_show_help("help-orterun.txt", "orterun:version", false,
                       orterun_basename, project_name, OPAL_VERSION,
                       PACKAGE_BUGREPORT);
        /* if we were the only argument, exit */
        if (2 == argc) exit(0);
    }

    /* Check for help request */

    if (1 == argc || orterun_globals.help) {
        char *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orterun_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orterun.txt", "orterun:usage", false,
                       orterun_basename, project_name, OPAL_VERSION,
                       orterun_basename, args,
                       PACKAGE_BUGREPORT);
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    /* Do we want a user-level debugger? */

    if (orterun_globals.debugger) {
        orte_run_debugger(orterun_basename, argc, argv);
    }

    /* Allocate and map by node or by slot?  Shortcut for setting an
       MCA param. */

    /* JMS To be changed post-beta to LAM's C/N command line notation */
    /* Don't initialize the MCA parameter here unless we have to,
     * since it really should be initialized in rmaps_base_open */
    if (orterun_globals.by_node || orterun_globals.by_slot) {
        char *policy = NULL;
        id = mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                            "Scheduling policy for RMAPS. [slot | node]",
                                            false, false, "slot", &policy);

        if (orterun_globals.by_node) {
            orterun_globals.by_slot = false;
            mca_base_param_set_string(id, "node");
        } else {
            orterun_globals.by_slot = true;
            mca_base_param_set_string(id, "slot");
        }
        free(policy);
    }
    else {
        /* Default */
        orterun_globals.by_slot = true;
    }

    /* If we don't want to wait, we don't want to wait */

    if (orterun_globals.no_wait_for_job_completion) {
        wait_for_job_completion = false;
    }

    OBJ_DESTRUCT(&cmd_line);
    return ORTE_SUCCESS;
}


static int parse_locals(int argc, char* argv[])
{
    int i, rc, app_num;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;

    /* Make the apps */

    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);
    /* Make the max size of the array be INT_MAX because we may be
       parsing an app file, in which case we don't know how many
       entries there will be.  The max size of an orte_pointer_array
       is only a safety net; it only initially allocates block_size
       entries (2, in this case) */
    orte_pointer_array_init(&apps_pa, 1, INT_MAX, 2);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
                /** keep track of the number of apps - point this app_context to that index */
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    orte_std_cntr_t dummy;
                    app->idx = app_num;
                    ++app_num;
                    orte_pointer_array_add(&dummy, apps_pa, app);
                }

                /* Reset the temps */

                temp_argc = 0;
                temp_argv = NULL;
                opal_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            opal_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (opal_argv_count(temp_argv) > 1) {
        app = NULL;
        rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            orte_std_cntr_t dummy;
            app->idx = app_num;
            ++app_num;
            orte_pointer_array_add(&dummy, apps_pa, app);
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

    /* Once we've created all the apps, add the global MCA params to
       each app's environment (checking for duplicates, of
       course -- yay opal_environ_merge()).  */

    if (NULL != global_mca_env) {
        size1 = orte_pointer_array_get_size(apps_pa);
        /* Iterate through all the apps */
        for (j = 0; j < size1; ++j) {
            app = (orte_app_context_t *)
                orte_pointer_array_get_item(apps_pa, j);
            if (NULL != app) {
                /* Use handy utility function */
                env = opal_environ_merge(global_mca_env, app->env);
                opal_argv_free(app->env);
                app->env = env;
            }
        }
    }

    /* Now take a subset of the MCA params and set them as MCA
       overrides here in orterun (so that when we orte_init() later,
       all the components see these MCA params).  Here's how we decide
       which subset of the MCA params we set here in orterun:

       1. If any global MCA params were set, use those
       2. If no global MCA params were set and there was only one app,
          then use its app MCA params
       3. Otherwise, don't set any
    */

    env = NULL;
    if (NULL != global_mca_env) {
        env = global_mca_env;
    } else {
        if (orte_pointer_array_get_size(apps_pa) >= 1) {
            /* Remember that pointer_array's can be padded with NULL
               entries; so only use the app's env if there is exactly
               1 non-NULL entry */
            app = (orte_app_context_t *)
                orte_pointer_array_get_item(apps_pa, 0);
            if (NULL != app) {
                env = app->env;
                for (j = 1; j < orte_pointer_array_get_size(apps_pa); ++j) {
                    if (NULL != orte_pointer_array_get_item(apps_pa, j)) {
                        env = NULL;
                        break;
                    }
                }
            }
        }
    }

    if (NULL != env) {
        size1 = opal_argv_count(env);
        for (j = 0; j < size1; ++j) {
            putenv(env[j]);
        }
    }

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   orterun --mca foo bar -app appfile
 *
 * That is, we'll need to keep foo=bar, but the presence of the app
 * file will cause an invocation of parse_appfile(), which will cause
 * one or more recursive calls back to create_app().  Since the
 * foo=bar value applies globally to all apps in the appfile, we need
 * to pass in the "base" environment (that contains the foo=bar value)
 * when we parse each line in the appfile.
 *
 * This is really just a special case -- when we have a simple case like:
 *
 *   orterun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[], orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    opal_cmd_line_t cmd_line;
    char cwd[OMPI_PATH_MAX];
    int i, j, count, rc;
    char *param, *value, *value2;
    orte_app_context_t *app = NULL;
#if 0 /* Used only in the C/N notion case, remove to silence compiler warnings */
    orte_std_cntr_t l, len;
#endif
    bool map_data = false, save_arg, cmd_line_made = false;
    int new_argc = 0;
    char **new_argv = NULL;

    *made_app = false;

    /* Pre-process the command line:

       - convert C, cX, N, nX arguments to "-rawmap <id> <arg>" so
         that the parser can pick it up nicely.
       - convert -host to -rawmap <id> <arg>
       - convert -arch to -rawmap <id> <arg>

       Converting these to the same argument type will a) simplify the
       logic down below, and b) allow us to preserve the ordering of
       these arguments as the user specified them on the command
       line.  */

    for (i = 0; i < argc; ++i) {
        map_data = false;
        save_arg = true;

    /* JJH To fix in the future
     * Currently C/N notation is not supported so don't execute this check
     * Bug: Make this context sensitive since it will not behave properly
     *      with the following argument set:
     *      $ orterun -np 2 -host c2,c3,c12 hostname
     *      Since it will see the hosts c2, c3, and c12 as C options instead
     *      of hostnames.
     */
        if(false) { ; } /* Wrapper to preserve logic continuation while the below
                           is commented out */
#if 0
        if (0 == strcmp(argv[i], "C") ||
            0 == strcmp(argv[i], "N")) {
            map_data = true;
        }

        /* Heuristic: if the string fits "[cn][0-9]+" or "[cn][0-9],",
           then accept it as mapping data */

        else if ('c' == argv[i][0] || 'n' == argv[i][0]) {
            len = strlen(argv[i]);
            if (len > 1) {
                for (l = 1; l < len; ++l) {
                    if (',' == argv[i][l]) {
                        map_data = true;
                        break;
                    } else if (!isdigit(argv[i][l])) {
                        break;
                    }
                }
                if (l >= len) {
                    map_data = true;
                }
            }
        }
#endif

#if 0
        /* JMS commented out because we don't handle this in any
           mapper */
        /* Save -arch args */

        else if (0 == strcmp("-arch", argv[i])) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_ARCH, '\0' };

            opal_argv_append(&new_argc, &new_argv, "-rawmap");
            opal_argv_append(&new_argc, &new_argv, str);
            save_arg = false;
        }
#endif

        /* Save -host args */
        else if (0 == strcmp("--host",argv[i]) ||
                 0 == strcmp("-host", argv[i]) ||
                 0 == strcmp("-H", argv[i])) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_HOSTNAME, '\0' };
            opal_argv_append(&new_argc, &new_argv, "-rawmap");
            opal_argv_append(&new_argc, &new_argv, str);
            save_arg = false;
        }

        /* If this token was C/N map data, save it */

        if (map_data) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_CN, '\0' };

            opal_argv_append(&new_argc, &new_argv, "-rawmap");
            opal_argv_append(&new_argc, &new_argv, str);
        }

        if (save_arg) {
            opal_argv_append(&new_argc, &new_argv, argv[i]);
        }
    }

    /* Parse application command line options.  Add the -rawmap option
       separately so that the user doesn't see it in the --help
       message. */

    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    cmd_line_made = true;
    opal_cmd_line_make_opt3(&cmd_line, '\0', NULL, "rawmap", 2,
                            "Hidden / internal parameter -- users should not use this!");
    rc = opal_cmd_line_parse(&cmd_line, true, new_argc, new_argv);
    opal_argv_free(new_argv);
    new_argv = NULL;
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    mca_base_cmd_line_process_args(&cmd_line, app_env, &global_mca_env);

    /* Is there an appfile in here? */

    if (NULL != orterun_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(orterun_globals.appfile), app_env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    opal_cmd_line_get_tail(&cmd_line, &count, &app->argv);

    /* See if we have anything left */

    if (0 == count) {
        opal_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, orterun_basename, orterun_basename);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Grab all OMPI_* environment variables */

    app->env = opal_argv_copy(*app_env);
    for (i = 0; NULL != environ[i]; ++i) {
        if (0 == strncmp("OMPI_", environ[i], 5)) {
            opal_argv_append_nosize(&app->env, environ[i]);
        }
    }

    /* Did the user request to export any environment variables? */

    if (opal_cmd_line_is_taken(&cmd_line, "x")) {
        j = opal_cmd_line_get_ninsts(&cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = opal_cmd_line_get_param(&cmd_line, "x", i, 0);

            if (NULL != strchr(param, '=')) {
                opal_argv_append_nosize(&app->env, param);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        opal_argv_append_nosize(&app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", param, value);
                        opal_argv_append_nosize(&app->env, value2);
                        free(value2);
                    }
                } else {
                    opal_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
    }

    /* Did the user request a specific path? */

    if (NULL != orterun_globals.path) {
        asprintf(&value, "PATH=%s", orterun_globals.path);
        opal_argv_append_nosize(&app->env, value);
        free(value);
    }

    /* Did the user request a specific wdir? */

    if (NULL != orterun_globals.wdir) {
        app->cwd = strdup(orterun_globals.wdir);
        app->user_specified_cwd = true;
    } else {
        getcwd(cwd, sizeof(cwd));
        app->cwd = strdup(cwd);
        app->user_specified_cwd = false;
    }

    /* Check to see if the user explicitly wanted to disable automatic
       --prefix behavior */

    if (opal_cmd_line_is_taken(&cmd_line, "noprefix")) {
        want_prefix_by_default = false;
    }

    /* Did the user specify a specific prefix for this app_context_t
       or provide an absolute path name to argv[0]? */
    if (opal_cmd_line_is_taken(&cmd_line, "prefix") ||
        '/' == argv[0][0] || want_prefix_by_default) {
        size_t param_len;

        /* The --prefix option takes precedence over /path/to/orterun */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix")) {
            param = opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0);
        } 
        /* /path/to/orterun */
        else if ('/' == argv[0][0]) {
            char* tmp_basename = NULL;
            /* If they specified an absolute path, strip off the
               /bin/<exec_name>" and leave just the prefix */
            param = opal_dirname(argv[0]);
            /* Quick sanity check to ensure we got
               something/bin/<exec_name> and that the installation
               tree is at least more or less what we expect it to
               be */
            tmp_basename = opal_basename(param);
            if (0 == strcmp("bin", tmp_basename)) {
                char* tmp = param;
                param = opal_dirname(tmp);
                free(tmp);
            } else {
                free(param);
                param = NULL;
            }
            free(tmp_basename);
        }
        /* --enable-orterun-prefix-default was given to orterun */
        else {
            param = strdup(opal_install_dirs.prefix);
        }

        if (NULL != param) {
            /* "Parse" the param, aka remove superfluous path_sep. */
            param_len = strlen(param);
            while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                param[param_len-1] = '\0';
                param_len--;
                if (0 == param_len) {
                    opal_show_help("help-orterun.txt", "orterun:empty-prefix",
                                   true, orterun_basename, orterun_basename);
                    return ORTE_ERR_FATAL;
                }
            }

            app->prefix_dir = strdup(param);
        }
    }

    /* Did the user request any mappings?  They were all converted to
       --rawmap items, above. */

    if (opal_cmd_line_is_taken(&cmd_line, "rawmap")) {
        j = opal_cmd_line_get_ninsts(&cmd_line, "rawmap");
        app->map_data = (orte_app_context_map_t**)malloc(sizeof(orte_app_context_map_t*) * j);
        if (NULL == app->map_data) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        app->num_map = j;
        for (i = 0; i < j; ++i) {
            app->map_data[i] = NULL;
        }
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(&cmd_line, "rawmap", i, 0);
            value2 = opal_cmd_line_get_param(&cmd_line, "rawmap", i, 1);
            app->map_data[i] = OBJ_NEW(orte_app_context_map_t);
            if (NULL == app->map_data[i]) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            app->map_data[i]->map_type = value[0] - '0';
            app->map_data[i]->map_data = strdup(value2);
            /* map_data = true;
             * JJH - This activates the C/N mapping stuff,
             * or at least allows us to pass the 'num_procs' check below.
             * since it is not implemented yet, leave commented. */
        }
    }

    /* Get the numprocs */

    app->num_procs = (orte_std_cntr_t)orterun_globals.num_procs;

    /* If the user didn't specify the number of processes to run, then we
       default to launching an app process using every slot. We can't do
       anything about that here - we leave it to the RMAPS framework's
       components to note this and deal with it later.
        
       HOWEVER, we ONLY support this mode of operation if the number of
       app_contexts is equal to ONE. If the user provides multiple applications,
       we simply must have more information - in this case, generate an
       error.
    */
    if (app->num_procs == 0) {
        have_zero_np = true;  /** flag that we have a zero_np situation */
    }
    if (0 < total_num_apps && have_zero_np) {
        /** we have more than one app and a zero_np - that's no good.
         * note that we have to do this as a two step logic check since
         * the user may fail to specify num_procs for the first app, but
         * then give us another application.
         */
        opal_show_help("help-orterun.txt", "orterun:multi-apps-and-zero-np",
                       true, orterun_basename, NULL);
        return ORTE_ERR_FATAL;
    }
    
    total_num_apps++;
    
    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->app = strdup(app->argv[0]);
    if (NULL == app->app) {
        opal_show_help("help-orterun.txt", "orterun:call-failed",
                       true, orterun_basename, "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (NULL != new_argv) {
        opal_argv_free(new_argv);
    }
    if (cmd_line_made) {
        OBJ_DESTRUCT(&cmd_line);
    }
    return rc;
}


static int parse_appfile(char *filename, char ***env)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc, app_num;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /* Try to open the file */

    fp = fopen(filename, "r");
    if (NULL == fp) {
        opal_show_help("help-orterun.txt", "orterun:appfile-not-found", true,
                       filename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Read in line by line */

    line[sizeof(line) - 1] = '\0';
    app_num = 0;
    do {

        /* We need a bogus argv[0] (because when argv comes in from
           the command line, argv[0] is "orterun", so the parsing
           logic ignores it).  So create one here rather than making
           an argv and then pre-pending a new argv[0] (which would be
           rather inefficient). */

        line[0] = '\0';
        strcat(line, bogus);

        if (NULL == fgets(line + sizeof(bogus) - 1,
                          sizeof(line) - sizeof(bogus) - 1, fp)) {
            break;
        }

        /* Remove a trailing newline */

        len = strlen(line);
        if (len > 0 && '\n' == line[len - 1]) {
            line[len - 1] = '\0';
            if (len > 0) {
                --len;
            }
        }

        /* Remove comments */

        for (i = 0; i < len; ++i) {
            if ('#' == line[i]) {
                line[i] = '\0';
                break;
            } else if (i + 1 < len && '/' == line[i] && '/' == line[i + 1]) {
                line[i] = '\0';
                break;
            }
        }

        /* Is this a blank line? */

        len = strlen(line);
        for (blank = true, i = sizeof(bogus); i < len; ++i) {
            if (!isspace(line[i])) {
                blank = false;
                break;
            }
        }
        if (blank) {
            continue;
        }

        /* We got a line with *something* on it.  So process it */

        argv = opal_argv_split(line, ' ');
        argc = opal_argv_count(argv);
        if (argc > 0) {

            /* Create a temporary env to use in the recursive call --
               that is: don't disturb the original env so that we can
               have a consistent global env.  This allows for the
               case:

                   orterun --mca foo bar --appfile file

               where the "file" contains multiple apps.  In this case,
               each app in "file" will get *only* foo=bar as the base
               environment from which its specific environment is
               constructed. */

            if (NULL != *env) {
                tmp_env = opal_argv_copy(*env);
                if (NULL == tmp_env) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, &app, &made_app, &tmp_env);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
            if (made_app) {
                orte_std_cntr_t dummy;
                app->idx = app_num;
                ++app_num;
                orte_pointer_array_add(&dummy, apps_pa, app);
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}
