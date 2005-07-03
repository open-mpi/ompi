/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include <sys/types.h>
#include <sys/wait.h>

#include "include/orte_constants.h"

#include "opal/event/event.h"
#include "class/orte_pointer_array.h"
#include "util/proc_info.h"
#include "util/argv.h"
#include "util/ompi_environ.h"
#include "util/path.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "util/output.h"
#include "util/universe_setup_file_io.h"
#include "util/show_help.h"
#include "util/basename.h"
#include "opal/threads/condition.h"

#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rmgr/rmgr.h"
#include "mca/schema/schema.h"
#include "mca/errmgr/errmgr.h"

#include "runtime/runtime.h"
#include "runtime/orte_wait.h"


/*
 * The environment
 */
extern char** environ;

/*
 * Globals
 */
static struct opal_event term_handler;
static struct opal_event int_handler;
static orte_jobid_t jobid = ORTE_JOBID_MAX;
static orte_pointer_array_t *apps_pa;
static bool wait_for_job_completion = true;
static char *abort_msg = NULL;
static size_t abort_msg_len = -1;
static char *orterun_basename = NULL;
static int max_display_aborted = 1;
static int num_aborted = 0;
static int num_killed = 0;

/*
 * setup globals for catching orterun command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool exit;
    bool no_wait_for_job_completion;
    bool by_node;
    bool by_slot;
    size_t num_procs;
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

struct proc_info_t {
    bool reported;
    int32_t exit_status;
};
struct proc_info_t *proc_infos = NULL;


ompi_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orterun_globals.help, OMPI_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, '\0', NULL, "version", 0,
      &orterun_globals.version, OMPI_CMD_LINE_TYPE_BOOL,
      "Show the orterun version" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &orterun_globals.verbose, OMPI_CMD_LINE_TYPE_BOOL,
      "Be verbose" },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &orterun_globals.appfile, OMPI_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &orterun_globals.num_procs, OMPI_CMD_LINE_TYPE_SIZE_T,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &orterun_globals.num_procs, OMPI_CMD_LINE_TYPE_SIZE_T,
      "Number of processes to run" },

    /* Set a hostfile */
    { "rds", "hostfile", "path", '\0', "hostfile", "hostfile", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "rds", "hostfile", "path", '\0', "machinefile", "machinefile", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    /* Don't wait for the process to finish before exiting */
    { NULL, NULL, NULL, '\0', "nw", "nw", 0,
      &orterun_globals.no_wait_for_job_completion, OMPI_CMD_LINE_TYPE_BOOL,
      "Launch the processes and do not wait for their completion (i.e., let orterun complete as soon a successful launch occurs)" },

    /* Set the max number of aborted processes to show */
    { NULL, NULL, NULL, '\0', "aborted", "aborted", 1,
      &max_display_aborted, OMPI_CMD_LINE_TYPE_INT,
      "The maximum number of aborted processes to display" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OMPI_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Specific mapping (C, cX, N, nX) */
    { NULL, NULL, NULL, '\0', NULL, "map", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Mapping of processes to nodes / CPUs" },
    { NULL, NULL, NULL, '\0', "bynode", "bynode", 0,
      &orterun_globals.by_node, OMPI_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by node" },
    { NULL, NULL, NULL, '\0', "byslot", "byslot", 0,
      &orterun_globals.by_slot, OMPI_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by slot (the default)" },

    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &orterun_globals.wdir, OMPI_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &orterun_globals.path, OMPI_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },
    /* These arguments can be specified multiple times */
    { NULL, NULL, NULL, '\0', "arch", "arch", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Architecture to start processes on" },
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', NULL, "debug", 0,
      NULL, OMPI_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OMPI_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OMPI_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      NULL, OMPI_CMD_LINE_TYPE_BOOL,
      "Do not detach OpenRTE daemons used by this application" },
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OMPI_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OMPI_CMD_LINE_TYPE_NULL, NULL }
};


/*
 * Local functions
 */
static void exit_callback(int fd, short event, void *arg);
static void signal_callback(int fd, short flags, void *arg);
static int create_app(int argc, char* argv[], orte_app_context_t **app,
                      bool *made_app, char ***env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[]);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename, char ***env);
static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);


int main(int argc, char *argv[])
{
    orte_app_context_t **apps;
    int rc, i, num_apps, j;
    int id, iparam;

    /* Setup the abort message (for use in the signal handler) */

    orterun_basename = ompi_basename(argv[0]);
    asprintf(&abort_msg, "%s: killing job...\n", orterun_basename);
    abort_msg_len = strlen(abort_msg);

    /* Check for some "global" command line params */

    parse_globals(argc, argv);

    /* If we're still here, parse each app */

    parse_locals(argc, argv);

    /* Convert the list of apps to an array of orte_app_context_t
       pointers */

    num_apps = orte_pointer_array_get_size(apps_pa);
    if (0 == num_apps) {
        /* This should never happen -- this case should be caught in
           create_app(), but let's just double check... */
        ompi_show_help("help-orterun.txt", "orterun:nothing-to-do", 
                       true, orterun_basename);
        exit(1);
    }
    apps = malloc(sizeof(orte_app_context_t *) * num_apps);
    if (NULL == apps) {
        ompi_show_help("help-orterun.txt", "orterun:syscall-failed", 
                       true, orterun_basename, "malloc returned NULL", errno);
        exit(1);
    }
    for (j = i = 0; i < num_apps; ++i) {
        apps[i] = (orte_app_context_t *) 
            orte_pointer_array_get_item(apps_pa, i);
        j += apps[i]->num_procs;
    }
    proc_infos = malloc(sizeof(struct proc_info_t) * j);
    if (NULL == proc_infos) {
        ompi_show_help("help-orterun.txt", "orterun:syscall-failed", 
                       true, orterun_basename, "malloc returned NULL", errno);
        exit(1);
    }
    for (i = 0; i < j; ++i) {
        proc_infos[i].reported = false;
        proc_infos[i].exit_status = 0;
    }

    /* Intialize our Open RTE environment */
    /* first, set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    id = mca_base_param_register_int("orte", "base", "infrastructure", NULL, 0);
    mca_base_param_set_int(id, 1);

    /* now call orte_init and setup the RTE */
    if (ORTE_SUCCESS != (rc = orte_init())) {
        ompi_show_help("help-orterun.txt", "orterun:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

    /* check for daemon flags and push them into the environment
     * since this isn't being automatically done
     */
    id = mca_base_param_register_int("orte","debug","daemons",NULL,0);
    mca_base_param_lookup_int(id,&iparam);
    if (iparam) {
        if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_orte_debug_daemons",
                                              "1", true, &environ))) {
            fprintf(stderr, "orterun: could not set orte_debug_daemons in environ\n");
            return rc;
        }
    }
    id = mca_base_param_register_int("orte","debug",NULL,NULL,0);
    mca_base_param_lookup_int(id,&iparam);
    if (iparam) {
        if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_orte_debug",
                                              "1", true, &environ))) {
            fprintf(stderr, "orterun: could not set orte_debug in environ\n");
            return rc;
        }
    }
    id = mca_base_param_register_int("orte","debug","daemons_file",NULL,0);
    mca_base_param_lookup_int(id,&iparam);
    if (iparam) {
        if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_orte_debug_daemons_file",
                                              "1", true, &environ))) {
            fprintf(stderr, "orterun: could not set orte_debug_daemons_file in environ\n");
            return rc;
        }
    }
    id = mca_base_param_register_int("orte","no_daemonize",NULL,NULL,0);
    mca_base_param_lookup_int(id,&iparam);
    if (iparam) {
        if (ORTE_SUCCESS != (rc = ompi_setenv("OMPI_MCA_orte_no_daemonize",
                                              "1", true, &environ))) {
            fprintf(stderr, "orterun: could not set orte_no_daemonize in environ\n");
            return rc;
        }
    }
    
     /* Prep to start the application */

    opal_event_set(&term_handler, SIGTERM, OPAL_EV_SIGNAL,
                   signal_callback, NULL);
    opal_event_add(&term_handler, NULL);
    opal_event_set(&int_handler, SIGINT, OPAL_EV_SIGNAL,
                   signal_callback, NULL);
    opal_event_add(&int_handler, NULL);

    /* Spawn the job */
    
    rc = orte_rmgr.spawn(apps, num_apps, &jobid, job_state_callback);
    if (ORTE_SUCCESS != rc) {
        /* JMS show_help */
        ompi_output(0, "%s: spawn failed with errno=%d\n", orterun_basename, rc);
    } else {
        /* Wait for the app to complete */

        if (wait_for_job_completion) {
            OPAL_THREAD_LOCK(&orterun_globals.lock);
            while (!orterun_globals.exit) {
                opal_condition_wait(&orterun_globals.cond, 
                                    &orterun_globals.lock);
            }
            /* Make sure we propagate the exit code */
            if (WIFEXITED(orterun_globals.exit_status)) {
                rc = WEXITSTATUS(orterun_globals.exit_status);
            } else {
                rc = WTERMSIG(orterun_globals.exit_status);
            }
            OPAL_THREAD_UNLOCK(&orterun_globals.lock);

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
    }

    /* All done */
    for (i = 0; i < num_apps; ++i) {
        OBJ_RELEASE(apps[i]);
    }
    free(apps);
    OBJ_RELEASE(apps_pa);
    orte_finalize();
    free(abort_msg);
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
    size_t i, k, num_values = 0;
    int rc;
    int32_t exit_status = 0;
    bool exit_status_set;
    char *keys[] = {
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_PID_KEY,
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_EXIT_CODE_KEY,
        ORTE_NODE_NAME_KEY,
        NULL
    };

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
        orte_process_name_t name;
        pid_t pid = 0;
        size_t rank = 0;
        bool rank_found=false;
        char* node_name = NULL;

        exit_status = 0;
        exit_status_set = false;
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = value->keyvals[k];
            if(strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                name = keyval->value.proc;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_PID_KEY) == 0) {
                pid = keyval->value.pid;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                rank_found = true;
                rank = keyval->value.size;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_EXIT_CODE_KEY) == 0) {
                exit_status = keyval->value.exit_code;
                exit_status_set = true;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                node_name = keyval->value.strptr;
                continue;
            }
        }
        if (rank_found && exit_status_set) {
            proc_infos[rank].exit_status = exit_status;
        }

        if (WIFSIGNALED(exit_status) && rank_found && 
            !proc_infos[rank].reported) { 
            proc_infos[rank].reported = true;

            if (9 == WTERMSIG(exit_status)) {
                ++num_killed;
            } else {
                if (num_aborted < max_display_aborted) {
                    fprintf(stderr, "Job rank %lu (pid %lu) on node \"%s\" exited on signal %d\n",
                            (unsigned long)rank, (unsigned long)pid, node_name, WTERMSIG(exit_status));
                }
                ++num_aborted;
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
}


/*
 * signal main thread when application completes
 */

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state)
{
    OPAL_THREAD_LOCK(&orterun_globals.lock);

    /* Note that there's only two states that we're interested in
       here:

       ABORTED: which means that one or more processes have aborted
                (terminated abnormally).  In which case, we probably
                want to print out some information.

       TERMINATED: which means that all the processes in the job have
                completed (normally and/or abnormally).

       Remember that the rmgr itself will also be called for the
       ABORTED state and call the pls.terminate_job, which will result
       in killing all the other processes. */
    
    switch(state) {
        case ORTE_PROC_STATE_ABORTED:
            dump_aborted_procs(jobid);
            orte_rmgr.terminate_job(jobid);
            break;

        case ORTE_PROC_STATE_TERMINATED:
            dump_aborted_procs(jobid);
            orterun_globals.exit = true;
            opal_condition_signal(&orterun_globals.cond);
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
    fprintf(stderr, "%s: abnormal exit\n", orterun_basename);
    exit(1);
}


/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted. 
 */

static void signal_callback(int fd, short flags, void *arg)
{
    int ret;
    struct timeval tv = { 5, 0 };
    opal_event_t* event;

    static int signalled = 0;
    if (0 != signalled++) {
         return;
    }
    write(2, abort_msg, abort_msg_len);

    if (jobid != ORTE_JOBID_MAX) {
        ret = orte_rmgr.terminate_job(jobid);
        if (ORTE_SUCCESS != ret) {
            jobid = ORTE_JOBID_MAX;
        }
    }

    if (NULL != (event = (opal_event_t*)malloc(sizeof(opal_event_t)))) {
        opal_evtimer_set(event, exit_callback, NULL);
        opal_evtimer_add(event, &tv);
    }
}


static int init_globals(void) 
{
    struct globals_t tmp = {
        false,
        false,
        false,
        false,
        false,
        false,
        true,
        0,
        0,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL
    };

    /* Only CONSTRUCT things once */
    
    if (!globals_init) {
        OBJ_CONSTRUCT(&orterun_globals.lock, opal_mutex_t);
        OBJ_CONSTRUCT(&orterun_globals.cond, opal_condition_t);
    }

    /* Reset this every time */

    orterun_globals = tmp;

    /* All done */

    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[])
{
    ompi_cmd_line_t cmd_line;
    int ras;

    /* Setup and parse the command line */

    init_globals();
    ompi_cmd_line_create(&cmd_line, cmd_line_init);
    ompi_cmd_line_parse(&cmd_line, true, argc, argv);

    /* Check for help and version requests */

    if (1 == argc || orterun_globals.help) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(&cmd_line);
        ompi_show_help("help-orterun.txt", "orterun:usage", false,
                       orterun_basename, args);
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    if (orterun_globals.version) {
        printf("Open MPI v%s\n", OMPI_VERSION);

        /* If someone asks for version, that should be all we do */
        exit(0);
    }

    /* Allocate and map by node or by slot?  Shortcut for setting an
       MCA param. */

    /* JMS To be changed post-beta to LAM's C/N command line notation */
    ras = mca_base_param_register_string("ras", "base", "schedule_policy",
                                         NULL, "slot");
    if (orterun_globals.by_node) {
        orterun_globals.by_slot = false;
        mca_base_param_set_string(ras, "node");
    } else {
        orterun_globals.by_slot = true;
        mca_base_param_set_string(ras, "slot");
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
    char **temp_argv;
    orte_app_context_t *app;
    bool made_app;
    char **env;

    /* Make the apps */

    temp_argc = 0;
    temp_argv = NULL;
    ompi_argv_append(&temp_argc, &temp_argv, argv[0]);
    orte_pointer_array_init(&apps_pa, 1, argc + 1, 2);

    env = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            
            /* Make an app with this argv */

            if (ompi_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    ompi_argv_free(env);
                    env = NULL;
                }

                app = OBJ_NEW(orte_app_context_t);
                rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    size_t dummy;
                    orte_pointer_array_add(&dummy, apps_pa, app);
                } else {
                    OBJ_RELEASE(app);
                }
            
                /* Reset the temps */
            
                temp_argc = 0;
                temp_argv = NULL;
                ompi_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            ompi_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (ompi_argv_count(temp_argv) > 1) {
        app = OBJ_NEW(orte_app_context_t);
        rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            size_t dummy;
            orte_pointer_array_add(&dummy, apps_pa, app);
        } else {
            OBJ_RELEASE(app);
        }
    }
    if (NULL != env) {
        ompi_argv_free(env);
    }
    ompi_argv_free(temp_argv);

    /* All done */

    return ORTE_SUCCESS;
}


static int create_app(int argc, char* argv[], orte_app_context_t **app_ptr,
                      bool *made_app, char ***env)
{
    ompi_cmd_line_t cmd_line;
    char cwd[OMPI_PATH_MAX];
    int i, j, rc;
    char *param, *value, *value2;
    orte_app_context_t *app = NULL;
    extern char **environ;
    size_t l, len;
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
        if (0 == strcmp(argv[i], "C") ||
            0 == strcmp(argv[i], "N")) {
            map_data = true;
        } 

        /* Huersitic: if the string fits "[cn][0-9]+" or [cn][0-9],",
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

        /* Save -arch args */

        else if (0 == strcmp("-arch", argv[i])) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_ARCH, '\0' };

            ompi_argv_append(&new_argc, &new_argv, "-rawmap");
            ompi_argv_append(&new_argc, &new_argv, str);
            save_arg = false;
        }

        /* Save -host args */

        else if (0 == strcmp("-host", argv[i])) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_HOSTNAME, '\0' };

            ompi_argv_append(&new_argc, &new_argv, "-rawmap");
            ompi_argv_append(&new_argc, &new_argv, str);
            save_arg = false;
        }

        /* If this token was C/N map data, save it */

        if (map_data) {
            char str[2] = { '0' + ORTE_APP_CONTEXT_MAP_CN, '\0' };

            ompi_argv_append(&new_argc, &new_argv, "-rawmap");
            ompi_argv_append(&new_argc, &new_argv, str);
        }

        if (save_arg) {
            ompi_argv_append(&new_argc, &new_argv, argv[i]);
        }
    }

    /* Parse application command line options.  Add the -rawmap option
       separately so that the user doesn't see it in the --help
       message. */

    init_globals();
    ompi_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    cmd_line_made = true;
    ompi_cmd_line_make_opt3(&cmd_line, '\0', NULL, "rawmap", 2,
                            "Hidden / internal parameter -- users should not use this!");
    rc = ompi_cmd_line_parse(&cmd_line, true, new_argc, new_argv);
    ompi_argv_free(new_argv);
    new_argv = NULL;
    if (OMPI_SUCCESS != rc) {
        goto cleanup;
    }
    mca_base_cmd_line_process_args(&cmd_line, env);

    /* Is there an appfile in here? */

    if (NULL != orterun_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(orterun_globals.appfile), env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    ompi_cmd_line_get_tail(&cmd_line, &app->argc, &app->argv);

    /* See if we have anything left */

    if (0 == app->argc) {
        ompi_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, orterun_basename, orterun_basename);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Grab all OMPI_* environment variables */

    app->env = ompi_argv_copy(*env);
    app->num_env = ompi_argv_count(*env);
    for (i = 0; NULL != environ[i]; ++i) {
        if (0 == strncmp("OMPI_", environ[i], 5)) {
            ompi_argv_append_nosize(&app->env, environ[i]);
        }
    }

    /* Did the user request to export any environment variables? */

    if (ompi_cmd_line_is_taken(&cmd_line, "x")) {
        j = ompi_cmd_line_get_ninsts(&cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = ompi_cmd_line_get_param(&cmd_line, "x", i, 0);

            if (NULL != strchr(param, '=')) {
                ompi_argv_append_nosize(&app->env, param);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        ompi_argv_append_nosize(&app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", param, value);
                        ompi_argv_append_nosize(&app->env, value2);
                        free(value2);
                    }
                } else {
                    ompi_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
    }
    app->num_env = ompi_argv_count(app->env);

    /* Did the user request a specific path? */

    if (NULL != orterun_globals.path) {
        asprintf(&value, "PATH=%s", orterun_globals.path);
        ompi_argv_append_nosize(&app->env, value);
        free(value);
    }

    /* Did the user request a specific wdir? */

    if (NULL != orterun_globals.wdir) {
        app->cwd = strdup(orterun_globals.wdir);
    } else {
        getcwd(cwd, sizeof(cwd));
        app->cwd = strdup(cwd);
    }

    /* Did the user request any mappings?  They were all converted to
       --rawmap items, above. */

    if (ompi_cmd_line_is_taken(&cmd_line, "rawmap")) {
        j = ompi_cmd_line_get_ninsts(&cmd_line, "rawmap");
        app->map_data = malloc(sizeof(orte_app_context_map_t*) * j);
        if (NULL == app->map_data) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        app->num_map = j;
        for (i = 0; i < j; ++i) {
            app->map_data[i] = NULL;
        }
        for (i = 0; i < j; ++i) {
            value = ompi_cmd_line_get_param(&cmd_line, "rawmap", i, 0);
            value2 = ompi_cmd_line_get_param(&cmd_line, "rawmap", i, 1);
            app->map_data[i] = OBJ_NEW(orte_app_context_map_t);
            if (NULL == app->map_data[i]) {
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                goto cleanup;
            }
            app->map_data[i]->map_type = value[0] - '0';
            app->map_data[i]->map_data = strdup(value2);
        }
    }

    /* Get the numprocs */

    app->num_procs = orterun_globals.num_procs;

    /* If the user didn't specify a num procs or any map data, then we
       really have no idea what the launch... */

    if (app->num_procs == 0 && !map_data) {
        ompi_show_help("help-orterun.txt", "orterun:num-procs-unspecified",
                       true, orterun_basename, app->argv[0]);
        rc = ORTE_ERR_BAD_PARAM;
        goto cleanup;
    }

    /* Find the argv[0] in the path, but only if not absolute or
       relative pathname was specified */

    value = ompi_basename(app->argv[0]);
    if (strlen(value) == strlen(app->argv[0])) {
        app->app = ompi_path_findv(app->argv[0], 0, environ, app->cwd); 
    } else {
        app->app = strdup(app->argv[0]);
    }
    free(value);

    if (NULL == app->app) {
        ompi_show_help("help-orterun.txt", "orterun:executable-not-found",
                       true, orterun_basename, app->argv[0], orterun_basename);
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
        ompi_argv_free(new_argv);
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
    int rc, argc;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /* Try to open the file */

    fp = fopen(filename, "r");
    if (NULL == fp) {
        ompi_show_help("help-orterun.txt", "orterun:appfile-not-found", true,
                       filename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Read in line by line */

    line[sizeof(line) - 1] = '\0';
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

        argv = ompi_argv_split(line, ' ');
        argc = ompi_argv_count(argv);
        if (argc > 0) {

            /* Create a temporary env to play with in the recursive
               call -- that is: don't disturb the original env so that
               we can have a consistent global env */

            if (NULL != *env) {
                tmp_env = ompi_argv_copy(*env);
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
                ompi_argv_free(tmp_env);
            }
            if (made_app) {
                size_t dummy;
                orte_pointer_array_add(&dummy, apps_pa, app);
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}
