/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "include/orte_constants.h"

#include "event/event.h"
#include "class/ompi_pointer_array.h"
#include "util/proc_info.h"
#include "util/argv.h"
#include "util/path.h"
#include "util/cmd_line.h"
#include "util/sys_info.h"
#include "util/output.h"
#include "util/universe_setup_file_io.h"
#include "util/show_help.h"
#include "threads/condition.h"

#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rmgr/rmgr.h"

#include "runtime/runtime.h"
#include "runtime/orte_wait.h"


/*
 * The environment
 */
extern char** environ;

/*
 * Globals
 */
static struct ompi_event term_handler;
static struct ompi_event int_handler;
static orte_jobid_t jobid = ORTE_JOBID_MAX;
static ompi_pointer_array_t apps_pa;
static bool wait_for_job_completion = true;

/*
 * setup globals for catching orterun command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool exit;
    bool no_wait_for_job_completion;
    bool debug;
    int num_procs;
    char *hostfile;
    char *env_val;
    char *appfile;
    char *wdir;
    char *path;
    ompi_mutex_t lock;
    ompi_condition_t cond;
} orterun_globals;


ompi_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orterun_globals.help, OMPI_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, '\0', NULL, "version", 0,
      &orterun_globals.version, OMPI_CMD_LINE_TYPE_BOOL,
      "Show the orterun version" },
    { "orte", "debug", NULL, 'd', NULL, "debug", 0,
      &orterun_globals.debug, OMPI_CMD_LINE_TYPE_BOOL,
      "Enable debugging" },
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
      &orterun_globals.num_procs, OMPI_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &orterun_globals.num_procs, OMPI_CMD_LINE_TYPE_INT,
      "Number of processes to run" },

    /* Set a hostfile */
    { "hostfile", NULL, NULL, '\0', NULL, "hostfile", 1,
      &orterun_globals.num_procs, OMPI_CMD_LINE_TYPE_INT,
      "Provide a hostfile" },

    /* Don't wait for the process to finish before exiting */
    { NULL, NULL, NULL, '\0', "nw", "nw", 0,
      &orterun_globals.no_wait_for_job_completion, OMPI_CMD_LINE_TYPE_BOOL,
      "Launch the processes and do not wait for their completion (i.e., let orterun complete as soon a successful launch occurs)" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OMPI_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Specific mapping (C, cX, N, nX) */
    { NULL, NULL, NULL, '\0', NULL, "map", 1,
      NULL, OMPI_CMD_LINE_TYPE_STRING,
      "Mapping of processes to nodes / CPUs" },

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
                      bool *made_app);
static int init_globals(void);
static int parse_globals(int argc, char* argv[]);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename);
static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);


int main(int argc, char *argv[], char* env[])
{
    orte_app_context_t **apps;
    int rc, i, num_apps;

    /* Check for some "global" command line params */

    parse_globals(argc, argv);

    /* If we're still here, parse each app */

    parse_locals(argc, argv);

    /* Convert the list of apps to an array of orte_app_context_t
       pointers */

    num_apps = ompi_pointer_array_get_size(&apps_pa);
    apps = malloc(sizeof(orte_app_context_t *) * num_apps);
    if (NULL == apps) {
        /* JMS show_help */
        ompi_output(0, "orterun: malloc failed");
        exit(1);
    }
    for (i = 0; i < num_apps; ++i) {
        apps[i] = (orte_app_context_t *) 
            ompi_pointer_array_get_item(&apps_pa, i);
    }

    /* Intialize our Open RTE environment */

    if (ORTE_SUCCESS != (rc = orte_init())) {
        ompi_show_help("help-orterun.txt", "orterun:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

     /* Prep to start the application */

    ompi_event_set(&term_handler, SIGTERM, OMPI_EV_SIGNAL,
                   signal_callback, NULL);
    ompi_event_add(&term_handler, NULL);
    ompi_event_set(&int_handler, SIGINT, OMPI_EV_SIGNAL,
                   signal_callback, NULL);
    ompi_event_add(&int_handler, NULL);

    /* Spawn the job */

    rc = orte_rmgr.spawn(apps, num_apps, &jobid, job_state_callback);
    if (ORTE_SUCCESS != rc) {
        /* JMS show_help */
        ompi_output(0, "orterun: spawn failed with errno=%d\n", rc);
    } else {
        /* Wait for the app to complete */

        if (wait_for_job_completion) {
            OMPI_THREAD_LOCK(&orterun_globals.lock);
            while (!orterun_globals.exit) {
                ompi_condition_wait(&orterun_globals.cond, 
                                    &orterun_globals.lock);
            }
            OMPI_THREAD_UNLOCK(&orterun_globals.lock);
        }
    }

    /* All done */
    for (i = 0; i < num_apps; ++i) {
        OBJ_RELEASE(apps[i]);
    }
    free(apps);
    OBJ_DESTRUCT(&apps_pa);
    orte_finalize();
    return rc;
}


/*
 * signal main thread when application completes
 */

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state)
{
    OMPI_THREAD_LOCK(&orterun_globals.lock);
    switch(state) {
        case ORTE_PROC_STATE_TERMINATED:
        case ORTE_PROC_STATE_ABORTED:
            orterun_globals.exit = true;
            ompi_condition_signal(&orterun_globals.cond);
            break;
    }
    OMPI_THREAD_UNLOCK(&orterun_globals.lock);
}

static void exit_callback(int fd, short event, void *arg)
{
    fprintf(stderr, "orterun: abnormal exit\n");
    exit(1);
}


static void signal_callback(int fd, short flags, void *arg)
{
    int ret;
    struct timeval tv = { 5, 0 };
    ompi_event_t* event;

    static int signalled = 0;
    printf("in orterun signal_callback\n");
    if (0 != signalled++) {
        printf("exiting gratuitious callback\n");
         return;
    }

    if (jobid != ORTE_JOBID_MAX) {
        ret = orte_rmgr.terminate_job(jobid);
        if (ORTE_SUCCESS != ret) {
            jobid = ORTE_JOBID_MAX;
        }
    }

    if (NULL != (event = (ompi_event_t*)malloc(sizeof(ompi_event_t)))) {
        ompi_evtimer_set(event, exit_callback, NULL);
        ompi_evtimer_add(event, &tv);
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
        -1,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL
    };

    orterun_globals = tmp;
    OBJ_CONSTRUCT(&orterun_globals.lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&orterun_globals.cond, ompi_condition_t);
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[])
{
    ompi_cmd_line_t cmd_line;

    /* Setup and parse the command line */

    init_globals();
    ompi_cmd_line_create(&cmd_line, cmd_line_init);
    ompi_cmd_line_parse(&cmd_line, true, argc, argv);

    /* Check for help and version requests */

    if (orterun_globals.help) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(&cmd_line);
        ompi_show_help("help-orterun.txt", "orterun:usage", false,
                       argv[0], args);
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    if (orterun_globals.version) {
        printf("Open MPI v%s\n", OMPI_VERSION);

        /* If someone asks for version, that should be all we do */
        exit(0);
    }

    /* If we don't want to wait, we don't want to wait */

    if (orterun_globals.no_wait_for_job_completion) {
        wait_for_job_completion = false;
    }

    /* debug */
    if (orterun_globals.debug) {
        int id = mca_base_param_register_int("debug",NULL,NULL,NULL,0);
        mca_base_param_set_int(id,orterun_globals.debug);
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

    /* Make the apps */

    temp_argc = 0;
    temp_argv = NULL;
    ompi_argv_append(&temp_argc, &temp_argv, argv[0]);
    OBJ_CONSTRUCT(&apps_pa, ompi_pointer_array_t);
    
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            
            /* Make an app with this argv */

            if (ompi_argv_count(temp_argv) > 1) {
                app = OBJ_NEW(orte_app_context_t);
                rc = create_app(temp_argc, temp_argv, &app, &made_app);
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    ompi_pointer_array_add(&apps_pa, app);
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
        rc = create_app(temp_argc, temp_argv, &app, &made_app);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            ompi_pointer_array_add(&apps_pa, app);
        }
    }
    ompi_argv_free(temp_argv);

    /* All done */

    return ORTE_SUCCESS;
}


static int create_app(int argc, char* argv[], orte_app_context_t **app_ptr,
                      bool *made_app)
{
    ompi_cmd_line_t cmd_line;
    char cwd[OMPI_PATH_MAX];
    int i, j, rc;
    char *param, *value, *value2;
    orte_app_context_t *app = NULL;
    extern char **environ;
    size_t l, len;
    bool map_data, save_arg, cmd_line_made = false;
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
    mca_base_cmd_line_process_args(&cmd_line);

    /* Is there an appfile in here? */

    if (NULL != orterun_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(orterun_globals.appfile));
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    ompi_cmd_line_get_tail(&cmd_line, &app->argc, &app->argv);

    /* See if we have anything left */

    if (0 == app->argc) {
        ompi_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, argv[0], argv[0]);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Grab all OMPI_MCA_* environment variables */

    app->env = NULL;
    app->num_env = 0;
    for (i = 0; NULL != environ[i]; ++i) {
        if (0 == strncmp("OMPI_MCA_", environ[i], 9)) {
            ompi_argv_append(&app->num_env, &app->env, environ[i]);
        }
    }

    /* Did the user request to export any environment variables? */

    if (ompi_cmd_line_is_taken(&cmd_line, "x")) {
        j = ompi_cmd_line_get_ninsts(&cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = ompi_cmd_line_get_param(&cmd_line, "x", i, 0);

            if (NULL != strchr(param, '=')) {
                ompi_argv_append(&app->num_env, &app->env, param);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        ompi_argv_append(&app->num_env, &app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", param, value);
                        ompi_argv_append(&app->num_env, &app->env, value2);
                    }
                } else {
                    ompi_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
            free(param);
        }
    }

    /* Did the user request a specific path? */

    if (NULL != orterun_globals.path) {
        asprintf(&value, "PATH=%s", orterun_globals.path);
        ompi_argv_append(&app->num_env, &app->env, value);
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
    /* JMS This may not be a valid assumption -- e.g., mpirun C foo */
    if (0 == app->num_procs) {
        app->num_procs = 1; 
    }

    /* Find the argv[0] in the path */

    app->app = ompi_path_findv(app->argv[0], 0, environ, app->cwd); 
    if (NULL == app->app) {
        ompi_show_help("help-orterun.txt", "orterun:executable-not-found",
                       true, argv[0], app->argv[0], argv[0]);
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


static int parse_appfile(char *filename)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";

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

        /* Remove comments */

        len = strlen(line);
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
            rc = create_app(argc, argv, &app, &made_app);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (made_app) {
                ompi_pointer_array_add(&apps_pa, app);
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}
