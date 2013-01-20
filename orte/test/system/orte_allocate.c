/* -*- C -*-
 *
 * $HEADER$
 *
 * Take cmd-line arguments and execute a dynamic allocation
 *
 * NOTE: this program is executed as a standalone program since
 * it emulates mpirun!
 *
 * Usage:  orte_allocate -np 3 -N 1 -host foo,bar : -np 1 -host bee -mandatory
*
 */

#include <stdio.h>

#include "opal/mca/if/if.h"
#include "opal/util/argv.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"

#include "orte/util/proc_info.h"
#include "orte/mca/plm/plm.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/ras/ras.h"

struct my_globals_t {
    bool help;
    int num_procs;
    int min_num_nodes;
    bool mandatory;
};
struct my_globals_t my_globals;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, 'h', NULL, "help", 0,
      &my_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, 'c', "np", "np", 1,
      &my_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, '\0', "n", "n", 1,
      &my_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },

    /* Minimum number of nodes */
    { NULL, '\0', "N", "N", 1,
      &my_globals.min_num_nodes, OPAL_CMD_LINE_TYPE_INT,
      "Minimum number of nodes for this app" },

    /* the hosts to use for this app */
    { NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

    /* These hosts are mandatory */
    { NULL, '\0', NULL, "mandatory", 0,
      &my_globals.mandatory, OPAL_CMD_LINE_TYPE_BOOL,
      "The hosts are mandatory (default: optional)" },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

static void init_globals(void)
{
    my_globals.help = false;
    my_globals.num_procs = 0;
    my_globals.min_num_nodes = -1;
    my_globals.mandatory = false;
}

int main(int argc, char* argv[])
{
    int rc;
    opal_cmd_line_t cmd_line;
    orte_job_t *jdata;
    orte_app_context_t *app;
    int temp_argc;
    char **temp_argv, *value;
    opal_cmd_line_t tmp_cmd_line;
    int i, j, k, app_num;

    /* bozo check - we don't allow recursive calls of orterun */
    if (NULL != getenv("OMPI_UNIVERSE_SIZE")) {
        fprintf(stderr, "\n\n**********************************************************\n\n");
        fprintf(stderr, "This program is a standalone program and cannot be run via mpirun\n");
        fprintf(stderr, "\n**********************************************************\n");
        exit(1);
    }

    /* Setup and parse the command line */
    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* Need to initialize OPAL so that install_dirs are filled in */
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        fprintf(stderr, "Error in opal_init_util\n");
        exit(1);
    }

    if (my_globals.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orterun.txt", "orterun:usage", false,
                                    "orte_allocate", "orte_allocate", "ignore",
                                    "orte_allocate", args, "ignore");
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);
        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    mca_base_cmd_line_process_args(&cmd_line, &environ, &environ);

    /* create the job object */
    jdata = OBJ_NEW(orte_job_t);

    /* collect the apps */
    temp_argc = 0;
    temp_argv = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            fprintf(stderr, "Creating app\n");
            /* Make an app with this argv */
            app = OBJ_NEW(orte_app_context_t);
            opal_cmd_line_create(&tmp_cmd_line, cmd_line_init);
            mca_base_cmd_line_setup(&tmp_cmd_line);
            rc = opal_cmd_line_parse(&tmp_cmd_line, true, argc, argv);
            if (ORTE_SUCCESS != rc) {
                fprintf(stderr, "Error processing app\n");
                exit(1);
            }
            mca_base_cmd_line_process_args(&tmp_cmd_line, &app->env, &environ);
            opal_pointer_array_add(jdata->apps, app);
            app->idx = jdata->num_apps;
            ++jdata->num_apps;
            /* fake an app executable - we don't care */
            app->app = strdup("fakeapp");
            opal_argv_append_nosize(&app->argv, "fakeapp");
            /* set the num_procs */
            app->num_procs = my_globals.num_procs;
            /* Did the user specify any hosts? */
            if (0 < (j = opal_cmd_line_get_ninsts(&tmp_cmd_line, "host"))) {
                for (k = 0; k < j; ++k) {
                    value = opal_cmd_line_get_param(&tmp_cmd_line, "host", k, 0);
                    opal_argv_append_nosize(&app->dash_host, value);
                }
            }
            /* set the minimum number of nodes */
            app->min_number_of_nodes = my_globals.min_num_nodes;
            /* set the mandatory flag */
            app->mandatory = my_globals.mandatory;
            opal_dss.dump(0, app, ORTE_APP_CONTEXT);
        }
        opal_argv_append(&temp_argc, &temp_argv, argv[i]);
    }
    if (opal_argv_count(temp_argv) > 1) {
            fprintf(stderr, "Creating app\n");
            /* Make an app with this argv */
            app = OBJ_NEW(orte_app_context_t);
            opal_cmd_line_create(&tmp_cmd_line, cmd_line_init);
            mca_base_cmd_line_setup(&tmp_cmd_line);
            rc = opal_cmd_line_parse(&tmp_cmd_line, true, argc, argv);
            if (ORTE_SUCCESS != rc) {
                fprintf(stderr, "Error processing app\n");
                exit(1);
            }
            mca_base_cmd_line_process_args(&tmp_cmd_line, &app->env, &environ);
            opal_pointer_array_add(jdata->apps, app);
            app->idx = jdata->num_apps;
            ++jdata->num_apps;
            /* fake an app executable - we don't care */
            app->app = strdup("fakeapp");
            opal_argv_append_nosize(&app->argv, "fakeapp");
            /* set the num_procs */
            app->num_procs = my_globals.num_procs;
            /* Did the user specify any hosts? */
            if (0 < (j = opal_cmd_line_get_ninsts(&tmp_cmd_line, "host"))) {
                for (k = 0; k < j; ++k) {
                    value = opal_cmd_line_get_param(&tmp_cmd_line, "host", k, 0);
                    opal_argv_append_nosize(&app->dash_host, value);
                }
            }
            /* set the minimum number of nodes */
            app->min_number_of_nodes = my_globals.min_num_nodes;
            /* set the mandatory flag */
            app->mandatory = my_globals.mandatory;
    }

    /* flag that I am the HNP - needs to be done prior to
     * registering params
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;

    /* Setup MCA params */
    orte_register_params();

    /* flag that we do NOT want to launch anything - we just
     * want to test the allocator
     */
    orte_do_not_launch = true;
    /* do not resolve the hosts */
    opal_if_do_not_resolve = true;

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_HNP)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    /* finalize the OPAL utils. As they are opened again from orte_init->opal_init
     * we continue to have a reference count on them. So we have to finalize them twice...
     */
    opal_finalize_util();

    opal_dss.dump(0, jdata, ORTE_JOB);

    /* spawn the job and its daemons */
    rc = orte_plm.spawn(jdata);

    /* loop the event lib until an exit event is detected */
#if ORTE_ENABLE_PROGRESS_THREADS
    while (orte_event_base_active) {
        /* provide a very short quiet period so we
         * don't hammer the cpu while
         */
        struct timespec tp = {0, 100};
        nanosleep(&tp, NULL);
    }
#else
    while (orte_event_base_active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }
#endif

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
