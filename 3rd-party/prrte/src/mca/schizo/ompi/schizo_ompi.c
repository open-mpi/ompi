/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2017 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2017      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include <ctype.h>

#ifdef HAVE_SYS_UTSNAME_H
#    include <sys/utsname.h>
#endif

#include "src/util/pmix_argv.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/proc_info.h"
#include "src/util/prte_cmd_line.h"
#include "src/runtime/pmix_init_util.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/base/pmix_mca_base_vari.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/state/base/base.h"
#include "src/runtime/prte_globals.h"

#include "schizo_ompi.h"
#include "src/mca/schizo/base/base.h"

static int parse_cli(char **argv, pmix_cli_result_t *results, bool silent);
static int detect_proxy(char *argv);
static int parse_env(char **srcenv, char ***dstenv, pmix_cli_result_t *cli);
static void allow_run_as_root(pmix_cli_result_t *results);
static int set_default_ranking(prte_job_t *jdata,
                               prte_rmaps_options_t *options);
static void job_info(pmix_cli_result_t *results,
                     void *jobinfo);
static int setup_app(prte_pmix_app_t *app);
static int set_default_rto(prte_job_t *jdata,
                           prte_rmaps_options_t *options);

prte_schizo_base_module_t prte_schizo_ompi_module = {
    .name = "ompi",
    .parse_cli = parse_cli,
    .parse_env = parse_env,
    .setup_fork = prte_schizo_base_setup_fork,
    .setup_app = setup_app,
    .detect_proxy = detect_proxy,
    .allow_run_as_root = allow_run_as_root,
    .set_default_ranking = set_default_ranking,
    .job_info = job_info,
    .set_default_rto = set_default_rto,
    .check_sanity = prte_schizo_base_sanity
};

static struct option ompioptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PARSEABLE, PMIX_ARG_NONE, 'p'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PARSABLE, PMIX_ARG_NONE, 'p'), // synonym for parseable
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PERSONALITY, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DVM, PMIX_ARG_REQD),

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PRTEMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("omca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("gomca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_TUNE, PMIX_ARG_REQD),

    PMIX_OPTION_DEFINE(PRTE_CLI_LAUNCH_AGENT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_MAX_VM_SIZE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS_FILE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_LEAVE_SESSION_ATTACHED, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_TMPDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PREFIX, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NOPREFIX, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_SIGNALS, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_RUN_AS_ROOT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_CHILD_SEP, PMIX_ARG_NONE),

    /* debug options */
    PMIX_OPTION_DEFINE(PRTE_CLI_XTERM, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_ON_EXEC, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_INIT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_APP, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_STATE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STACK_TRACES, PMIX_ARG_NONE),
#ifdef PMIX_SPAWN_TIMEOUT
    PMIX_OPTION_DEFINE(PRTE_CLI_SPAWN_TIMEOUT, PMIX_ARG_REQD),
#endif
#ifdef PMIX_LOG_AGG
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_AGG_HELP, PMIX_ARG_NONE),
#endif

    /* Conventional options - for historical compatibility, support
     * both single and multi dash versions */
    /* Number of processes; -c, -n, --n, -np, and --np are all
     synonyms */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'n'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'c'),
    PMIX_OPTION_DEFINE("n", PMIX_ARG_REQD),  // will be converted to "np" after parsing
    PMIX_OPTION_SHORT_DEFINE("N", PMIX_ARG_REQD, 'N'),
    PMIX_OPTION_DEFINE(PRTE_CLI_APPFILE, PMIX_ARG_REQD),

    /* output options */
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT, PMIX_ARG_REQD),

    /* input options */
    PMIX_OPTION_DEFINE(PRTE_CLI_STDIN, PMIX_ARG_REQD),


    /* launch options */
    PMIX_OPTION_DEFINE(PRTE_CLI_PRELOAD_FILES, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PRELOAD_BIN, PMIX_ARG_NONE, 's'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_FWD_ENVAR, PMIX_ARG_REQD, 'x'),
    PMIX_OPTION_DEFINE(PRTE_CLI_WDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("wd", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PATH, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_SHOW_PROGRESS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_PSET, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("machinefile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEFAULT_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HOST, PMIX_ARG_REQD, 'H'),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_ENVIRON, PMIX_ARG_OPTIONAL),

    /* placement options */
    /* Mapping options */
    PMIX_OPTION_DEFINE(PRTE_CLI_MAPBY, PMIX_ARG_REQD),

    /* Ranking options */
    PMIX_OPTION_DEFINE(PRTE_CLI_RANKBY, PMIX_ARG_REQD),

    /* Binding options */
    PMIX_OPTION_DEFINE(PRTE_CLI_BINDTO, PMIX_ARG_REQD),

    /* display options */
    PMIX_OPTION_DEFINE(PRTE_CLI_DISPLAY, PMIX_ARG_REQD),

    /* developer options */
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_LAUNCH, PMIX_ARG_NONE),

    // Runtime options
    PMIX_OPTION_DEFINE(PRTE_CLI_RTOS, PMIX_ARG_REQD),


    PMIX_OPTION_DEFINE(PRTE_CLI_ENABLE_RECOVERY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_MAX_RESTARTS, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DISABLE_RECOVERY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_CONTINUOUS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("with-ft", PMIX_ARG_REQD),

    /* mpiexec mandated form launch key parameters */
    PMIX_OPTION_DEFINE("initial-errhandler", PMIX_ARG_REQD),

    /* Display Commumication Protocol : MPI_Init */
    PMIX_OPTION_DEFINE("display-comm", PMIX_ARG_NONE),

    /* Display Commumication Protocol : MPI_Finalize */
    PMIX_OPTION_DEFINE("display-comm-finalize", PMIX_ARG_NONE),

    // unsupported, but mandated options
    PMIX_OPTION_DEFINE("soft", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("arch", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("file", PMIX_ARG_REQD),

    // deprecated options
    PMIX_OPTION_DEFINE("mca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("gmca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("xml", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("tag-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("timestamp-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("output-directory", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("output-filename", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("merge-stderr-to-stdout", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-devel-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-topo", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("report-bindings", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-allocation", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("nolocal", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("oversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("nooversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("use-hwthread-cpus", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpu-set", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpu-list", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("bind-to-core", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bynode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bycore", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("byslot", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpus-per-proc", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpus-per-rank", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("npernode", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("pernode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("npersocket", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("ppr", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("amca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("am", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("rankfile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT_PROCTABLE, PMIX_ARG_OPTIONAL),
    PMIX_OPTION_DEFINE("debug", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("stream-buffering", PMIX_ARG_REQD),

    PMIX_OPTION_END
};
static char *ompishorts = "h::vVpn:c:N:sH:x:";
static char *ompi_install_dirs_libdir = NULL;

static int convert_deprecated_cli(pmix_cli_result_t *results,
                                  bool silent);

static void set_classpath_jar_file(prte_pmix_app_t *app, int index, char *jarfile)
{
    if (NULL == strstr(app->app.argv[index], jarfile)) {
        /* nope - need to add it */
        char *fmt = ':' == app->app.argv[index][strlen(app->app.argv[index]-1)]
                    ? "%s%s/%s" : "%s:%s/%s";
        char *str;
        asprintf(&str, fmt, app->app.argv[index], ompi_install_dirs_libdir, jarfile);
        free(app->app.argv[index]);
        app->app.argv[index] = str;
    }
}

/*
 * OMPI schizo setup_app is the place we prep for Java apps
 */

static int setup_app(prte_pmix_app_t *app)
{
    bool found;
    int i,rc;
    char *value;

    /* if this is a Java application, we have a bit more work to do. Such
     * applications actually need to be run under the Java virtual machine
     * and the "java" command will start the "executable". So we need to ensure
     * that all the proper java-specific paths are provided
     */

    if (0 != strcmp(app->app.argv[0], "java")) {
        return PRTE_SUCCESS;
    }

    ompi_install_dirs_libdir = getenv("OMPI_LIBDIR_LOC");
    if (NULL == ompi_install_dirs_libdir) {
        pmix_show_help("help-schizo-ompi.txt", "openmpi-install-path-not-found",1);
        return PRTE_ERR_NOT_AVAILABLE;
    }

    /* see if we were given a library path */
    found = false;
    for (i=1; NULL != app->app.argv[i]; i++) {
        if (NULL != strstr(app->app.argv[i], "java.library.path")) {
            char *dptr;
            /* find the '=' that delineates the option from the path */
            if (NULL == (dptr = strchr(app->app.argv[i], '='))) {
                /* that's just wrong */
                rc = PRTE_ERR_BAD_PARAM;
                goto cleanup;
            }
            /* step over the '=' */
            ++dptr;
            /* yep - but does it include the path to the mpi libs? */
            found = true;
            if (NULL == strstr(app->app.argv[i], ompi_install_dirs_libdir)) {
                /* doesn't appear to - add it to be safe */
                if (':' == app->app.argv[i][strlen(app->app.argv[i]-1)]) {
                    asprintf(&value, "-Djava.library.path=%s%s", dptr, ompi_install_dirs_libdir);
                } else {
                    asprintf(&value, "-Djava.library.path=%s:%s", dptr, ompi_install_dirs_libdir);
                }
                free(app->app.argv[i]);
                app->app.argv[i] = value;
            }
            break;
        }
    }

    if (!found) {
        /* need to add it right after the java command */
        asprintf(&value, "-Djava.library.path=%s", ompi_install_dirs_libdir);
        pmix_argv_insert_element(&app->app.argv, 1, value);
        free(value);
    }

    /* see if we were given a class path
     * See https://docs.oracle.com/javase/8/docs/technotes/tools/findingclasses.html
     * for more info about rules for the ways to set the class path
     */
    found = false;
    for (i=1; NULL != app->app.argv[i]; i++) {
        if (NULL != strstr(app->app.argv[i], "cp") ||
            NULL != strstr(app->app.argv[i], "classpath")) {
            /* yep - but does it include the path to the mpi libs? */
            found = true;
            /* check if mpi.jar exists - if so, add it */
            value = pmix_os_path(false, ompi_install_dirs_libdir, "mpi.jar", NULL);
            if (access(value, F_OK ) != -1) {
                set_classpath_jar_file(app, i+1, "mpi.jar");
            }
            free(value);
            /* always add the local directory */
            asprintf(&value, "%s:%s", app->app.cwd, app->app.argv[i+1]);
            free(app->app.argv[i+1]);
            app->app.argv[i+1] = value;
            break;
        }
    }

    if (!found) {
        /* check to see if CLASSPATH is in the environment */
        found = false;  // just to be pedantic
        for (i=0; NULL != environ[i]; i++) {
            if (0 == strncmp(environ[i], "CLASSPATH", strlen("CLASSPATH"))) {
                value = strchr(environ[i], '=');
                ++value; /* step over the = */
                pmix_argv_insert_element(&app->app.argv, 1, value);
                /* check for mpi.jar */
                value = pmix_os_path(false, ompi_install_dirs_libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, 1, "mpi.jar");
                }
                free(value);
                /* always add the local directory */
                (void)asprintf(&value, "%s:%s", app->app.cwd, app->app.argv[1]);
                free(app->app.argv[1]);
                app->app.argv[1] = value;
                pmix_argv_insert_element(&app->app.argv, 1, "-cp");
                found = true;
                break;
            }
        }

        if (!found) {
            /* need to add it right after the java command - have
             * to include the working directory and trust that
             * the user set cwd if necessary
             */
            char *str, *str2;
            /* always start with the working directory */
            str = strdup(app->app.cwd);
            /* check for mpi.jar */
            value = pmix_os_path(false, ompi_install_dirs_libdir, "mpi.jar", NULL);
            if (access(value, F_OK ) != -1) {
                (void)asprintf(&str2, "%s:%s", str, value);
                free(str);
                str = str2;
            }
            free(value);
            pmix_argv_insert_element(&app->app.argv, 1, str);
            free(str);
            pmix_argv_insert_element(&app->app.argv, 1, "-cp");
        }
    }

    return PRTE_SUCCESS;

cleanup:
    return rc;
}

static bool mcaoption(char *s)
{
    size_t len = strlen(s);

    if (3 > len) {
        return false;
    }
    if ('a' == s[len-1] &&
        'c' == s[len-2] &&
        'm' == s[len-3]) {
        return true;
    }
    return false;
}

static int parse_cli(char **argv, pmix_cli_result_t *results,
                     bool silent)
{
    int rc, m, n;
    pmix_cli_item_t *opt;
    char *p1, *p2;
    char **pargv;

    /* backup the argv */
    pargv = PMIX_ARGV_COPY_COMPAT(argv);

    char **caught_single_dashes = NULL;
    int  *caught_positions = NULL;
    int cur_caught_pos = 0;

    bool warn;

    if (silent) {
        warn = false;
    } else {
        warn = prte_mca_schizo_ompi_component.warn_deprecations;
    }

    if(warn) {
      int argc = 0;
      for(n=1; NULL != pargv[n]; n++) argc++;
      caught_single_dashes = calloc(argc + 1, sizeof(char *));
      caught_positions = calloc(argc + 1, sizeof(int));
    }

    /* Convert single dashes to multi-dashes. */
    for (n=1; NULL != pargv[n]; n++) {
        if (0 == strcmp("--", pargv[n])) {
            // quit processing
            break;
        }
        /* check for option */
        if ('-' != pargv[n][0]) {
            continue;
        }
        // if this is an mca spec, then skip it
        if (mcaoption(pargv[n])) {
            if ('-' != pargv[n][1]) {
                // make it a "--" option
                p2 = strdup(pargv[n]);
                free(pargv[n]);
                pmix_asprintf(&pargv[n], "-%s", p2);
                free(p2);
            }
            // now skip the next two positions
            n += 2;
            continue;
        }
        /* check for single-dash errors */
        if ('-' != pargv[n][1] && 2 < strlen(pargv[n])) {
            /* we know this is incorrect */
            char *p2 = pargv[n];
            pmix_asprintf(&pargv[n], "-%s", p2);
            if(warn) {
                caught_single_dashes[cur_caught_pos] = strdup(p2);
                caught_positions[cur_caught_pos++] = n;
            }
            free(p2);
        }
    }

    const char *tool_version = getenv("OMPI_VERSION");
    const char *tool_name    = getenv("OMPI_TOOL_NAME");
    // If the user is using prterun --personality ompi, these
    // won't be set, and thus this is not mpirun/mpiexec.
    if(tool_version && tool_name) {
        pmix_tool_version  = tool_version;
        pmix_tool_basename = tool_name;
        pmix_tool_org      = "Open MPI";
        pmix_tool_msg      = "Report bugs to https://www.open-mpi.org/community/help/";
    }

    rc = pmix_cmd_line_parse(pargv, ompishorts, ompioptions, NULL,
                             results, "help-schizo-ompi.txt");
    if (PMIX_SUCCESS != rc) {
        PMIX_ARGV_FREE_COMPAT(pargv);
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* pmix cmd line interpreter output result
             * successfully - usually means version or
             * some other stock output was generated */
            return PRTE_OPERATION_SUCCEEDED;
        }
        if(warn) {
            for(n = 0; n < cur_caught_pos; n++) {
                free(caught_single_dashes[n]);
            }
            free(caught_single_dashes);
            free(caught_positions);
        }
        return prte_pmix_convert_status(rc);
    }

    /*
     * If warning is enabled, list all offending
     * single dash params are before the last found
     * argument by the parser (results -> tail).
     *
     * The only case where tail should be NULL is
     * if the user didn't specify an executable on
     * the command line, and that should have been caught
     * earlier. Put a bozo check anyway.
     */
    if(warn && cur_caught_pos > 0 && results->tail) {
        char *orig_args = NULL;
        char *corrected_args = NULL;
        int tail_pos = 0;

        // Find the position of the tail.
        for(n = 0; NULL != pargv[n]; n++) {
            if(0 == strcmp(results->tail[0], pargv[n])) break;
        }
        tail_pos = n;

        for(n = 0; n < cur_caught_pos; n++) {
            // Add all offending arguments before the user executable (tail).
            if(caught_positions[n] < tail_pos) {
                // Multiple offending single dashes case. Append and free.
                if(orig_args && corrected_args) {
                    char *tmp = orig_args;
                    pmix_asprintf(&orig_args, "%s, %s", tmp, caught_single_dashes[n]);
                    free(tmp);
                    tmp = corrected_args;
                    pmix_asprintf(&corrected_args, "%s, -%s", tmp, caught_single_dashes[n]);
                    free(tmp);
                }
                else {
                    // First case.
                    pmix_asprintf(&orig_args, "%s", caught_single_dashes[n]);
                    pmix_asprintf(&corrected_args, "-%s", caught_single_dashes[n]);
                }
            }
            else {
                break;
            }
        }
        if(orig_args && corrected_args) {
            pmix_show_help("help-schizo-base.txt", "single-dash-error", true,
                            orig_args, corrected_args);
            free(orig_args);
            free(corrected_args);
        }
        for(n = 0; n < cur_caught_pos; n++) {
            free(caught_single_dashes[n]);
        }
        free(caught_single_dashes);
        free(caught_positions);
    }

    PMIX_ARGV_FREE_COMPAT(pargv);
    /* check for deprecated options - warn and convert them */
    rc = convert_deprecated_cli(results, silent);
    if (PRTE_SUCCESS != rc) {
        return rc;
    }

    // handle relevant MCA params
    PMIX_LIST_FOREACH(opt, &results->instances, pmix_cli_item_t) {
        if (0 == strcmp(opt->key, PRTE_CLI_PRTEMCA)) {
            for (n=0; NULL != opt->values[n]; n++) {
                p1 = opt->values[n];
                prte_schizo_base_expose(p1, "PRTE_MCA_");
            }
        } else if (0 == strcmp(opt->key, PRTE_CLI_PMIXMCA)) {
            for (n=0; NULL != opt->values[n]; n++) {
                p1 = opt->values[n];
                prte_schizo_base_expose(p1, "PMIX_MCA_");
            }
        } else if(0 == strcmp(opt->key, "mca") || 0 == strcmp(opt->key, "omca")) {
            for (n=0; NULL != opt->values[n]; n++) {
                p1 = opt->values[n];
                prte_schizo_base_expose(p1, "OMPI_MCA_");
            }
        }
    }

    if (NULL != results->tail) {
        /* search for the leader of the tail */
        for (n=0; NULL != argv[n]; n++) {
            if (0 == strcmp(results->tail[0], argv[n])) {
                /* this starts the tail - replace the rest of the
                 * tail with the original argv */
                PMIX_ARGV_FREE_COMPAT(results->tail);
                results->tail = PMIX_ARGV_COPY_COMPAT(&argv[n]);
                break;
            }
        }
    }

    return PRTE_SUCCESS;
}

static int convert_deprecated_cli(pmix_cli_result_t *results,
                                  bool silent)
{
    char *option, *p1, *p2, *tmp, *tmp2, *output;
    int rc = PRTE_SUCCESS;
    pmix_cli_item_t *opt, *nxt;
    bool warn;

    if (silent) {
        warn = false;
    } else {
        warn = prte_mca_schizo_ompi_component.warn_deprecations;
    }

    PMIX_LIST_FOREACH_SAFE(opt, nxt, &results->instances, pmix_cli_item_t) {
        option = opt->key;
        if (0 == strcmp(option, "n")) {
            /* if they passed a "--n" option, we need to convert it
             * back to the "--np" one without a deprecation warning */
            rc = prte_schizo_base_add_directive(results, option, PRTE_CLI_NP, opt->values[0], false);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --nolocal -> --map-by :nolocal */
        else if (0 == strcmp(option, "nolocal")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NOLOCAL,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --oversubscribe -> --map-by :OVERSUBSCRIBE */
        else if (0 == strcmp(option, "oversubscribe")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_OVERSUB,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --nooversubscribe -> --map-by :NOOVERSUBSCRIBE */
        else if (0 == strcmp(option, "nooversubscribe")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NOOVER,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --use-hwthread-cpus -> --map-by hwtcpus */
        else if (0 == strcmp(option, "use-hwthread-cpus")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_HWTCPUS,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
            if (NULL != prte_set_slots) {
                free(prte_set_slots);
            }
            prte_set_slots = strdup("hwthreads");
        }
        /* --do-not-launch -> --runtime-options do-not-launch */
        else if(0 == strcmp(option, "do-not-launch")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_RTOS, PRTE_CLI_NOLAUNCH,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --show-progress -> --runtime-options show-progress */
        else if(0 == strcmp(option, PRTE_CLI_SHOW_PROGRESS)) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_RTOS, PRTE_CLI_SHOW_PROGRESS,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --cpu-set and --cpu-list -> --map-by pe-list:X
         */
        else if (0 == strcmp(option, "cpu-set") || 0 == strcmp(option, "cpu-list")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_PELIST, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bind-to-core and --bind-to-socket -> --bind-to X */
        else if (0 == strcmp(option, "bind-to-core")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_BINDTO, PRTE_CLI_CORE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "bind-to-socket")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_BINDTO, PRTE_CLI_PACKAGE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bynode -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "bynode")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NODE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bycore -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "bycore")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_CORE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --byslot -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "byslot")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_SLOT,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --cpus-per-proc/rank X -> --map-by :pe=X */
        else if (0 == strcmp(option, "cpus-per-proc") || 0 == strcmp(option, "cpus-per-rank")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_PE, opt->values[0]);
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* -N ->   map-by ppr:N:node */
        else if (0 == strcmp(option, "N")) {
            pmix_asprintf(&p2, "ppr:%s:node", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --npernode X and --npersocket X -> --map-by ppr:X:node/socket */
        else if (0 == strcmp(option, "npernode")) {
            pmix_asprintf(&p2, "ppr:%s:node", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "pernode")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, "ppr:1:node",
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "npersocket")) {
            pmix_asprintf(&p2, "ppr:%s:package", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --ppr X -> --map-by ppr:X */
        else if (0 == strcmp(option, "ppr")) {
            /* if they didn't specify a complete pattern, then this is an error */
            if (NULL == strchr(opt->values[0], ':')) {
                pmix_show_help("help-schizo-base.txt", "bad-ppr", true, opt->values[0], true);
                return PRTE_ERR_SILENT;
            }
            pmix_asprintf(&p2, "ppr:%s", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --am[ca] X -> --tune X */
        else if (0 == strcmp(option, "amca") || 0 == strcmp(option, "am")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_TUNE, opt->values[0],
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --rankfile X -> map-by rankfile:file=X */
        else if (0 == strcmp(option, "rankfile")) {
            pmix_asprintf(&p2, "%s:%s%s", PRTE_CLI_RANKFILE, PRTE_CLI_QFILE, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option, PRTE_CLI_MAPBY, p2, true);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --tag-output  ->  "--output tag */
        else if (0 == strcmp(option, "tag-output")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_TAG,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --timestamp-output  ->  --output timestamp */
        else if (0 == strcmp(option, "timestamp-output")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_TIMESTAMP,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-directory DIR  ->  --output dir=DIR */
        else if (0 == strcmp(option, "output-directory")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_QDIR, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-filename DIR  ->  --output file=file */
        else if (0 == strcmp(option, "output-filename")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_QFILE, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* merge-stderr-to-stdout -> --output merge-stderr-to-stdout */
        else if (0 == strcmp(option, "merge-stderr-to-stdout")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_MERGE_ERROUT,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --xml  ->  --output xml */
        else if (0 == strcmp(option, "xml")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_XML,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-devel-map  -> --display allocation-devel */
        else if (0 == strcmp(option, "display-devel-map")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_MAPDEV,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-proctable  ->  --runtime-options output-proctable */
        else if (0 == strcmp(option, PRTE_CLI_OUTPUT_PROCTABLE)) {
            if (NULL != opt->values && NULL != opt->values[0]) {
                pmix_asprintf(&p2, "%s=%s", PRTE_CLI_OUTPUT_PROCTABLE, opt->values[0]);
            } else {
                p2 = strdup(PRTE_CLI_OUTPUT_PROCTABLE);
            }
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_RTOS, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-map  ->  --display map */
        else if (0 == strcmp(option, "display-map")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_MAP,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-topo  ->  --display topo */
        else if (0 == strcmp(option, "display-topo")) {
            pmix_asprintf(&p2, "%s=%s", "topo", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --report-bindings  ->  --display bind */
        else if (0 == strcmp(option, "report-bindings")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_BIND,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-allocation  ->  --display allocation */
        else if (0 == strcmp(option, "display-allocation")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_ALLOC,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --debug will be deprecated starting with open mpi v5
         */
        else if (0 == strcmp(option, "debug")) {
            if (warn) {
                pmix_show_help("help-schizo-base.txt", "deprecated-inform", true, option,
                               "This CLI option will be deprecated starting in Open MPI v5");
            }
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
       }
        /* --map-by socket ->  --map-by package */
        else if (0 == strcmp(option, PRTE_CLI_MAPBY)) {
            /* check the value of the option for "socket" */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup(PRTE_CLI_PACKAGE);
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "%s:%s", PRTE_CLI_PACKAGE, p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt", "deprecated-converted", true, p2,
                                                   tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(tmp2);
                    free(p2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }
        }
        /* --rank-by socket ->  --rank-by package */
        else if (0 == strcmp(option, PRTE_CLI_RANKBY)) {
            /* check the value of the option for "socket" */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup("package");
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "package:%s", p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt", "deprecated-converted", true, p2,
                                                   tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(tmp2);
                    free(p2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }
        }
        /* --bind-to socket ->  --bind-to package */
        else if (0 == strcmp(option, PRTE_CLI_BINDTO)) {
            /* check the value of the option for "socket" */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup(PRTE_CLI_PACKAGE);
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "%s:%s", PRTE_CLI_PACKAGE, p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt", "deprecated-converted", true, p2,
                                                   tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(tmp2);
                    free(p2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }

        } else if (0 == strcmp(option, "with-ft")) {
            p1 = opt->values[0];
            if (0 != strcmp("no", p1) && 0 != strcmp("false", p1) && 0 != strcmp("0", p1)) {
                if (0 == strcmp("yes", p1) || 0 == strcmp("true", p1) || 0 == strcmp("1", p1) ||
                    0 == strcmp("ulfm", p1) || 0 == strcmp("mpi", p1)) {
                    rc = prte_schizo_base_add_directive(results, option,
                                                        PRTE_CLI_RTOS, PRTE_CLI_RECOVERABLE,
                                                        warn);
                    rc = prte_schizo_base_add_directive(results, option,
                                                        PRTE_CLI_RTOS, PRTE_CLI_NOTIFY_ERRORS,
                                                        warn);
                    tmp = strdup("mpi_ft_enable=1");
                    pmix_output_verbose(1, prte_schizo_base_framework.framework_output,
                                        "%s schizo:ompi:parse_cli pushing OMPI_MCA_mpi_ft_enable into environment",
                                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                    prte_schizo_base_expose(tmp, "OMPI_MCA_");
                    free(tmp);
                }
                else {
                    pmix_show_help("help-schizo-ompi.txt", "with-ft-bad-option", true, p1);
                    return PRTE_ERR_SILENT;
                }
            }
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
    }

    return rc;
}

static int check_cache_noadd(char ***c1, char ***c2, char *p1, char *p2)
{
    char **cache;
    char **cachevals;
    int k;

    if (NULL == c1 || NULL == c2) {
        return PRTE_SUCCESS;
    }

    cache = *c1;
    cachevals = *c2;

    if (NULL != cache) {
        /* see if we already have these */
        for (k = 0; NULL != cache[k]; k++) {
            if (0 == strcmp(cache[k], p1)) {
                /* we do have it - check for same value */
                if (0 != strcmp(cachevals[k], p2)) {
                    /* this is an error */
                    pmix_show_help("help-schizo-base.txt", "duplicate-mca-value", true, p1, p2,
                                   cachevals[k]);
                    return PRTE_ERR_BAD_PARAM;
                }
            }
        }
    }
    return PRTE_SUCCESS;
}

static int check_cache(char ***c1, char ***c2, char *p1, char *p2)
{
    int rc;

    rc = check_cache_noadd(c1, c2, p1, p2);

    if (PRTE_SUCCESS == rc) {
        /* add them to the cache */
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(c1, p1);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(c2, p2);
    }
    return rc;
}

static int process_envar(const char *p, char ***cache, char ***cachevals)
{
    char *value, **tmp;
    char *p1, *p2;
    size_t len;
    int k, rc = PRTE_SUCCESS;
    bool found;

    p1 = strdup(p);
    if (NULL != (value = strchr(p1, '='))) {
        /* terminate the name of the param */
        *value = '\0';
        /* step over the equals */
        value++;
        rc = check_cache(cache, cachevals, p1, value);
    } else {
        /* check for a '*' wildcard at the end of the value */
        if ('*' == p1[strlen(p1) - 1]) {
            /* search the local environment for all params
             * that start with the string up to the '*' */
            p1[strlen(p1) - 1] = '\0';
            len = strlen(p1);
            for (k = 0; NULL != environ[k]; k++) {
                if (0 == strncmp(environ[k], p1, len)) {
                    value = strdup(environ[k]);
                    /* find the '=' sign */
                    p2 = strchr(value, '=');
                    if (NULL == p2) {
                        free(p1);
                        free(value);
                        return PRTE_ERR_BAD_PARAM;
                    }
                    *p2 = '\0';
                    ++p2;
                    rc = check_cache(cache, cachevals, value, p2);
                    free(value);
                }
            }
        } else {
            value = getenv(p1);
            if (NULL != value) {
                rc = check_cache(cache, cachevals, p1, value);
            } else {
                found = false;
                if (NULL != cache) {
                    /* see if it is already in the cache */
                    tmp = *cache;
                    for (k = 0; NULL != tmp[k]; k++) {
                        if (0 == strncmp(p1, tmp[k], strlen(p1))) {
                            found = true;
                            break;
                        }
                    }
                }
                if (!found) {
                    pmix_show_help("help-schizo-base.txt", "env-not-found", true, p1);
                    rc = PRTE_ERR_NOT_FOUND;
                }
            }
        }
    }
    free(p1);
    return rc;
}

/* process params from an env_list - add them to the cache */
static int process_token(char *token, char ***cache, char ***cachevals)
{
    char *ptr, *value;
    int rc;

    if (NULL == (ptr = strchr(token, '='))) {
        value = getenv(token);
        if (NULL == value) {
            return PRTE_ERR_NOT_FOUND;
        }

        /* duplicate the value to silence tainted string coverity issue */
        value = strdup(value);
        if (NULL == value) {
            /* out of memory */
            return PRTE_ERR_OUT_OF_RESOURCE;
        }

        if (NULL != (ptr = strchr(value, '='))) {
            *ptr = '\0';
            rc = check_cache(cache, cachevals, value, ptr + 1);
        } else {
            rc = check_cache(cache, cachevals, token, value);
        }

        free(value);
    } else {
        *ptr = '\0';
        rc = check_cache(cache, cachevals, token, ptr + 1);
        /* NTH: don't bother resetting ptr to = since the string will not be used again */
    }
    return rc;
}

static int process_env_list(const char *env_list, char ***xparams, char ***xvals, char sep)
{
    char **tokens;
    int rc = PRTE_SUCCESS;

    tokens = PMIX_ARGV_SPLIT_COMPAT(env_list, (int) sep);
    if (NULL == tokens) {
        return PRTE_SUCCESS;
    }

    for (int i = 0; NULL != tokens[i]; ++i) {
        rc = process_token(tokens[i], xparams, xvals);
        if (PRTE_SUCCESS != rc) {
            if (PRTE_ERR_NOT_FOUND == rc) {
                pmix_show_help("help-schizo-base.txt", "incorrect-env-list-param", true, tokens[i],
                               env_list);
            }
            break;
        }
    }

    PMIX_ARGV_FREE_COMPAT(tokens);
    return rc;
}

static int process_tune_files(char *filename, char ***dstenv, char sep)
{
    FILE *fp;
    char **tmp, **opts, *line, *param, *p1, *p2;
    int i, n, rc = PRTE_SUCCESS;
    char **cache = NULL, **cachevals = NULL;
    char **xparams = NULL, **xvals = NULL;

    tmp = PMIX_ARGV_SPLIT_COMPAT(filename, sep);
    if (NULL == tmp) {
        return PRTE_SUCCESS;
    }

    /* Iterate through all the files passed in -- it is an ERROR if
     * a given param appears more than once with different values */

    for (i = 0; NULL != tmp[i]; i++) {
        fp = fopen(tmp[i], "r");
        if (NULL == fp) {
            /* if the file given wasn't absolute, check in the default location */
            if (!pmix_path_is_absolute(tmp[i])) {
                p1 = pmix_os_path(false, DEFAULT_PARAM_FILE_PATH, tmp[i], NULL);
                fp = fopen(p1, "r");
                if (NULL == fp) {
                    pmix_show_help("help-schizo-base.txt", "missing-param-file-def", true, tmp[i], p1);;
                    PMIX_ARGV_FREE_COMPAT(tmp);
                    PMIX_ARGV_FREE_COMPAT(cache);
                    PMIX_ARGV_FREE_COMPAT(cachevals);
                    PMIX_ARGV_FREE_COMPAT(xparams);
                    PMIX_ARGV_FREE_COMPAT(xvals);
                    free(p1);
                    return PRTE_ERR_NOT_FOUND;
                }
                free(p1);
            } else {
                pmix_show_help("help-schizo-base.txt", "missing-param-file", true, tmp[i]);;
                PMIX_ARGV_FREE_COMPAT(tmp);
                PMIX_ARGV_FREE_COMPAT(cache);
                PMIX_ARGV_FREE_COMPAT(cachevals);
                PMIX_ARGV_FREE_COMPAT(xparams);
                PMIX_ARGV_FREE_COMPAT(xvals);
                return PRTE_ERR_NOT_FOUND;
            }
        }
        while (NULL != (line = prte_schizo_base_getline(fp))) {
            if ('\0' == line[0]) {
                continue; /* skip empty lines */
            }
            opts = PMIX_ARGV_SPLIT_WITH_EMPTY_COMPAT(line, ' ');
            if (NULL == opts) {
                pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i], line);
                free(line);
                PMIX_ARGV_FREE_COMPAT(tmp);
                PMIX_ARGV_FREE_COMPAT(cache);
                PMIX_ARGV_FREE_COMPAT(cachevals);
                PMIX_ARGV_FREE_COMPAT(xparams);
                PMIX_ARGV_FREE_COMPAT(xvals);
                fclose(fp);
                return PRTE_ERR_BAD_PARAM;
            }
            for (n = 0; NULL != opts[n]; n++) {
                if ('\0' == opts[n][0] || '#' == opts[n][0]) {
                    /* the line is only spaces, or a comment, ignore */
                    break;
                }
                if (0 == strcmp(opts[n], "-x")) {
                    /* the next value must be the envar */
                    if (NULL == opts[n + 1]) {
                        pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i],
                                       line);
                        free(line);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        fclose(fp);
                        return PRTE_ERR_BAD_PARAM;
                    }
                    p1 = prte_schizo_base_strip_quotes(opts[n + 1]);
                    /* some idiot decided to allow spaces around an "=" sign, which is
                     * a violation of the Posix cmd line syntax. Rather than fighting
                     * the battle to correct their error, try to accommodate it here */
                    if (NULL != opts[n + 2] && 0 == strcmp(opts[n + 2], "=")) {
                        if (NULL == opts[n + 3]) {
                            pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i],
                                           line);
                            free(line);
                            PMIX_ARGV_FREE_COMPAT(tmp);
                            PMIX_ARGV_FREE_COMPAT(opts);
                            PMIX_ARGV_FREE_COMPAT(cache);
                            PMIX_ARGV_FREE_COMPAT(cachevals);
                            PMIX_ARGV_FREE_COMPAT(xparams);
                            PMIX_ARGV_FREE_COMPAT(xvals);
                            fclose(fp);
                            return PRTE_ERR_BAD_PARAM;
                        }
                        p2 = prte_schizo_base_strip_quotes(opts[n + 3]);
                        pmix_asprintf(&param, "%s=%s", p1, p2);
                        free(p1);
                        free(p2);
                        p1 = param;
                        ++n; // need an extra step
                    }
                    rc = process_envar(p1, &xparams, &xvals);
                    free(p1);
                    if (PRTE_SUCCESS != rc) {
                        fclose(fp);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        free(line);
                        return rc;
                    }
                    ++n; // skip over the envar option
                } else if (0 == strcmp(opts[n], "--mca")) {
                    if (NULL == opts[n + 1] || NULL == opts[n + 2]) {
                        pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i],
                                       line);
                        free(line);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        fclose(fp);
                        return PRTE_ERR_BAD_PARAM;
                    }
                    p1 = prte_schizo_base_strip_quotes(opts[n + 1]);
                    p2 = prte_schizo_base_strip_quotes(opts[n + 2]);
                    if (0 == strcmp(p1, "mca_base_env_list")) {
                        /* next option must be the list of envars */
                        rc = process_env_list(p2, &xparams, &xvals, ';');
                    } else {
                        /* treat it as an arbitrary MCA param */
                        rc = check_cache(&cache, &cachevals, p1, p2);
                    }
                    free(p1);
                    free(p2);
                    if (PRTE_SUCCESS != rc) {
                        fclose(fp);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        free(line);
                        return rc;
                    }
                    n += 2; // skip over the MCA option
                } else if (0 == strncmp(opts[n], "mca_base_env_list", strlen("mca_base_env_list"))) {
                    /* find the equal sign */
                    p1 = strchr(opts[n], '=');
                    if (NULL == p1) {
                        pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i],
                                       line);
                        free(line);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        fclose(fp);
                        return PRTE_ERR_BAD_PARAM;
                    }
                    ++p1;
                    rc = process_env_list(p1, &xparams, &xvals, ';');
                    if (PRTE_SUCCESS != rc) {
                        fclose(fp);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        free(line);
                        return rc;
                    }
                } else {
                    rc = process_token(opts[n], &cache, &cachevals);
                    if (PRTE_SUCCESS != rc) {
                        pmix_show_help("help-schizo-base.txt", "bad-param-line", true, tmp[i],
                                       line);
                        fclose(fp);
                        PMIX_ARGV_FREE_COMPAT(tmp);
                        PMIX_ARGV_FREE_COMPAT(opts);
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        free(line);
                        return rc;
                    }
                }
            }
            free(line);
        }
        fclose(fp);
    }

    PMIX_ARGV_FREE_COMPAT(tmp);

    if (NULL != cache) {
        /* add the results into dstenv */
        for (i = 0; NULL != cache[i]; i++) {
            if (0 != strncmp(cache[i], "OMPI_MCA_", strlen("OMPI_MCA_"))) {
                pmix_asprintf(&p1, "OMPI_MCA_%s", cache[i]);
                PMIX_SETENV_COMPAT(p1, cachevals[i], true, dstenv);
                free(p1);
            } else {
                PMIX_SETENV_COMPAT(cache[i], cachevals[i], true, dstenv);
            }
        }
        PMIX_ARGV_FREE_COMPAT(cache);
        PMIX_ARGV_FREE_COMPAT(cachevals);
    }

    /* add the -x values */
    if (NULL != xparams) {
        for (i = 0; NULL != xparams[i]; i++) {
            PMIX_SETENV_COMPAT(xparams[i], xvals[i], true, dstenv);
        }
        PMIX_ARGV_FREE_COMPAT(xparams);
        PMIX_ARGV_FREE_COMPAT(xvals);
    }

    return PRTE_SUCCESS;
}

// These frameworks are current as of 16 Sep, 2022, and are the list
// of frameworks that are planned to be in Open MPI v5.0.0.
static char *ompi_frameworks_static_5_0_0[] = {
    // Generic prefixes used by OMPI
    "mca",
    "opal",
    "ompi",

    /* OPAL frameworks */
    "allocator",
    "backtrace",
    "btl",
    "dl",
    "hwloc",
    "if",
    "installdirs",
    "memchecker",
    "memcpy",
    "memory",
    "mpool",
    "patcher",
    "pmix",
    "rcache",
    "reachable",
    "shmem",
    "smsc",
    "threads",
    "timer",
    /* OMPI frameworks */
    "mpi", /* global options set in runtime/ompi_mpi_params.c */
    "bml",
    "coll",
    "fbtl",
    "fcoll",
    "fs",
    "hook",
    "io",
    "mtl",
    "op",
    "osc",
    "part",
    "pml",
    "sharedfp",
    "topo",
    "vprotocol",
    /* OSHMEM frameworks */
    "memheap",
    "scoll",
    "spml",
    "sshmem",
    NULL,
};
static char **ompi_frameworks = ompi_frameworks_static_5_0_0;
static bool ompi_frameworks_setup = false;

static void setup_ompi_frameworks(void)
{
    if (ompi_frameworks_setup) {
        return;
    }
    ompi_frameworks_setup = true;

    char *env = getenv("OMPI_MCA_PREFIXES");
    if (NULL == env) {
        return;
    }

    // If we found the env variable, it will be a comma-delimited list
    // of values.  Split it into an argv-style array.
    char **tmp = PMIX_ARGV_SPLIT_COMPAT(env, ',');
    if (NULL != tmp) {
        ompi_frameworks = tmp;
    }
}

static bool check_generic(char *p1)
{
    setup_ompi_frameworks();

    /* See if the parameter we were passed belongs to one of the OMPI
       frameworks or prefixes */
    for (int j = 0; NULL != ompi_frameworks[j]; j++) {
        if (0 == strncmp(p1, ompi_frameworks[j], strlen(ompi_frameworks[j]))) {
            return true;
        }
    }

    return false;
}

static int parse_env(char **srcenv, char ***dstenv,
                     pmix_cli_result_t *results)
{
    char *p1, *p2, *p3;
    char *env_set_flag;
    char **cache = NULL, **cachevals = NULL;
    char **xparams = NULL, **xvals = NULL;
    char **envlist = NULL, **envtgt = NULL;
    pmix_cli_item_t *opt;
    int i, j, rc;
    PRTE_HIDE_UNUSED_PARAMS(srcenv);

    pmix_output_verbose(1, prte_schizo_base_framework.framework_output,
                        "%s schizo:ompi: parse_env",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* protect against bozo input */
    if (NULL == results) {
        return PRTE_SUCCESS;
    }

    /* Begin by examining the environment as the cmd line trumps all */
    env_set_flag = getenv("OMPI_MCA_mca_base_env_list");
    if (NULL != env_set_flag) {
        rc = process_env_list(env_set_flag, &xparams, &xvals, ';');
        if (PRTE_SUCCESS != rc) {
            PMIX_ARGV_FREE_COMPAT(xparams);
            PMIX_ARGV_FREE_COMPAT(xvals);
            return rc;
        }
    }
    /* process the resulting cache into the dstenv */
    if (NULL != xparams) {
        for (i = 0; NULL != xparams[i]; i++) {
            PMIX_SETENV_COMPAT(xparams[i], xvals[i], true, dstenv);
        }
        PMIX_ARGV_FREE_COMPAT(xparams);
        xparams = NULL;
        PMIX_ARGV_FREE_COMPAT(xvals);
        xvals = NULL;
    }

    /* now process any tune file specification - the tune file processor
     * will police itself for duplicate values */
    if (NULL != (opt = pmix_cmd_line_get_param(results, "tune"))) {
        p1 = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
        rc = process_tune_files(p1, dstenv, ',');
        free(p1);
        if (PRTE_SUCCESS != rc) {
            return rc;
        }
    }

    if (NULL != (opt = pmix_cmd_line_get_param(results, "initial-errhandler"))) {
        rc = check_cache(&cache, &cachevals, "mpi_initial_errhandler", opt->values[0]);
        if (PRTE_SUCCESS != rc) {
            PMIX_ARGV_FREE_COMPAT(cache);
            PMIX_ARGV_FREE_COMPAT(cachevals);
            return rc;
        }
    }

    if (pmix_cmd_line_is_taken(results, "display-comm") &&
        pmix_cmd_line_is_taken(results, "display-comm-finalize")) {
        PMIX_SETENV_COMPAT("OMPI_MCA_ompi_display_comm", "mpi_init,mpi_finalize", true, dstenv);
    } else if (pmix_cmd_line_is_taken(results, "display-comm")) {
        PMIX_SETENV_COMPAT("OMPI_MCA_ompi_display_comm", "mpi_init", true, dstenv);
    } else if (pmix_cmd_line_is_taken(results, "display-comm-finalize")) {
        PMIX_SETENV_COMPAT("OMPI_MCA_ompi_display_comm", "mpi_finalize", true, dstenv);
    }

    /* --stream-buffering will be deprecated starting with Open MPI v5 */
    if (NULL != (opt = pmix_cmd_line_get_param(results, "stream-buffering"))) {
        uint16_t u16;
        if (prte_mca_schizo_ompi_component.warn_deprecations) {
            pmix_show_help("help-schizo-base.txt", "deprecated-inform", true, "stream-buffering",
                           "This CLI option will be deprecated starting in Open MPI v5. "
                           "If you need this functionality use the Open MPI MCA option: ompi_stream_buffering");
        }
        u16 = strtol(opt->values[0], NULL, 10);
        if (0 != u16 && 1 != u16 && 2 != u16) {
            /* bad value */
            pmix_show_help("help-schizo-base.txt", "bad-stream-buffering-value", true, u16);
        }
        PMIX_SETENV_COMPAT("OMPI_MCA_ompi_stream_buffering", opt->values[0], true, dstenv);
    }

    /* now look for any "--mca" options - note that it is an error
     * for the same MCA param to be given more than once if the
     * values differ */
    if (NULL != (opt = pmix_cmd_line_get_param(results, "omca"))) {
        for (i = 0; NULL != opt->values[i]; ++i) {
            /* the value is provided in "param=value" format, so
             * we need to split it here - it is okay to change
             * the value as we won't be using it again */
            p3 = strchr(opt->values[i], '=');
            *p3 = '\0';
            ++p3;
            p1 = opt->values[i];
            /* treat mca_base_env_list as a special case */
            if (0 == strcmp(p1, "mca_base_env_list")) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&envlist, p3);
                continue;
            }
            rc = check_cache(&cache, &cachevals, p1, p3);
            if (PRTE_SUCCESS != rc) {
                PMIX_ARGV_FREE_COMPAT(cache);
                PMIX_ARGV_FREE_COMPAT(cachevals);
                return rc;
            }
        }
    }
    if (NULL != (opt = pmix_cmd_line_get_param(results, "gomca"))) {
        for (i = 0; NULL != opt->values[i]; ++i) {
            /* the value is provided in "param=value" format, so
             * we need to split it here - it is okay to change
             * the value as we won't be using it again */
            p3 = strchr(opt->values[i], '=');
            *p3 = '\0';
            ++p3;
            p1 = opt->values[i];
            /* treat mca_base_env_list as a special case */
            if (0 == strcmp(p1, "mca_base_env_list")) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&envlist, p3);
                continue;
            }
            rc = check_cache(&cache, &cachevals, p1, p3);
            if (PRTE_SUCCESS != rc) {
                PMIX_ARGV_FREE_COMPAT(cache);
                PMIX_ARGV_FREE_COMPAT(cachevals);
                return rc;
            }
        }
    }
    if (NULL != (opt = pmix_cmd_line_get_param(results, "mca"))) {
        for (i = 0; NULL != opt->values[i]; ++i) {
            /* the value is provided in "param=value" format, so
             * we need to split it here - it is okay to change
             * the value as we won't be using it again */
            p3 = strchr(opt->values[i], '=');
            *p3 = '\0';
            ++p3;
            p1 = opt->values[i];
            /* check if this is one of ours */
            if (check_generic(p1)) {
                /* treat mca_base_env_list as a special case */
                if (0 == strcmp(p1, "mca_base_env_list")) {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&envlist, p3);
                    continue;
                }
                rc = check_cache(&cache, &cachevals, p1, p3);
                if (PRTE_SUCCESS != rc) {
                    PMIX_ARGV_FREE_COMPAT(cache);
                    PMIX_ARGV_FREE_COMPAT(cachevals);
                    PMIX_ARGV_FREE_COMPAT(envlist);
                    return rc;
                }
            }
        }
    }
    if (NULL != (opt = pmix_cmd_line_get_param(results, "gmca"))) {
        for (i = 0; NULL != opt->values[i]; ++i) {
            /* the value is provided in "param=value" format, so
             * we need to split it here - it is okay to change
             * the value as we won't be using it again */
            p3 = strchr(opt->values[i], '=');
            *p3 = '\0';
            ++p3;
            p1 = opt->values[i];
            /* check if this is one of ours */
            if (check_generic(p1)) {
                /* treat mca_base_env_list as a special case */
                if (0 == strcmp(p1, "mca_base_env_list")) {
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&envlist, p3);
                    continue;
                }
                rc = check_cache(&cache, &cachevals, p1, p3);
                if (PRTE_SUCCESS != rc) {
                    PMIX_ARGV_FREE_COMPAT(cache);
                    PMIX_ARGV_FREE_COMPAT(cachevals);
                    PMIX_ARGV_FREE_COMPAT(envlist);
                    return rc;
                }
            }
        }
    }

    /* if we got any env lists, process them here */
    if (NULL != envlist) {
        for (i = 0; NULL != envlist[i]; i++) {
            envtgt = PMIX_ARGV_SPLIT_COMPAT(envlist[i], ';');
            for (j = 0; NULL != envtgt[j]; j++) {
                if (NULL == (p2 = strchr(envtgt[j], '='))) {
                    p1 = getenv(envtgt[j]);
                    if (NULL == p1) {
                        continue;
                    }
                    p1 = strdup(p1);
                    if (NULL != (p2 = strchr(p1, '='))) {
                        *p2 = '\0';
                        rc = check_cache(&xparams, &xvals, p1, p2 + 1);
                    } else {
                        rc = check_cache(&xparams, &xvals, envtgt[j], p1);
                    }
                    free(p1);
                    if (PRTE_SUCCESS != rc) {
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(envtgt);
                        PMIX_ARGV_FREE_COMPAT(envlist);
                        return rc;
                    }
                } else {
                    *p2 = '\0';
                    rc = check_cache(&xparams, &xvals, envtgt[j], p2 + 1);
                    if (PRTE_SUCCESS != rc) {
                        PMIX_ARGV_FREE_COMPAT(cache);
                        PMIX_ARGV_FREE_COMPAT(cachevals);
                        PMIX_ARGV_FREE_COMPAT(envtgt);
                        PMIX_ARGV_FREE_COMPAT(envlist);
                        return rc;
                    }
                }
            }
            PMIX_ARGV_FREE_COMPAT(envtgt);
        }
    }
    PMIX_ARGV_FREE_COMPAT(envlist);

    /* now look for -x options - not allowed to conflict with a -mca option */
    if (NULL != (opt = pmix_cmd_line_get_param(results, "x"))) {
        for (i = 0; NULL != opt->values[i]; ++i) {
            /* the value is the envar */
            p1 = opt->values[i];
            /* if there is an '=' in it, then they are setting a value */
            if (NULL != (p2 = strchr(p1, '='))) {
                *p2 = '\0';
                ++p2;
            } else {
                p2 = getenv(p1);
                if (NULL == p2) {
                    continue;
                }
            }
            /* not allowed to duplicate anything from an MCA param on the cmd line */
            rc = check_cache_noadd(&cache, &cachevals, p1, p2);
            if (PRTE_SUCCESS != rc) {
                PMIX_ARGV_FREE_COMPAT(cache);
                PMIX_ARGV_FREE_COMPAT(cachevals);
                PMIX_ARGV_FREE_COMPAT(xparams);
                PMIX_ARGV_FREE_COMPAT(xvals);
                return rc;
            }
            /* cache this for later inclusion */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&xparams, p1);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&xvals, p2);
        }
    }

    /* process the resulting cache into the dstenv */
    if (NULL != cache) {
        for (i = 0; NULL != cache[i]; i++) {
            if (0 != strncmp(cache[i], "OMPI_MCA_", strlen("OMPI_MCA_"))) {
                pmix_asprintf(&p1, "OMPI_MCA_%s", cache[i]);
                PMIX_SETENV_COMPAT(p1, cachevals[i], true, dstenv);
                free(p1);
            } else {
                PMIX_SETENV_COMPAT(cache[i], cachevals[i], true, dstenv);
            }
        }
    }
    PMIX_ARGV_FREE_COMPAT(cache);
    PMIX_ARGV_FREE_COMPAT(cachevals);

    /* add the -x values */
    if (NULL != xparams) {
        for (i = 0; NULL != xparams[i]; i++) {
            PMIX_SETENV_COMPAT(xparams[i], xvals[i], true, dstenv);
        }
        PMIX_ARGV_FREE_COMPAT(xparams);
        PMIX_ARGV_FREE_COMPAT(xvals);
    }

#if PMIX_NUMERIC_VERSION != 0x00040208
    /* add a flag indicating that we did indeed check the tmpdir
     * for a shared file system */
    if (prte_process_info.shared_fs) {
        p1 = "TRUE";
    } else {
        p1 = "FALSE";
    }
    PMIX_SETENV_COMPAT("PRTE_SHARED_FS", p1, true, dstenv);
#endif

    return PRTE_SUCCESS;
}

static bool check_prte_overlap(char *var, char *value)
{
    char *tmp;

    if (0 == strncmp(var, "dl_", 3)) {
        pmix_asprintf(&tmp, "PRTE_MCA_prtedl_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "oob_", 4)) {
        pmix_asprintf(&tmp, "PRTE_MCA_%s", var);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "hwloc_", 6)) {
        pmix_asprintf(&tmp, "PRTE_MCA_%s", var);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "if_", 3)) {
        // need to convert if to prteif
        pmix_asprintf(&tmp, "PRTE_MCA_prteif_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "reachable_", strlen("reachable_"))) {
        // need to convert reachable to prtereachable
        pmix_asprintf(&tmp, "PRTE_MCA_prtereachable_%s", &var[strlen("reachable_")]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "plm_rsh_", strlen("plm_rsh_"))) {
        // need to convert rsh to ssh
        pmix_asprintf(&tmp, "PRTE_MCA_plm_ssh_%s", &var[strlen("plm_rsh_")]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "orte_", strlen("orte_"))) {
        // need to convert "orte" to "prte"
        pmix_asprintf(&tmp, "PRTE_MCA_prte_%s", &var[strlen("orte_")]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    }
    return false;
}


static bool check_pmix_overlap(char *var, char *value)
{
    char *tmp;

    if (0 == strncmp(var, "dl_", 3)) {
        pmix_asprintf(&tmp, "PMIX_MCA_pdl_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "oob_", 4)) {
        pmix_asprintf(&tmp, "PMIX_MCA_ptl_%s", &var[4]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "hwloc_", 6)) {
        pmix_asprintf(&tmp, "PMIX_MCA_%s", var);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "if_", 3)) {
        // need to convert if to pif
        pmix_asprintf(&tmp, "PMIX_MCA_pif_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    }
    return false;
}

// NOTE: This code is fundamentally the same (module PMIX <-> OPAL)
//      as the translate_params() routine in the OMPI repo's
//      opal/mca/pmix/base/pmix_base_fns.c file.  If there are
//      changes here, there are likely to be changes there.
static int translate_params(void)
{
    char *evar, *tmp, *e2;
    char *file;
    const char *home;
    pmix_list_t params;
    pmix_mca_base_var_file_value_t *fv;
    uid_t uid;
    int n, len;

    /* since we are the proxy, we need to check the OMPI default
     * MCA params to see if there is something relating to PRRTE
     * in them - this would be "old" references to things from
     * ORTE, as well as a few OPAL references that also impact us
     *
     * NOTE: we do this in the following precedence order. Note
     * that we do not overwrite at any step - this is so that we
     * don't overwrite something previously set by the user. So
     * the order to execution is the opposite of the intended
     * precedence order.
     *
     * 1. check the environmental paramaters for OMPI_MCA values
     *    that need to be translated
     *
     * 2. the user's home directory file as it should
     *    overwrite the system default file, but not the
     *    envars
     *
     * 3. the system default parameter file
     */
    len = strlen("OMPI_MCA_");
    for (n=0; NULL != environ[n]; n++) {
        if (0 == strncmp(environ[n], "OMPI_MCA_", len)) {
            e2 = strdup(environ[n]);
            evar = strrchr(e2, '=');
            *evar = '\0';
            ++evar;
            if (check_prte_overlap(&e2[len], evar)) {
                // check for pmix overlap
                check_pmix_overlap(&e2[len], evar);
            } else if (prte_schizo_base_check_prte_param(&e2[len])) {
                    pmix_asprintf(&tmp, "PRTE_MCA_%s", &e2[len]);
                    // set it, but don't overwrite if they already
                    // have a value in our environment
                    setenv(tmp, evar, false);
                    free(tmp);
                    // check for pmix overlap
                    check_pmix_overlap(&e2[len], evar);
            } else if (prte_schizo_base_check_pmix_param(&e2[len])) {
                pmix_asprintf(&tmp, "PMIX_MCA_%s", &e2[len]);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, evar, false);
                free(tmp);
            }
            free(e2);
        }
    }

    /* see if the user has a default MCA param file */
    uid = geteuid();

    /* try to get their home directory */
    home = pmix_home_directory(uid);
    if (NULL != home) {
        file = pmix_os_path(false, home, ".openmpi", "mca-params.conf", NULL);
        PMIX_CONSTRUCT(&params, pmix_list_t);
        pmix_mca_base_parse_paramfile(file, &params);
        free(file);
        PMIX_LIST_FOREACH (fv, &params, pmix_mca_base_var_file_value_t) {
            // see if this param relates to PRRTE
            if (check_prte_overlap(fv->mbvfv_var, fv->mbvfv_value)) {
                check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
            } else if (prte_schizo_base_check_prte_param(fv->mbvfv_var)) {
                pmix_asprintf(&tmp, "PRTE_MCA_%s", fv->mbvfv_var);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, fv->mbvfv_value, false);
                free(tmp);
                // if this relates to the DL, OOB, HWLOC, IF, or
                // REACHABLE frameworks, then we also need to set
                // the equivalent PMIx value
                check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
            } else if (prte_schizo_base_check_pmix_param(fv->mbvfv_var)) {
                pmix_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, fv->mbvfv_value, false);
                free(tmp);
            }
        }
        PMIX_LIST_DESTRUCT(&params);
    }

    /* check if the user has set OMPIHOME in their environment */
    if (NULL != (evar = getenv("OMPIHOME"))) {
        /* look for the default MCA param file */
        file = pmix_os_path(false, evar, "etc", "openmpi-mca-params.conf", NULL);
        PMIX_CONSTRUCT(&params, pmix_list_t);
        pmix_mca_base_parse_paramfile(file, &params);
        free(file);
        PMIX_LIST_FOREACH (fv, &params, pmix_mca_base_var_file_value_t) {
            // see if this param relates to PRRTE
            if (check_prte_overlap(fv->mbvfv_var, fv->mbvfv_value)) {
                check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
            } else if (prte_schizo_base_check_prte_param(fv->mbvfv_var)) {
                pmix_asprintf(&tmp, "PRTE_MCA_%s", fv->mbvfv_var);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, fv->mbvfv_value, false);
                free(tmp);
                // if this relates to the DL, OOB, HWLOC, IF, or
                // REACHABLE frameworks, then we also need to set
                // the equivalent PMIx value
                check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
            }
        }
        PMIX_LIST_DESTRUCT(&params);
    }

    return 100;
}

static int detect_proxy(char *personalities)
{
    char *evar;

    pmix_output_verbose(2, prte_schizo_base_framework.framework_output,
                        "%s[%s]: detect proxy with %s (%s)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__,
                        (NULL == personalities) ? "NULL" : personalities,
                        prte_tool_basename);

    /* COMMAND-LINE OVERRIDES ALL */
    if (NULL != personalities) {
        /* this is a list of personalities we need to check -
         * if it contains "ompi", then we are available */
        if (NULL != strstr(personalities, "ompi")) {
            return translate_params();
        }
        return 0;
    }

    /* if we were told the proxy, then use it */
    if (NULL != (evar = getenv("PRTE_MCA_schizo_proxy"))) {
        if (0 == strcmp(evar, "ompi")) {
            return translate_params();
        } else {
            return 0;
        }
    }

    /* if neither of those were true, then it cannot be us */
    return 0;
}

static void allow_run_as_root(pmix_cli_result_t *results)
{
    /* we always run last */
    char *r1, *r2;

    if (pmix_cmd_line_is_taken(results, "allow-run-as-root")) {
        prte_allow_run_as_root = true;
        return;
    }

    if (NULL != (r1 = getenv("OMPI_ALLOW_RUN_AS_ROOT"))
        && NULL != (r2 = getenv("OMPI_ALLOW_RUN_AS_ROOT_CONFIRM"))) {
        if (0 == strcmp(r1, "1") && 0 == strcmp(r2, "1")) {
            prte_allow_run_as_root = true;
            return;
        }
    }

    prte_schizo_base_root_error_msg();
}

static int set_default_ranking(prte_job_t *jdata,
                               prte_rmaps_options_t *options)
{
    int rc;
    prte_mapping_policy_t map;

    /* use the base system and then we will correct it */
    rc = prte_rmaps_base_set_default_ranking(jdata, options);
    if (PRTE_SUCCESS != rc) {
        // it will have output the error message
        return rc;
    }
    // correct how we handle PPR
    if (PRTE_MAPPING_GIVEN & PRTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
        map = PRTE_GET_MAPPING_POLICY(jdata->map->mapping);
        // set for dense packing - but don't override any user setting
        if (PRTE_MAPPING_PPR == map && !PRTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
            PRTE_SET_RANKING_POLICY(jdata->map->ranking, PRTE_RANK_BY_SLOT);
        }
    }
    return PRTE_SUCCESS;
}

static void job_info(pmix_cli_result_t *results,
                     void *jobinfo)
{
    PRTE_HIDE_UNUSED_PARAMS(results, jobinfo);
    return;
}

static int set_default_rto(prte_job_t *jdata,
                           prte_rmaps_options_t *options)
{
    PRTE_HIDE_UNUSED_PARAMS(options);
    return prte_state_base_set_runtime_options(jdata, NULL);
}
