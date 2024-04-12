/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/mca/schizo/base/base.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_getcwd.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/pmix_show_help.h"

#include "src/runtime/prte_globals.h"

#include "src/prted/prted.h"

/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   prun --mca foo bar -app appfile
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
 *   prun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(prte_schizo_base_module_t *schizo, char **argv, pmix_list_t *jdata,
                      prte_pmix_app_t **app_ptr, bool *made_app, char ***app_env,
                      char ***hostfiles, char ***hosts)
{
    char cwd[PRTE_PATH_MAX];
    int i, count, rc;
    char *param, *value, *ptr;
    prte_pmix_app_t *app = NULL;
    char *appname = NULL;
    pmix_cli_item_t *opt;
    pmix_cli_result_t results;
    char *tval;
    bool fwd;
    pmix_value_t val;
    PRTE_HIDE_UNUSED_PARAMS(jdata, app_env);

    *made_app = false;

    /* parse the cmd line - do this every time thru so we can
     * repopulate the globals */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    rc = schizo->parse_cli(argv, &results, PMIX_CLI_SILENT);
    if (PRTE_SUCCESS != rc) {
        if (PRTE_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0], prte_strerror(rc));
        }
        PMIX_DESTRUCT(&results);
        return rc;
    }
    // sanity check the results
    rc = schizo->check_sanity(&results);
    if (PRTE_SUCCESS != rc) {
        // sanity checker prints the reason
        PMIX_DESTRUCT(&results);
        return rc;
    }

    /* See if we have anything left */
    if (NULL == results.tail) {
        rc = PRTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    /* Setup application context */
    app = PMIX_NEW(prte_pmix_app_t);
    app->app.argv = PMIX_ARGV_COPY_COMPAT(results.tail);
    // app->app.cmd is setup below.

    /* see if we are to forward the environment */
    fwd = prte_fwd_environment;
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_FWD_ENVIRON);
    if (NULL != opt) {
        /* cmd line trumps the MCA param */
        if (NULL != opt->values) {
            val.type = PMIX_STRING;
            val.data.string = opt->values[0];
            fwd = PMIX_CHECK_TRUE(&val);
        } else {
            fwd = true;
        }
    }
    if (fwd) {
        app->app.env = PMIX_ARGV_COPY_COMPAT(environ);
    }

    /* get the cwd - we may need it in several places */
    if (PRTE_SUCCESS != (rc = pmix_getcwd(cwd, sizeof(cwd)))) {
        pmix_show_help("help-prun.txt", "prun:init-failure", true, "get the cwd", rc);
        goto cleanup;
    }

    /* Did the user specify a path to the executable? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PATH);
    if (NULL != opt) {
        param = opt->values[0];
        /* if this is a relative path, convert it to an absolute path */
        if (pmix_path_is_absolute(param)) {
            value = strdup(param);
        } else {
            /* construct the absolute path */
            value = pmix_os_path(false, cwd, param, NULL);
        }
        /* construct the new argv[0] */
        ptr = pmix_os_path(false, value, app->app.argv[0], NULL);
        free(value);
        free(app->app.argv[0]);
        app->app.argv[0] = ptr;
    }

    /* Did the user request a specific wdir? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_WDIR);
    if (NULL != opt) {
        param = opt->values[0];
        /* if this is a relative path, convert it to an absolute path */
        if (pmix_path_is_absolute(param)) {
            app->app.cwd = strdup(param);
        } else {
            /* construct the absolute path */
            app->app.cwd = pmix_os_path(false, cwd, param, NULL);
        }
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_WDIR_USER_SPECIFIED, NULL, PMIX_BOOL);
    } else if (pmix_cmd_line_is_taken(&results, PRTE_CLI_SET_CWD_SESSION)) {
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_SET_SESSION_CWD, NULL, PMIX_BOOL);
    } else {
        app->app.cwd = strdup(cwd);
    }

    /* if they specified a process set name, then pass it along */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PSET);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_PSET_NAME,
                           opt->values[0], PMIX_STRING);
    }

    /* Did the user specify a hostfile? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_HOSTFILE);
    if (NULL != opt) {
        for (i=0; NULL != opt->values[i]; i++) {
            if (!pmix_path_is_absolute(opt->values[i])) {
                value = pmix_os_path(false, cwd, opt->values[i], NULL);
                free(opt->values[i]);
                opt->values[i] = value;
            }
        }
        tval = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_HOSTFILE,
                           tval, PMIX_STRING);
        free(tval);
        if (NULL != hostfiles) {
            for (i=0; NULL != opt->values[i]; i++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(hostfiles, opt->values[i]);
            }
        }
    }

    /* Did the user specify an add-hostfile? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_ADDHOSTFILE);
    if (NULL != opt) {
        for (i=0; NULL != opt->values[i]; i++) {
            if (!pmix_path_is_absolute(opt->values[i])) {
                value = pmix_os_path(false, cwd, opt->values[i], NULL);
                free(opt->values[i]);
                opt->values[i] = value;
            }
        }
        tval = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_ADD_HOSTFILE,
                           tval, PMIX_STRING);
        free(tval);
        // we don't add these to the hostfiles array as they
        // are not part of an initial DVM
    }

    /* Did the user specify any hosts? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_HOST);
    if (NULL != opt) {
        tval = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_HOST, tval, PMIX_STRING);
        free(tval);
        if (NULL != hosts) {
            for (i=0; NULL != opt->values[i]; i++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(hosts, opt->values[i]);
            }
        }
    }

    /* Did the user specify any add-hosts? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_ADDHOST);
    if (NULL != opt) {
        tval = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_ADD_HOST, tval, PMIX_STRING);
        free(tval);
        // we don't add these to the hosts array as they
        // are not part of an initial DVM
    }

    /* check for bozo error */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_NP);
    if (NULL != opt) {
        count = strtol(opt->values[0], NULL, 10);
        if (0 > count) {
            pmix_show_help("help-prun.txt", "prun:negative-nprocs", true,
                           prte_tool_basename,
                           app->app.argv[0], count, NULL);
            return PRTE_ERR_FATAL;
        }
        /* we don't require that the user provide --np or -n because
         * the cmd line might stipulate a mapping policy that computes
         * the number of procs - e.g., a map-by ppr option */
        app->app.maxprocs = count;
    }

    /* check for preload files */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PRELOAD_FILES);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_PRELOAD_FILES, opt->values[0], PMIX_STRING);
    }

    /* check for preload binary */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PRELOAD_BIN);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(rc, app->info, PMIX_PRELOAD_BIN, NULL, PMIX_BOOL);
    }

    /* Do not try to find argv[0] here -- the starter is responsible
     for that because it may not be relevant to try to find it on
     the node where prun is executing.  So just strdup() argv[0]
     into app. */

    app->app.cmd = strdup(app->app.argv[0]);
    if (NULL == app->app.cmd) {
        pmix_show_help("help-prun.txt", "prun:call-failed", true, "prun", "library",
                       "strdup returned NULL", errno);
        rc = PRTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * does the schizo being used have a setup_app method?
     * if so call here
     */
    if(NULL != schizo->setup_app) {
        rc = schizo->setup_app(app);
        if (PRTE_SUCCESS != rc) {
            goto cleanup;
        }
    }

    // parse any environment-related cmd line options
    rc = schizo->parse_env(prte_launch_environ, &app->app.env, &results);
    if (PRTE_SUCCESS != rc) {
        goto cleanup;
    }

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

cleanup:
    if (NULL != app) {
        PMIX_RELEASE(app);
    }
    if (NULL != appname) {
        free(appname);
    }
    PMIX_DESTRUCT(&results);
    return rc;
}

int prte_parse_locals(prte_schizo_base_module_t *schizo,
                      pmix_list_t *jdata, char *argv[],
                      char ***hostfiles, char ***hosts)
{
    int i, j, rc;
    char **temp_argv, **env;
    prte_pmix_app_t *app;
    bool made_app;

    /* Make the apps */
    temp_argv = NULL;
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
     create_app(), below.  See comment immediately before the
     create_app() function for an explanation. */

    env = NULL;
    for (i = 1; NULL != argv[i]; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (PMIX_ARGV_COUNT_COMPAT(temp_argv) > 1) {
                if (NULL != env) {
                    PMIX_ARGV_FREE_COMPAT(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(schizo, temp_argv, jdata, &app, &made_app, &env,
                                hostfiles, hosts);
                if (PRTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                     printed; */
                    PMIX_ARGV_FREE_COMPAT(temp_argv);
                    return rc;
                }
                if (made_app) {
                    pmix_list_append(jdata, &app->super);
                }

                /* Reset the temps */
                PMIX_ARGV_FREE_COMPAT(temp_argv);
                temp_argv = NULL;
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&temp_argv, argv[0]);
            }
        } else {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&temp_argv, argv[i]);
        }
    }

    if (PMIX_ARGV_COUNT_COMPAT(temp_argv) > 1) {
        app = NULL;
        rc = create_app(schizo, temp_argv, jdata, &app, &made_app, &env,
                        hostfiles, hosts);
        if (PRTE_SUCCESS != rc) {
            return rc;
        }
        if (made_app) {
            pmix_list_append(jdata, &app->super);
        }
    }

    if (NULL != env) {
        PMIX_ARGV_FREE_COMPAT(env);
    }
    PMIX_ARGV_FREE_COMPAT(temp_argv);

    /* All done */

    return PRTE_SUCCESS;
}
