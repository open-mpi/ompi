/*
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/path.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/schizo/base/base.h"

#include "schizo_singularity.h"

static int setup_app(char **personality,
                     orte_app_context_t *context);
static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *context);

orte_schizo_base_module_t orte_schizo_singularity_module = {
    .setup_app = setup_app,
    .setup_fork = setup_fork
};

static int setup_app(char **personality,
                     orte_app_context_t *app)
{
    int i;
    char *newenv, *pth;
    bool takeus = false;
    char *p, *t2;

    /* see if we are included */
    for (i=0; NULL != personality[i]; i++) {
        if (0 == strcmp(personality[i], "singularity")) {
            takeus = true;
            break;
        }
    }
    if (!takeus) {
        /* even if they didn't specify, check to see if
         * this involves a singularity container */
        if (0 != strcmp(app->argv[0],"singularity") &&
            0 != strcmp(app->argv[0],"sapprun") &&
            NULL == strstr(app->argv[0], ".sapp")) {
            /* guess not! */
            return ORTE_ERR_TAKE_NEXT_OPTION;
        }
    }

    opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                        "%s schizo:singularity: checking app %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->argv[0]);

    if (0 < strlen(OPAL_SINGULARITY_PATH)) {
        asprintf(&pth, "%s/singularity", OPAL_SINGULARITY_PATH);
    } else {
        /* since we allow for detecting singularity's presence, it
         * is possible that we found it in the PATH, but not in a
         * standard location. Check for that here */
         pth = opal_path_findv("singularity", X_OK, app->env, NULL);
         if (NULL == pth) {
            /* cannot execute */
            return ORTE_ERR_TAKE_NEXT_OPTION;
         }
    }
    /* find the path and prepend it with the path to Singularity */
    for (i = 0; NULL != app->env && NULL != app->env[i]; ++i) {
        /* add to PATH */
        if (0 == strncmp("PATH=", app->env[i], 5)) {
            t2 = opal_dirname(pth);
            asprintf(&newenv, "%s:%s", t2, app->env[i] + 5);
            opal_setenv("PATH", newenv, true, &app->env);
            free(newenv);
            free(t2);
            break;
        }
    }

    /* ensure that we use "singularity run" to execute this app */
    if (0 != strcmp(app->app, "singularity")) {
        opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                            "%s schizo:singularity: adding singularity cmd %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), pth);
        /* change the app to the "singularity" command */
        free(app->app);
        app->app = pth;
    } else {
        free(pth);
    }

    /* if the app contains .sapp, then we need to strip that
     * extension so singularity doesn't bark at us */
    if (NULL != (p = strstr(app->argv[0], ".sapp"))) {
        t2 = opal_basename(app->argv[0]);
        p = strstr(t2, ".sapp");
        *p = '\0'; // strip the extension
        free(app->argv[0]);
        app->argv[0] = t2;
    }
    opal_argv_prepend_nosize(&app->argv, "run");
    opal_argv_prepend_nosize(&app->argv, "singularity");

    /* export an envar to permit shared memory operations */
    opal_setenv("SINGULARITY_NO_NAMESPACE_PID", "1", true, &app->env);

    return ORTE_SUCCESS;
}

static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *app)
{
    int i;
    bool takeus = false;
    char *p;
    char dir[MAXPATHLEN];

    /* see if we are included */
    for (i=0; NULL != jdata->personality[i]; i++) {
        if (0 == strcmp(jdata->personality[i], "singularity")) {
            takeus = true;
            break;
        }
    }
    if (!takeus) {
        /* even if they didn't specify, check to see if
         * this involves a singularity container */
        if (0 != strcmp(app->argv[0],"singularity") &&
            0 != strcmp(app->argv[0],"sapprun") &&
            NULL == strstr(app->argv[0], ".sapp")) {
            /* guess not! */
            return ORTE_ERR_TAKE_NEXT_OPTION;
        }
    }

    /* save our current directory */
    getcwd(dir, sizeof(dir));

    /* change to the working directory for this context */
    chdir(app->cwd);

    /* ensure the app is installed */
    opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                        "%s schizo:singularity: installing app %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->argv[2]);
    (void)asprintf(&p, "%s install %s.sapp &> /dev/null", app->app, app->argv[2]);
    system(p);
    free(p);

    /* return to the original directory */
    chdir(dir);

    return ORTE_SUCCESS;
}

