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

#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/schizo/base/base.h"

#include "schizo_singularity.h"

static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *context);

orte_schizo_base_module_t orte_schizo_singularity_module = {
    NULL,
    NULL,
    setup_fork,
    NULL
};

static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *app)
{
    int i;
    char *newenv;
    bool takeus = false;
    char *cmd, *tmp = NULL, *p, *t2;

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

    opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                        "%s schizo:singularity: checking app %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->argv[0]);

    /* find the path and prepend it with the path to Singularity */
    for (i = 0; NULL != app->env && NULL != app->env[i]; ++i) {
        /* add to PATH */
        if (0 == strncmp("PATH=", app->env[i], 5)) {
            asprintf(&newenv, "%s:%s", OPAL_SINGULARITY_PATH, app->env[i] + 5);
            opal_setenv("PATH", newenv, true, &app->env);
            free(newenv);
            break;
        }
    }

    /* flag that the app is in a container */
    opal_setenv("OPAL_PROC_CONTAINER", "1", true, &app->env);

    /* ensure that we use "singularity run" to execute this app */
    if (0 != strcmp(app->app, "singularity")) {
        opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                            "%s schizo:singularity: adding singularity cmds at %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), OPAL_SINGULARITY_PATH);
        /* change the app to the "singularity" command */
        free(app->app);
        if (0 < strlen(OPAL_SINGULARITY_PATH)) {
            asprintf(&app->app, "%s/singularity", OPAL_SINGULARITY_PATH);
        } else {
            app->app = strdup("singularity");
        }
        /* if the app contains .sapp, then we need to strip that
         * extension so singularity doesn't bark at us */
        if (NULL != (p = strstr(app->argv[0], ".sapp"))) {
            tmp = strdup(app->argv[0]);
            t2 = opal_basename(app->argv[0]);
            p = strstr(t2, ".sapp");
            *p = '\0'; // strip the extension
            free(app->argv[0]);
            app->argv[0] = t2;
        }
        opal_argv_prepend_nosize(&app->argv, "run");
        opal_argv_prepend_nosize(&app->argv, "singularity");
    }
    /* ensure this application has been "installed" */
    if (NULL != tmp) {
        opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                            "%s schizo:singularity: installing container %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tmp);
        (void)asprintf(&cmd, "singularity install %s >> /dev/null", tmp);
        system(cmd);
        free(cmd);
        free(tmp);
    }

    return ORTE_SUCCESS;
}
