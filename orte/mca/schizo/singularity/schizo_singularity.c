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

    opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                        "%s schizo:singularity: checking app %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->argv[0]);

    /* see if this executable is a Singularity container */
    if (0 == strcmp(app->argv[0],"singularity") ||
        0 == strcmp(app->argv[0],"sapprun") ||
        NULL != strstr(app->argv[0], ".sapp")) {
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
    }
    /* flag that the app is in a container */
    opal_setenv("OPAL_PROC_CONTAINER", "1", true, &app->env);

    return ORTE_SUCCESS;
}
