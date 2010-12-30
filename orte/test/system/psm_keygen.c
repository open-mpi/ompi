/* -*- C -*-
 *
 * $HEADER$
 *
 * Generate a key for PSM transports
 */

#include <stdio.h>
#include "orte/constants.h"
#include "orte/runtime/runtime.h"

#include "orte/util/pre_condition_transports.h"

int main(int argc, char* argv[])
{
    orte_job_t *jdata;
    orte_app_context_t *app;
    int i;

    if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
        fprintf(stderr, "Failed orte_init\n");
        exit(1);
    }
    
    jdata = OBJ_NEW(orte_job_t);
    app = OBJ_NEW(orte_app_context_t);
    opal_pointer_array_set_item(jdata->apps, 0, app);
    jdata->num_apps = 1;

    if (ORTE_SUCCESS != orte_pre_condition_transports(jdata)) {
        fprintf(stderr, "Failed to generate PSM key\n");
        exit(1);
    }

    for (i=0; NULL != app->env[i]; i++) {
        if (0 == strncmp("OMPI_MCA_orte_precondition_transports", app->env[i],
                         strlen("OMPI_MCA_orte_precondition_transports"))) {
            fprintf(stderr, "%s\n", app->env[i]);
            break;
        }
    }

    OBJ_RELEASE(jdata);

    if (ORTE_SUCCESS != orte_finalize()) {
        fprintf(stderr, "Failed orte_finalize\n");
        exit(1);
    }
    return 0;
}
