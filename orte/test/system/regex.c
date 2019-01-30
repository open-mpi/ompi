/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include "orte_config.h"

#include <stdio.h>
#include <unistd.h>

#include "opal/util/argv.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/regx/regx.h"
#include "orte/mca/regx/base/base.h"

int main(int argc, char **argv)
{
    int rc;
    char *regex, **nodelist;
    char **nodes=NULL;
    int i;
    opal_pointer_array_t *node_pool;
    orte_node_t *nptr;

    if (argc < 1 || NULL == argv[1]) {
        fprintf(stderr, "usage: regex <comma-separated list of nodes>\n");
        return 1;
    }

    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    if (ORTE_SUCCESS != (rc = mca_base_framework_open(&orte_regx_base_framework, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_regx_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (NULL != strchr(argv[1], '[')) {
        /* given a regex to analyze */
        fprintf(stderr, "ANALYZING REGEX: %s\n", argv[1]);
        if (ORTE_SUCCESS != (rc = orte_regx.extract_node_names(argv[1], &nodes))) {
            ORTE_ERROR_LOG(rc);
        }
        for (i=0; NULL != nodes[i]; i++) {
            fprintf(stderr, "%s\n", nodes[i]);
        }
        opal_argv_free(nodes);
        orte_finalize();
        return 0;
    }

    node_pool = OBJ_NEW(opal_pointer_array_t);
    nodelist = opal_argv_split(argv[1], ',');
    for (i=0; NULL != nodelist[i]; i++) {
        orte_proc_t *daemon = NULL;

        nptr = OBJ_NEW(orte_node_t);
        nptr->name = strdup(nodelist[i]);
        daemon = OBJ_NEW(orte_proc_t);
        daemon->name.jobid = 123;
        daemon->name.vpid = i;
        nptr->daemon = daemon;

        nptr->index = opal_pointer_array_add(node_pool, nptr);
    }
    opal_argv_free(nodelist);


    if (ORTE_SUCCESS != (rc = orte_regx.nidmap_create(node_pool, &regex))) {
        ORTE_ERROR_LOG(rc);
    } else {
        char *vpids = strchr(regex, '@');
        vpids[0] = '\0';
        fprintf(stderr, "REGEX: %s\n", regex);
        if (ORTE_SUCCESS != (rc = orte_regx.extract_node_names(regex, &nodes))) {
            ORTE_ERROR_LOG(rc);
        }
        free(regex);
        regex = opal_argv_join(nodes, ',');
        opal_argv_free(nodes);
        if (0 == strcmp(regex, argv[1])) {
            fprintf(stderr, "EXACT MATCH\n");
        } else {
            fprintf(stderr, "ERROR: %s\n", regex);
        }
        free(regex);
    }

    for (i=0; (nptr = opal_pointer_array_get_item(node_pool, i)) != NULL; i++) {
        free(nptr->name);
        OBJ_RELEASE(nptr->daemon);
    }
    OBJ_RELEASE(node_pool);
}
