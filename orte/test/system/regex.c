/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include "orte_config.h"

#include <stdio.h>
#include <unistd.h>

#include "orte/util/proc_info.h"
#include "orte/util/regex.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

main(int argc, char **argv)
{
    int rc;
    char *regex, *save;
    char **nodes;

    if (argc < 1 || NULL == argv[1]) {
        fprintf(stderr, "usage: regex <comma-separated list of nodes>\n");
        return 1;
    }
    
    save = strdup(argv[1]);
    if (ORTE_SUCCESS != (rc = orte_regex_create(save, &regex))) {
        ORTE_ERROR_LOG(rc);
    } else {
        fprintf(stderr, "REGEX: %s\n", regex);
        if (ORTE_SUCCESS != (rc = orte_regex_extract_node_names(regex, &nodes))) {
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
    free(save);
}
