/*
 * Copyright (c) 2016      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/base/base.h"
#include "opal/util/cmd_line.h"
#include "opal/util/printf.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/schizo/base/base.h"

#include "orte/util/cmd_line.h"

int orte_cmd_line_create(opal_cmd_line_t *cmd_line,
                         int argc, char **argv,
                         char ***context_env, char ***global_env,
                         bool *version, bool *help)
{
    int i, rc;

    if (NULL != version) {
        *version = false;
    }
    if (NULL != help) {
        *help = false;
    }

    if (NULL != version) {
        /* see if they asked for us to print version */
        for (i=0; NULL != argv[i]; i++) {
            if (0 == strcmp(argv[i], "--version") ||
                0 == strcmp(argv[i], "-V")) {
                *version = true;
                return ORTE_SUCCESS;
            }
        }
    }

    /* process any mca params */
    if (OPAL_SUCCESS != (rc = mca_base_cmd_line_process_args(argv, context_env, global_env))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }


    opal_cmd_line_create(cmd_line, NULL);

    /* init the MCA system - will just refcount if already initialized */
    opal_init_util(NULL, NULL);

    /* open the SCHIZO framework so we can define the cmd line options */
    if (ORTE_SUCCESS != (rc = mca_base_framework_open(&orte_schizo_base_framework, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_schizo_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* define the cli options */
    if (ORTE_SUCCESS != (rc = orte_schizo.define_cli(cmd_line))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* close the framework for bookkeeping purposes */
    mca_base_framework_close(&orte_schizo_base_framework);

    /* decrement the opal refcount */
    opal_finalize_util();

    /* now that options have been defined, finish setup */
    mca_base_cmd_line_setup(cmd_line);


    /* Check for help request - must do this after we setup
     * the cmd line so the help messages can display */
    if (NULL != help) {
        for (i=0; NULL != argv[i]; i++) {
            if (0 == strcmp(argv[i], "--help") ||
                0 == strcmp(argv[i], "-h")) {
                *help = true;
                return ORTE_SUCCESS;
            }
        }
    }

    /* parse the result to get values */
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(cmd_line, true,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    return ORTE_SUCCESS;
}
