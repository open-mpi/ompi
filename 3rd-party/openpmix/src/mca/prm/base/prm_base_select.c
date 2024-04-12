/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_show_help.h"
#include "src/mca/prm/base/base.h"

/* Function for selecting a component
 * from all those that are available. */
int pmix_prm_base_select(void)
{
    pmix_mca_base_component_list_item_t *cli = NULL;
    pmix_prm_base_component_t *component = NULL;
    pmix_mca_base_module_t *mod;
    pmix_prm_module_t *pmod;
    int rc, pri, best_pri = -1;
    ;
    bool inserted = false;

    if (pmix_prm_base.selected) {
        /* ensure we don't do this twice */
        return PMIX_SUCCESS;
    }
    pmix_prm_base.selected = true;

    /* Query all available components and ask if they have a module */
    PMIX_LIST_FOREACH (cli, &pmix_prm_base_framework.framework_components,
                       pmix_mca_base_component_list_item_t) {
        component = (pmix_prm_base_component_t *) cli->cli_component;

        pmix_output_verbose(5, pmix_prm_base_framework.framework_output,
                            "mca:prm:select: checking available component %s",
                            component->pmix_mca_component_name);

        /* get the module for this component */
        rc = component->pmix_mca_query_component(&mod, &pri);
        if (PMIX_SUCCESS != rc || NULL == mod) {
            continue;
        }

        /* If we got a module, try to initialize it */
        pmod = (pmix_prm_module_t *) mod;
        if (NULL != pmod->init && PMIX_SUCCESS != pmod->init()) {
            continue;
        }

        /* keep only the highest priority module */
        if (best_pri < pri) {
            best_pri = pri;
            /* give any prior module a chance to finalize */
            if (NULL != pmix_prm.finalize) {
                pmix_prm.finalize();
            }
            pmix_prm = *pmod;
            inserted = true;
        }
    }

    /* if no modules were found, then that's an error as we require at least one */
    if (!inserted) {
        pmix_show_help("help-pmix-runtime.txt", "no-plugins", true, "PRM");
        return PMIX_ERR_SILENT;
    }

    pmix_output_verbose(5, pmix_prm_base_framework.framework_output,
                        "mca:prm:select: using component %s", pmix_prm.name);

    return PMIX_SUCCESS;
}
