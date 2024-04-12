/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"

#include "src/util/pmix_show_help.h"

#include "src/mca/oob/base/base.h"
#include "src/mca/oob/oob.h"
#include "src/runtime/prte_globals.h"

/**
 * Function for selecting all runnable modules from those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int prte_oob_base_select(void)
{
    pmix_mca_base_component_list_item_t *cli, *cmp, *c2;
    prte_oob_base_component_t *component, *c3;
    bool added;
    int i, rc;

    /* Query all available components and ask if their transport is available */
    PMIX_LIST_FOREACH(cli, &prte_oob_base_framework.framework_components,
                      pmix_mca_base_component_list_item_t)
    {
        component = (prte_oob_base_component_t *) cli->cli_component;

        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "mca:oob:select: checking available component %s",
                            component->oob_base.pmix_mca_component_name);

        /* If there's no query function, skip it */
        if (NULL == component->available) {
            pmix_output_verbose(
                5, prte_oob_base_framework.framework_output,
                "mca:oob:select: Skipping component [%s]. It does not implement a query function",
                component->oob_base.pmix_mca_component_name);
            continue;
        }

        /* Query the component */
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "mca:oob:select: Querying component [%s]",
                            component->oob_base.pmix_mca_component_name);

        rc = component->available();

        /* If the component is not available, then skip it as
         * it has no available interfaces
         */
        if (PRTE_SUCCESS != rc && PRTE_ERR_FORCE_SELECT != rc) {
            pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                                "mca:oob:select: Skipping component [%s] - no available interfaces",
                                component->oob_base.pmix_mca_component_name);
            continue;
        }

        /* if it fails to startup, then skip it */
        if (PRTE_SUCCESS != component->startup()) {
            pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                                "mca:oob:select: Skipping component [%s] - failed to startup",
                                component->oob_base.pmix_mca_component_name);
            continue;
        }

        if (PRTE_ERR_FORCE_SELECT == rc) {
            /* this component shall be the *only* component allowed
             * for use, so shutdown and remove any prior ones */
            while (NULL
                   != (cmp = (pmix_mca_base_component_list_item_t *) pmix_list_remove_first(
                           &prte_oob_base.actives))) {
                c3 = (prte_oob_base_component_t *) cmp->cli_component;
                if (NULL != c3->shutdown) {
                    c3->shutdown();
                }
                PMIX_RELEASE(cmp);
            }
            c2 = PMIX_NEW(pmix_mca_base_component_list_item_t);
            c2->cli_component = (pmix_mca_base_component_t *) component;
            pmix_list_append(&prte_oob_base.actives, &c2->super);
            break;
        }

        /* record it, but maintain priority order */
        added = false;
        PMIX_LIST_FOREACH(cmp, &prte_oob_base.actives, pmix_mca_base_component_list_item_t)
        {
            c3 = (prte_oob_base_component_t *) cmp->cli_component;
            if (c3->priority > component->priority) {
                continue;
            }
            pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                                "mca:oob:select: Inserting component");
            c2 = PMIX_NEW(pmix_mca_base_component_list_item_t);
            c2->cli_component = (pmix_mca_base_component_t *) component;
            pmix_list_insert_pos(&prte_oob_base.actives, &cmp->super, &c2->super);
            added = true;
            break;
        }
        if (!added) {
            /* add to end */
            pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                                "mca:oob:select: Adding component to end");
            c2 = PMIX_NEW(pmix_mca_base_component_list_item_t);
            c2->cli_component = (pmix_mca_base_component_t *) component;
            pmix_list_append(&prte_oob_base.actives, &c2->super);
        }
    }

    if (0 == pmix_list_get_size(&prte_oob_base.actives)) {
        /* no support available means we really cannot run */
        pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                            "mca:oob:select: Init failed to return any available transports");
        pmix_show_help("help-oob-base.txt", "no-interfaces-avail", true);
        return PRTE_ERR_SILENT;
    }

    /* provide them an index so we can track their usability in a bitmap */
    i = 0;
    PMIX_LIST_FOREACH(cmp, &prte_oob_base.actives, pmix_mca_base_component_list_item_t)
    {
        c3 = (prte_oob_base_component_t *) cmp->cli_component;
        c3->idx = i++;
    }

    pmix_output_verbose(5, prte_oob_base_framework.framework_output,
                        "mca:oob:select: Found %d active transports",
                        (int) pmix_list_get_size(&prte_oob_base.actives));
    return PRTE_SUCCESS;
}
