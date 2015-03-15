/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/util/show_help.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/qos/qos.h"
#include "orte/mca/qos/base/base.h"


/**
 * Function for selecting all runnable modules from those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int orte_qos_base_select(void)
{
    mca_base_component_list_item_t *cli;
    mca_qos_base_component_t *component;
    int count = 0;

    /* Query all available components and ask if their transport is available */
    OPAL_LIST_FOREACH(cli, &orte_qos_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_qos_base_component_t *) cli->cli_component;

        opal_output_verbose(5, orte_qos_base_framework.framework_output,
                            "mca:qos:select: checking available component %s",
                            component->qos_base.mca_component_name);
        if (NULL == component->start )
            opal_output_verbose(5, orte_qos_base_framework.framework_output,
                                "mca:qos:select:  component %s start function is null, type =%d",
                                component->qos_base.mca_component_name, component->type);
        else {
            /* if it fails to startup, then skip it */
        if (ORTE_SUCCESS != component->start()) {
            opal_output_verbose(5, orte_qos_base_framework.framework_output,
                                "mca:qos:select: Skipping component [%s] - failed to initialize",
                                component->qos_base.mca_component_name );
            continue;
            }
        }
        count++;
        /* store each qos componenet in the actives pointer array at the index of that component type */
       opal_pointer_array_set_item(&orte_qos_base.actives,
                                        component->type, component);
    }

    if (0 == count) {
        /* no support available means we really cannot run */
        opal_output_verbose(5, orte_qos_base_framework.framework_output,
                            "mca:qos:select: Init failed to return any available QoS components");
        orte_show_help("help-qos-base.txt", "no-interfaces-avail", true);
        return ORTE_ERR_SILENT;
    }
    opal_output_verbose(5, orte_qos_base_framework.framework_output,
                        "mca:qos:select: Found %d active QoS components",
                        count);
    return ORTE_SUCCESS;
}
