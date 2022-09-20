/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <stdio.h>
#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/util/printf.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/smsc/base/static-components.h"

static mca_smsc_component_t *selected_component = NULL;
mca_smsc_module_t *mca_smsc = NULL;

/*
 * Global variables
 */
MCA_BASE_FRAMEWORK_DECLARE(opal, smsc, NULL, NULL, NULL, NULL, mca_smsc_base_static_components, 0);

static int mca_smsc_compare_components(opal_list_item_t **a, opal_list_item_t **b)
{
    mca_smsc_component_t *componenta
        = (mca_smsc_component_t *) ((mca_base_component_list_item_t *) *a)->cli_component;
    mca_smsc_component_t *componentb
        = (mca_smsc_component_t *) ((mca_base_component_list_item_t *) *b)->cli_component;

    return (componenta->priority > componentb->priority)
               ? -1
               : ((componenta->priority < componentb->priority) ? 1 : 0);
}

int mca_smsc_base_select(void)
{
    mca_base_component_list_item_t *cli, *next;

    OPAL_LIST_FOREACH_SAFE (cli, next, &opal_smsc_base_framework.framework_components,
                            mca_base_component_list_item_t) {
        mca_smsc_component_t *component = (mca_smsc_component_t *) cli->cli_component;

        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
                            "mca_smsc_base_select: checking component %s",
                            component->smsc_version.mca_component_name);

        int ret = component->query();
        if (OPAL_SUCCESS != ret) {
            opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT,
                                opal_smsc_base_framework.framework_output,
                                "mca_smsc_base_select: could not select component %s. query "
                                "returned error code %d",
                                component->smsc_version.mca_component_name, ret);
            opal_list_remove_item(&opal_smsc_base_framework.framework_components, &cli->super);
            OBJ_RELEASE(cli);
            mca_base_component_close(&component->smsc_version,
                                     opal_smsc_base_framework.framework_output);
            continue;
        }
        opal_output_verbose(MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
                            "mca_smsc_base_select: component %s priority=%d",
                            component->smsc_version.mca_component_name, component->priority);
    }

    opal_list_sort(&opal_smsc_base_framework.framework_components, mca_smsc_compare_components);

    if (opal_list_get_size(&opal_smsc_base_framework.framework_components) > 0) {
        cli = (mca_base_component_list_item_t *) opal_list_get_first(
            &opal_smsc_base_framework.framework_components);

        selected_component = (mca_smsc_component_t *) cli->cli_component;
        mca_smsc = selected_component->enable();

        opal_output_verbose(
            MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
            "mca_smsc_base_select: selected shared-memory single-copy component: %s",
            selected_component->smsc_version.mca_component_name);
    } else {
        opal_output_verbose(
            MCA_BASE_VERBOSE_COMPONENT, opal_smsc_base_framework.framework_output,
            "mca_smsc_base_select: no shared-memory single-copy component available for selection");
    }

    return OPAL_SUCCESS;
}

void mca_smsc_base_register_default_params(mca_smsc_component_t *component, int default_priority)
{

    char *tmp;
    (void) opal_asprintf(&tmp, "Priority of the %s component (default: %d)",
                         component->smsc_version.mca_component_name, default_priority);
    component->priority = default_priority;
    (void) mca_base_component_var_register(&component->smsc_version, "priority", /*help_msg=*/tmp,
                                           MCA_BASE_VAR_TYPE_INT, /*enumerator=*/NULL, /*bind=*/0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ, &component->priority);
    free(tmp);
}
