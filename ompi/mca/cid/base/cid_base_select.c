/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"

#include "ompi/constants.h"
#include "ompi/mca/cid/cid.h"
#include "ompi/mca/cid/base/base.h"

/**
 * Function for selecting one component from all those that are
 * available.
 */
int ompi_cid_base_select(bool ompi_mpi_thread_multiple)
{
    ompi_cid_base_component_t *best_component = NULL;
    mca_base_component_list_item_t *cli = NULL;
    ompi_cid_base_component_t *component = NULL;
    ompi_cid_base_module_t *module = NULL;
    int priority = 0, best_priority = INT32_MIN;
    int rc;

    best_component = NULL;

    opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                         "mca:base:select: Auto-selecting cid components");

    /*
     * Traverse the list of available components.
     * For each call their 'query' functions to determine relative priority.
     */
    OPAL_LIST_FOREACH(cli, &ompi_cid_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (ompi_cid_base_component_t *) cli->cli_component;

        /*
         * If there is a query function then use it.
         */
        if (NULL == component->query) {
            opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                                 "mca:base:select:(cid) Skipping component [%s]. It does not implement a query function",
                                 component->base_version.mca_component_name );
            continue;
        }

        /*
         * Query this component for the module and priority
         */
        opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                             "mca:base:select:(cid) Querying component [%s]",
                             component->base_version.mca_component_name);

        rc = component->query(&module, &priority, ompi_mpi_thread_multiple);
        if (OPAL_ERR_FATAL == rc) {
            /* a fatal error was detected by this component - e.g., the
             * user specified a required element and the component could
             * not find it. In this case, we must not continue as we might
             * find some other component that could run, causing us to do
             * something the user didn't want */
             return rc;
        } else if (OPAL_SUCCESS != rc) {
            /* silently skip this component */
            continue;
        }

        /*
         * If no module was returned, then skip component
         */
        if (NULL == module) {
            opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                                 "mca:base:select:(cid) Skipping component [%s]. Query failed to return a module",
                                 component->base_version.mca_component_name );
            continue;
        }

        /*
         * Determine if this is the best module we have seen by looking the priority
         */
        opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                             "mca:base:select:(cid) Query of component [%s] set priority to %d",
                             component->base_version.mca_component_name, priority);
        if (priority > best_priority) {
            best_priority  = priority;
            best_component = component;
            ompi_cid    = module;
        }
    }

    /*
     * Finished querying all components.
     * Make sure we found something in the process.
     */
    if (NULL == best_component) {
        opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                            "mca:base:select:(cid) No component selected!");
        /*
         * Still close the non-selected components
         */
        mca_base_components_close(0, /* Pass 0 to keep this from closing the output handle */
                                  &ompi_cid_base_framework.framework_components,
                                  NULL);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose (MCA_BASE_VERBOSE_COMPONENT, ompi_cid_base_framework.framework_output,
                         "mca:base:select:(cid) Selected component [%s]",
                         best_component->base_version.mca_component_name);

    /*
     * Close the non-selected components
     */
    mca_base_components_close(ompi_cid_base_framework.framework_output,
                              &ompi_cid_base_framework.framework_components,
                              &best_component->base_version);


    return OPAL_SUCCESS;
}
