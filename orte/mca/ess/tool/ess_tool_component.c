/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/util/proc_info.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/tool/ess_tool.h"

extern orte_ess_base_module_t orte_ess_tool_module;

static int tool_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_tool_component_t mca_ess_tool_component = {
    {
        .base_version = {
            ORTE_ESS_BASE_VERSION_3_0_0,

            /* Component name and version */
            .mca_component_name = "tool",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = orte_ess_tool_component_open,
            .mca_close_component = orte_ess_tool_component_close,
            .mca_query_component = orte_ess_tool_component_query,
            .mca_register_component_params = tool_component_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    },
    .async = false
};

static int tool_component_register(void)
{
    mca_base_component_t *c = &mca_ess_tool_component.super.base_version;

    mca_ess_tool_component.async = false;
    (void) mca_base_component_var_register (c, "async_progress", "Setup an async progress thread",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_2,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_ess_tool_component.async);
    return ORTE_SUCCESS;
}

int
orte_ess_tool_component_open(void)
{
    return ORTE_SUCCESS;
}


int orte_ess_tool_component_query(mca_base_module_t **module, int *priority)
{
    /* if we are a tool, we want to be selected
     * UNLESS some enviro-specific component takes
     * precedence. This would happen, for example,
     * if the tool is a distributed set of processes
     */
    if (ORTE_PROC_IS_TOOL) {
       *priority = 10;
        *module = (mca_base_module_t *)&orte_ess_tool_module;
        return ORTE_SUCCESS;
    }

    /* else, don't */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


int
orte_ess_tool_component_close(void)
{
    return ORTE_SUCCESS;
}

