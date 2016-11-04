/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/pmix/pmix.h"
#include "pmix_s1.h"

/*
 * Public string showing the pmix s1 component version number
 */
const char *opal_pmix_s1_component_version_string =
        "OPAL s1 pmix MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pmix_s1_component_query(mca_base_module_t **module, int *priority);
static int pmix_s1_component_register(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_pmix_base_component_t mca_pmix_s1_component = {

    /* First, the mca_component_t struct containing meta information
           about the component itself */

    .base_version = {
        /* Indicate that we are a pmix v1.1.0 component (which also
               implies a specific MCA version) */

        OPAL_PMIX_BASE_VERSION_2_0_0,

        /* Component name and version */

        .mca_component_name = "s1",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_query_component = pmix_s1_component_query,
        .mca_register_component_params = pmix_s1_component_register,
    },
    /* Next the MCA v1.0.0 component meta data */
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    .priority = 10,
};

static int pmix_s1_component_register(void)
{
    int ret;
    mca_base_component_t *component = &mca_pmix_s1_component.base_version;

    mca_pmix_s1_component.priority = 10;
    ret = mca_base_component_var_register(component, "priority",
                                          "Priority of the pmix s1 component (default: 10)",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_pmix_s1_component.priority);
    if (0 > ret) {
        return ret;
    }

    return OPAL_SUCCESS;
}

static int pmix_s1_component_query(mca_base_module_t **module, int *priority)
{
    /* disqualify ourselves if we are not under slurm */
    if (NULL == getenv("SLURM_STEP_NUM_TASKS")) {
        *priority = 0;
        *module = NULL;
        return OPAL_ERROR;
    }

    /* we can be considered, but set our priority by default
     * to be less than s2 */
    *priority = mca_pmix_s1_component.priority;
    *module = (mca_base_module_t *)&opal_pmix_s1_module;
    return OPAL_SUCCESS;
}
