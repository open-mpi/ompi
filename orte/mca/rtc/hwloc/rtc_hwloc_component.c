/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "rtc_hwloc.h"

/*
 * Local functions
 */

static int rtc_hwloc_query(mca_base_module_t **module, int *priority);
static int rtc_hwloc_register(void);

static int my_priority;

orte_rtc_base_component_t mca_rtc_hwloc_component = {
    .base_version = {
        ORTE_RTC_BASE_VERSION_1_0_0,

        .mca_component_name = "hwloc",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),
        .mca_query_component = rtc_hwloc_query,
        .mca_register_component_params = rtc_hwloc_register,
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


static int rtc_hwloc_register(void)
{
    mca_base_component_t *c = &mca_rtc_hwloc_component.base_version;

    /* set as the default */
    my_priority = 70;
    (void) mca_base_component_var_register (c, "priority", "Priority of the HWLOC rtc component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &my_priority);

    return ORTE_SUCCESS;
}


static int rtc_hwloc_query(mca_base_module_t **module, int *priority)
{
    /* Only run on the HNP */

    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_rtc_hwloc_module;

    return ORTE_SUCCESS;
}
