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

#include "orte/util/show_help.h"

#include "rtc_freq.h"

/*
 * Local functions
 */

static int rtc_freq_query(mca_base_module_t **module, int *priority);
static int rtc_freq_register(void);

static int my_priority;

orte_rtc_freq_component_t mca_rtc_freq_component = {
    {
        .base_version = {
            ORTE_RTC_BASE_VERSION_1_0_0,

            .mca_component_name = "freq",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),
            .mca_query_component = rtc_freq_query,
            .mca_register_component_params = rtc_freq_register,
        },
        .base_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
    }
};

static char *freq;

static int rtc_freq_register(void)
{
    mca_base_component_t *c = &mca_rtc_freq_component.super.base_version;

    /* ordering here doesn't really matter */
    my_priority = 50;
    (void) mca_base_component_var_register (c, "priority", "Priority of the FREQ rtc component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &my_priority);

    mca_rtc_freq_component.governor = NULL;
    (void) mca_base_component_var_register (c, "governor", "Governor to be used by default (default: system setting)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_rtc_freq_component.governor);

    mca_rtc_freq_component.max_freq = NULL;
    (void) mca_base_component_var_register (c, "max", "Max frequency to be used by default (default: system setting)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_rtc_freq_component.max_freq);

    mca_rtc_freq_component.min_freq = NULL;
    (void) mca_base_component_var_register (c, "min", "Min frequency to be used by default (default: system setting)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_rtc_freq_component.min_freq);

    freq = NULL;
    (void) mca_base_component_var_register (c, NULL, "Specific frequency to be used by default",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &freq);

    if (NULL != freq) {
        /* if a max/min was also given, then that is an error */
        if (NULL != mca_rtc_freq_component.max_freq ||
            NULL != mca_rtc_freq_component.min_freq) {
            orte_show_help("help-rtc-freq.txt", "conflict-freq", true, freq,
                           (NULL == mca_rtc_freq_component.max_freq) ? "NULL" : mca_rtc_freq_component.max_freq,
                           (NULL == mca_rtc_freq_component.min_freq) ? "NULL" : mca_rtc_freq_component.min_freq);
            return ORTE_ERR_SILENT;
        }
        /* set the max/min to the given value */
        mca_rtc_freq_component.max_freq = strdup(freq);
        mca_rtc_freq_component.min_freq = strdup(freq);
    }

    return ORTE_SUCCESS;
}


static int rtc_freq_query(mca_base_module_t **module, int *priority)
{
    /* in general, only root can change the frequency setting
     * of a node. However, if the sys admin has configured the
     * remote node with the "userspace" governor, then individual
     * users will be able to do so. Hence, we allow this component
     * to always be selected, and will instead error out if the
     * remote node cannot support the desired setting
     */
    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_rtc_freq_module;

    return ORTE_SUCCESS;
}
