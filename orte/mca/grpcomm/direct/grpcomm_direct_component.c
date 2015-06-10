/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/mca/mca.h"
#include "opal/runtime/opal_params.h"

#include "orte/util/proc_info.h"

#include "grpcomm_direct.h"

static int my_priority=5;  /* must be below "bad" module */
static int direct_open(void);
static int direct_close(void);
static int direct_query(mca_base_module_t **module, int *priority);
static int direct_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_direct_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_3_0_0,

        "direct", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        direct_open,  /* component open */
        direct_close, /* component close */
        direct_query, /* component query */
        direct_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int direct_register(void)
{
    mca_base_component_t *c = &mca_grpcomm_direct_component.base_version;

    /* make the priority adjustable so users can select
     * direct for use by apps without affecting daemons
     */
    my_priority = 1;
    (void) mca_base_component_var_register(c, "priority",
                                           "Priority of the grpcomm direct component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_priority);
    return ORTE_SUCCESS;
}

/* Open the component */
static int direct_open(void)
{
    return ORTE_SUCCESS;
}

static int direct_close(void)
{
    return ORTE_SUCCESS;
}

static int direct_query(mca_base_module_t **module, int *priority)
{
    /* we are always available */
    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_grpcomm_direct_module;
    return ORTE_SUCCESS;
}
