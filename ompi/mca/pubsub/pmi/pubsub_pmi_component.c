/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/common/pmi/common_pmi.h"

#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "pubsub_pmi.h"

static int pubsub_pmi_component_register(void);
static int pubsub_pmi_component_open(void);
static int pubsub_pmi_component_close(void);
static int pubsub_pmi_component_query(mca_base_module_t **module, int *priority);

static int my_priority = 100;  /* must be above "orte" component */

ompi_pubsub_base_component_t mca_pubsub_pmi_component = {
    {
        OMPI_PUBSUB_BASE_VERSION_2_0_0,
        
        "pmi", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        pubsub_pmi_component_open,  /* component open */
        pubsub_pmi_component_close, /* component close */
        pubsub_pmi_component_query, /* component query */
        pubsub_pmi_component_register /* component register */
    },
    {
        /* This component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int pubsub_pmi_component_register(void)
{
    my_priority = 100;
    (void) mca_base_component_var_register(&mca_pubsub_pmi_component.base_version,
                                           "priority", "Priority of the pubsub pmi component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_priority);

    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_open(void)
{
    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_close(void)
{
    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* if we are indirectly launched via orted, the
     * selection will have been turned "off" for us
     */
    if (mca_common_pmi_init ()) {
        *priority = my_priority;
        *module = (mca_base_module_t *)&ompi_pubsub_pmi_module;
        return OMPI_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return OMPI_ERROR;
}
