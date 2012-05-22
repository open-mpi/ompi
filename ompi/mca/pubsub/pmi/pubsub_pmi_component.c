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
#include "ompi/constants.h"

#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/common/pmi/common_pmi.h"

#include "pubsub_pmi.h"

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
        pubsub_pmi_component_query  /* component query */
    },
    {
        /* This component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int pubsub_pmi_component_open(void)
{
    mca_base_component_t *c = &mca_pubsub_pmi_component.base_version;

    mca_base_param_reg_int(c, "priority",
                           "Priority of the pubsub pmi component",
                           false, false, my_priority,
                           &my_priority);
    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_close(void)
{
    mca_common_pmi_finalize ();

    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* for now, only use PMI when direct launched */
    if (NULL == orte_process_info.my_hnp_uri &&
        ORTE_PROC_IS_MPI &&
        mca_common_pmi_init ()) {
        /* if PMI is available, use it */
        *priority = my_priority;
        *module = (mca_base_module_t *)&ompi_pubsub_pmi_module;
        return ORTE_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return OMPI_ERROR;
}
