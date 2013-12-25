/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/common/pmi/common_pmi.h"

#include "orte/util/proc_info.h"

#include "grpcomm_pmi.h"

static int my_priority=5;  /* must be below "bad" module */

static int orte_grpcomm_pmi_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
orte_grpcomm_base_component_t mca_grpcomm_pmi_component = {
    {
        ORTE_GRPCOMM_BASE_VERSION_2_0_0,
        
        "pmi", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_grpcomm_pmi_open,  /* module open */
        orte_grpcomm_pmi_close, /* module close */
        orte_grpcomm_pmi_component_query, /* module query */
        orte_grpcomm_pmi_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int orte_grpcomm_pmi_register(void)
{
    mca_base_component_t *c = &mca_grpcomm_pmi_component.base_version;

    /* make the priority adjustable so users can select
     * pmi for use by apps without affecting daemons
     */
    my_priority = 5;
    (void) mca_base_component_var_register(c, "priority",
                                           "Priority of the grpcomm pmi component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_priority);
    return ORTE_SUCCESS;
}

/* Open the component */
int orte_grpcomm_pmi_open(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_close(void)
{
    mca_common_pmi_finalize ();

    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* if we are indirectly launched via orted, the
     * selection will have been turned "off" for us
     */
    if (ORTE_PROC_IS_APP &&
        mca_common_pmi_init ()) {
        /* if PMI is available, make it available for use by MPI procs */
        *priority = my_priority;
        *module = (mca_base_module_t *)&orte_grpcomm_pmi_module;
        return ORTE_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
