/* -*- C -*-
 *
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <pmi.h>
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "grpcomm_pmi.h"

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
        orte_grpcomm_pmi_component_query /* module query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/* Open the component */
int orte_grpcomm_pmi_open(void)
{
    return ORTE_SUCCESS;
}

int orte_grpcomm_pmi_close(void)
{
#if WANT_CRAY_PMI2_EXT
    if (PMI2_Initialized()) {
        PMI2_Finalize();
    }
#else
    PMI_BOOL initialized;

    /* if we weren't selected, cleanup */
    if (PMI_SUCCESS == PMI_Initialized(&initialized) &&
        PMI_TRUE == initialized) {
        PMI_Finalize();
    }
#endif

    return ORTE_SUCCESS;
}

static bool pmi_startup(void)
{
#if WANT_CRAY_PMI2_EXT
    int spawned, size, rank, appnum;

    if (PMI2_Initialized()) {
        /* already initialized */
        return true;
    }
    /* if we can't startup PMI, we can't be used */
    if (PMI_SUCCESS != PMI2_Init(&spawned, &size, &rank, &appnum)) {
        return false;
    }
    /* ignore the info - we'll pick it up elsewhere */
    return true;
#else
    PMI_BOOL initialized;

    if (PMI_SUCCESS != PMI_Init(&initialized)) {
        return false;
    }
    if (PMI_TRUE != initialized) {
        if (PMI_SUCCESS != PMI_Init(&initialized)) {
            return false;
        }
    }
    return true;
#endif
}

int orte_grpcomm_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* for now, only use PMI when direct launched */
    if (!ORTE_PROC_IS_HNP &&
        NULL == orte_process_info.my_hnp_uri &&
        pmi_startup()) {
        /* if PMI is available, use it */
        *priority = 100;
        *module = (mca_base_module_t *)&orte_grpcomm_pmi_module;
        return ORTE_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
