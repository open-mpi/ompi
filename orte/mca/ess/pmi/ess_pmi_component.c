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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/runtime/opal_params.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/pmi/ess_pmi.h"

extern orte_ess_base_module_t orte_ess_pmi_module;

static int pmi_component_open(void);
static int pmi_component_close(void);
static int pmi_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_pmi_component = {
    {
        ORTE_ESS_BASE_VERSION_3_0_0,

        /* Component name and version */
        "pmi",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        pmi_component_open,
        pmi_component_close,
        pmi_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int pmi_component_open(void)
{
    return ORTE_SUCCESS;
}

static int pmi_component_query(mca_base_module_t **module, int *priority)
{
    int ret;
    
    /* all APPS must use pmix */
    if (ORTE_PROC_IS_APP) {
        /* open and setup pmix */
        if (NULL == opal_pmix.initialized) {
            if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
                ORTE_ERROR_LOG(ret);
                *priority = -1;
                *module = NULL;
                return ret;
            }
            if (OPAL_SUCCESS != (ret = opal_pmix_base_select())) {
                /* don't error log this as it might not be an error at all */
                *priority = -1;
                *module = NULL;
                (void) mca_base_framework_close(&opal_pmix_base_framework);
                return ret;
            }
        }
        if (!opal_pmix.initialized()) {
            /* we may have everything setup, but we are not
             * in a PMIx environment and so we need to disqualify
             * ourselves - we are likely a singleton and will
             * pick things up from there */
            *priority = -1;
            *module = NULL;
            return ORTE_ERROR;
        }
        *priority = 35;
        *module = (mca_base_module_t *)&orte_ess_pmi_module;
        return ORTE_SUCCESS;
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


static int pmi_component_close(void)
{
    return ORTE_SUCCESS;
}

