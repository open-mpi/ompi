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

#include "opal/mca/pmix/pmix.h"
#include "opal/mca/pmix/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/singleton/ess_singleton.h"

extern orte_ess_base_module_t orte_ess_singleton_module;

char *orte_ess_singleton_server_uri = NULL;

static int
orte_ess_singleton_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_singleton_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        ORTE_ESS_BASE_VERSION_3_0_0,

        /* Component name and version */
        "singleton",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_singleton_component_open,
        orte_ess_singleton_component_close,
        orte_ess_singleton_component_query,
        orte_ess_singleton_component_register
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    }
};

static int
orte_ess_singleton_component_register(void)
{
    int ret;

    orte_ess_singleton_server_uri = NULL;
    ret = mca_base_component_var_register(&mca_ess_singleton_component.base_version,
                                          "server",
                                          "Server to be used as HNP - [file|FILE]:<filename> or just uri",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &orte_ess_singleton_server_uri);
    (void) mca_base_var_register_synonym(ret, "orte", NULL, NULL, "server", 0);

    return ORTE_SUCCESS;
}

int
orte_ess_singleton_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_ess_singleton_component_query(mca_base_module_t **module, int *priority)
{
    int ret;
    
    /* if we are an HNP, daemon, or tool, then we
     * are definitely not a singleton!
     */
    if (ORTE_PROC_IS_HNP ||
        ORTE_PROC_IS_DAEMON ||
        ORTE_PROC_IS_TOOL) {
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* okay, we still could be a singleton or
     * an application process. If we have been
     * given an HNP URI, then we are definitely
     * not a singleton
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* open and setup pmix */
    if (NULL == opal_pmix.initialized) {
        if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
            /* if PMIx is not available, then we are indeed a singleton */
            goto single;
        }
        if (OPAL_SUCCESS != (ret = opal_pmix_base_select())) {
            /* if PMIx is not available, then we are indeed a singleton */
            (void) mca_base_framework_close(&opal_pmix_base_framework);
            goto single;
        }
    }
    if (opal_pmix.initialized()) {
        /* we are in a PMI environment and are therefore
         * not a singleton */
        *priority = -1;
        *module = NULL;
        return ORTE_ERROR;
    }

  single:
    /* okay, we could still be an application process,
     * but launched in "standalone" mode - i.e., directly
     * launched by an environment instead of via mpirun.
     * We need to set our priority low so that any enviro
     * component will override us. If they don't, then we
     * want to be selected as we must be a singleton
     */
    *priority = 25;
    *module = (mca_base_module_t *)&orte_ess_singleton_module;
    return ORTE_SUCCESS;
}


int
orte_ess_singleton_component_close(void)
{
    return ORTE_SUCCESS;
}

