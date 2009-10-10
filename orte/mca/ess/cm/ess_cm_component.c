/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 *
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

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/cm/ess_cm.h"

extern orte_ess_base_module_t orte_ess_cm_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_cm_component_t mca_ess_cm_component = {
    {
        {
            ORTE_ESS_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            "cm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            orte_ess_cm_component_open,
            orte_ess_cm_component_close,
            orte_ess_cm_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


int
orte_ess_cm_component_open(void)
{
    mca_base_component_t *c = &mca_ess_cm_component.super.base_version;

    mca_base_param_reg_int(c, "max_slots",
                           "Max #slots/rack (must be > 0)",
                           false, false, 38, &mca_ess_cm_component.max_slots);
    
    return ORTE_SUCCESS;
}


int orte_ess_cm_component_query(mca_base_module_t **module, int *priority)
{
#if ORTE_ENABLE_MULTICAST
    char *spec;
    
    /* only select us if specified */
    spec = getenv("OMPI_MCA_ess");
    if (NULL == spec || 0 != strcmp("cm", spec)) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    *priority = 1000;
    *module = (mca_base_module_t *)&orte_ess_cm_module;
    return ORTE_SUCCESS;
#else
    /* cannot be used */
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
#endif
}


int
orte_ess_cm_component_close(void)
{
    return ORTE_SUCCESS;
}

