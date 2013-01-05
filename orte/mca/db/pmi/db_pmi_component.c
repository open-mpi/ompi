/*
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
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

#include "opal/mca/base/base.h"

#include "orte/mca/common/pmi/common_pmi.h"
#include "orte/util/proc_info.h"

#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_pmi.h"

static int db_pmi_component_open(void);
static int db_pmi_component_query(mca_base_module_t **module, int *priority);
static int db_pmi_component_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_base_component_t mca_db_pmi_component = {
    {
        ORTE_DB_BASE_VERSION_1_0_0,

        /* Component name and version */
        "pmi",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        db_pmi_component_open,
        db_pmi_component_close,
        db_pmi_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int db_pmi_component_open(void)
{
    return ORTE_SUCCESS;
}

static int db_pmi_component_query(mca_base_module_t **module, int *priority)
{
    /* only use PMI if available - the ESS pmi module
     * will force our selection if we are direct-launched
     */
    if (mca_common_pmi_init()) {
        *priority = 10;
        *module = (mca_base_module_t*)&orte_db_pmi_module;
        return ORTE_SUCCESS;
    }

    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}


static int db_pmi_component_close(void)
{
    mca_common_pmi_finalize();
    return ORTE_SUCCESS;
}

