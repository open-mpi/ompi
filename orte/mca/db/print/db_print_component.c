/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
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
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_print.h"

extern orte_db_base_module_t orte_db_print_module;

static int print_component_open(void);
static int print_component_close(void);
static int print_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_print_component_t mca_db_print_component = {
    {
        {
            ORTE_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "print",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            print_component_open,
            print_component_close,
            print_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int print_component_open(void)
{
    mca_base_component_t *c = &mca_db_print_component.super.base_version;

    mca_base_param_reg_string(c, "file",
                           "Print to the indicated file (- => stdout, + => stderr)",
                           false, false, NULL,  &mca_db_print_component.filename);
    return ORTE_SUCCESS;
}


static int print_component_query(mca_base_module_t **module, int *priority)
{
    if (NULL == mca_db_print_component.filename) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    /* put us at the top of the list */
    *priority = 100;
    *module = (mca_base_module_t*)&orte_db_print_module;
    return ORTE_SUCCESS;
}


static int print_component_close(void)
{
    if (NULL != mca_db_print_component.filename) {
        free(mca_db_print_component.filename);
    }
    return ORTE_SUCCESS;
}

