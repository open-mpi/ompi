/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
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

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"

#include "opal/mca/db/db.h"
#include "opal/mca/db/base/base.h"
#include "db_print.h"

extern opal_db_base_module_t opal_db_print_module;

static int print_component_register(void);
static int print_component_open(void);
static int print_component_close(void);
static int print_component_query(opal_db_base_module_t **module,
                                 int *store_priority,
                                 int *fetch_priority,
                                 bool restrict_local);
static int print_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_db_print_component_t mca_db_print_component = {
    {
        {
            OPAL_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "print",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */
            print_component_open,
            print_component_close,
            NULL,
            print_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        print_component_query
    }
};

static int print_component_open(void)
{
    return OPAL_SUCCESS;
}

/* this component is NEVER used for store or fetch */
static int print_component_query(opal_db_base_module_t **module,
                                 int *store_priority,
                                 int *fetch_priority,
                                 bool restrict_local)
{
    if (NULL == mca_db_print_component.filename) {
        *store_priority = 0;
        *fetch_priority = 0;
        *module = NULL;
        return OPAL_ERROR;
    }
    *store_priority = 0;
    *fetch_priority = 0;
    *module = &opal_db_print_module;
    return OPAL_SUCCESS;
}


static int print_component_close(void)
{
    return OPAL_SUCCESS;
}

static int print_component_register(void)
{
    mca_db_print_component.filename = NULL;
    (void) mca_base_component_var_register (&mca_db_print_component.super.base_version,
                                            "file", "Print to the indicated file (- => stdout, + => stderr)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_print_component.filename);

    return OPAL_SUCCESS;
}

