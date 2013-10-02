/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
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
#include "db_hash.h"

static int db_hash_component_open(void);
static int db_hash_component_query(opal_db_base_module_t **module,
                                   int *store_priority,
                                   int *fetch_priority,
                                   bool restrict_local);
static int db_hash_component_close(void);
static int db_hash_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_db_base_component_t mca_db_hash_component = {
    {
        OPAL_DB_BASE_VERSION_1_0_0,

        /* Component name and version */
        "hash",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        db_hash_component_open,
        db_hash_component_close,
        NULL,
        db_hash_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    db_hash_component_query
};

/* we should be the last place to store data as
 * it usually is stored globally, then can fall
 * down to us if it is internal
 */
static int my_store_priority = 1;
/* we should be the first place to look for data
 * in case we already have it - then try to fetch
 * it globally if we don't
 */
static int my_fetch_priority = 100;

static int db_hash_component_open(void)
{
    return OPAL_SUCCESS;
}

static int db_hash_component_query(opal_db_base_module_t **module,
                                   int *store_priority,
                                   int *fetch_priority,
                                   bool restrict_local)
{
    /* we are the default - the ESS modules will set the db selection
     * envar if they need someone else
     */
    *store_priority = my_store_priority;
    *fetch_priority = my_fetch_priority;
    *module = &opal_db_hash_module;
    return OPAL_SUCCESS;
}


static int db_hash_component_close(void)
{
    return OPAL_SUCCESS;
}

static int db_hash_component_register(void)
{
    mca_base_component_t *c = &mca_db_hash_component.base_version;

    my_store_priority = 1;
    (void) mca_base_component_var_register(c, "store_priority",
                                           "Priority dictating order in which store commands will given to database components",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_store_priority);

    my_fetch_priority = 100;
    (void) mca_base_component_var_register(c, "fetch_priority",
                                           "Priority dictating order in which fetch commands will given to database components",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_fetch_priority);

    return OPAL_SUCCESS;
}
