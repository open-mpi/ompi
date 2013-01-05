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
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_gpdb.h"

extern orte_db_base_module_t orte_db_gpdb_module;
char *orte_db_gpdb_file;

static int gpdb_component_open(void);
static int gpdb_component_close(void);
static int gpdb_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_gpdb_component_t mca_db_gpdb_component = {
    {
        {
            ORTE_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "gpdb",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            gpdb_component_open,
            gpdb_component_close,
            gpdb_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int gpdb_component_open(void)
{
    return ORTE_SUCCESS;
}


static int gpdb_component_query(mca_base_module_t **module, int *priority)
{
    mca_base_component_t *c = &mca_db_gpdb_component.super.base_version;

    /* retrieve the name of the database to be used */
    mca_base_param_reg_string(c, "database",
                              "Name of database",
                              false, false, NULL, &mca_db_gpdb_component.db_file);
    
    /* retrieve the number of worker threads to be used */
    mca_base_param_reg_int(c, "num_worker_threads",
                           "Number of worker threads to be used",
                           false, false, 0, &mca_db_gpdb_component.num_worker_threads);

    if (NULL != mca_db_gpdb_component.db_file) {
        *priority = 3;  /* ahead of sqlite3 */
        *module = (mca_base_module_t*)&orte_db_gpdb_module;
        return ORTE_SUCCESS;
    }
    
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}


static int gpdb_component_close(void)
{
    if (NULL != mca_db_gpdb_component.db_file) {
        free(mca_db_gpdb_component.db_file);
    }
    return ORTE_SUCCESS;
}

