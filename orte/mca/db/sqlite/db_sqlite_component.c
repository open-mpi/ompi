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

#include <sys/stat.h>

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_sqlite.h"

extern orte_db_base_module_t orte_db_sqlite_module;
char *orte_db_sqlite_file;

static int sqlite_component_open(void);
static int sqlite_component_close(void);
static int sqlite_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_sqlite_component_t mca_db_sqlite_component = {
    {
        {
            ORTE_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "sqlite",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            sqlite_component_open,
            sqlite_component_close,
            sqlite_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int sqlite_component_open(void)
{
    return ORTE_SUCCESS;
}


static int sqlite_component_query(mca_base_module_t **module, int *priority)
{
    mca_base_component_t *c = &mca_db_sqlite_component.super.base_version;
    struct stat buf;

    /* retrieve the name of the file to be used */
    mca_base_param_reg_string(c, "database",
                              "Name of file to be used for database",
                              false, false, NULL, &mca_db_sqlite_component.db_file);
    
    /* retrieve the number of worker threads to be used, if sqlite3 is thread-safe */
    mca_base_param_reg_int(c, "num_worker_threads",
                           "Number of worker threads to be used",
                           false, false, 0, &mca_db_sqlite_component.num_worker_threads);

    if (NULL != mca_db_sqlite_component.db_file) {
        /* if the database file doesn't exist, then we can't operate */
        if (0 != stat(mca_db_sqlite_component.db_file, &buf)) {
            /* not found */
            orte_show_help("help-db-sqlite.txt", "file-not-found",
                           true, mca_db_sqlite_component.db_file);
            *priority = 0;
            *module = NULL;
            return ORTE_ERROR;
        }
        *priority = 1;
        *module = (mca_base_module_t*)&orte_db_sqlite_module;
        return ORTE_SUCCESS;
    }
    
    
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}


static int sqlite_component_close(void)
{
    if (NULL != mca_db_sqlite_component.db_file) {
        free(mca_db_sqlite_component.db_file);
    }
    return ORTE_SUCCESS;
}

