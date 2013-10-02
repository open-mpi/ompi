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

#include <sys/stat.h>

#include "opal/mca/base/base.h"

#include "opal/util/show_help.h"

#include "opal/mca/db/db.h"
#include "opal/mca/db/base/base.h"
#include "db_sqlite.h"

extern opal_db_base_module_t opal_db_sqlite_module;
char *opal_db_sqlite_file;

static int sqlite_component_register(void);
static int sqlite_component_open(void);
static int sqlite_component_close(void);
static int sqlite_component_query(opal_db_base_module_t **module,
                                  int *store_priority,
                                  int *fetch_priority,
                                  bool restrict_local);
static int sqlite_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_db_sqlite_component_t mca_db_sqlite_component = {
    {
        {
            OPAL_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "sqlite",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */
            sqlite_component_open,
            sqlite_component_close,
            NULL,
            sqlite_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        sqlite_component_query
    }
};

static int sqlite_component_open(void)
{
    return OPAL_SUCCESS;
}

/* this component is NEVER used for store or fetch */
static int sqlite_component_query(opal_db_base_module_t **module,
                                  int *store_priority,
                                  int *fetch_priority,
                                  bool restrict_local)
{
    struct stat buf;

    *store_priority = 0;
    *fetch_priority = 0;

    if (NULL != mca_db_sqlite_component.db_file) {
        /* if the database file doesn't exist, then we can't operate */
        if (0 != stat(mca_db_sqlite_component.db_file, &buf)) {
            /* not found */
            opal_show_help("help-db-sqlite.txt", "file-not-found",
                           true, mca_db_sqlite_component.db_file);
            *module = NULL;
            return OPAL_ERROR;
        }
        *module = &opal_db_sqlite_module;
        return OPAL_SUCCESS;
    }
    
    *module = NULL;
    return OPAL_ERROR;
}


static int sqlite_component_close(void)
{
    return OPAL_SUCCESS;
}

static int sqlite_component_register(void)
{
    mca_base_component_t *c = &mca_db_sqlite_component.super.base_version;

    /* retrieve the name of the file to be used */
    mca_db_sqlite_component.db_file = NULL;
    (void) mca_base_component_var_register (c, "database", "Name of file to be used for database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_sqlite_component.db_file);
    
    /* retrieve the number of worker threads to be used, if sqlite3 is thread-safe */
    mca_db_sqlite_component.num_worker_threads = 0;
    (void) mca_base_component_var_register (c, "num_worker_threads", "Number of worker threads to be used",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_sqlite_component.num_worker_threads);

    return OPAL_SUCCESS;
}

