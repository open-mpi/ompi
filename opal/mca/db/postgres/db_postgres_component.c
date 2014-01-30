/*
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
#include "opal/mca/base/mca_base_var.h"

#include "opal/mca/db/db.h"
#include "opal/mca/db/base/base.h"
#include "db_postgres.h"

extern opal_db_base_module_t opal_db_postgres_module;
char *opal_db_postgres_file;

static int postgres_component_open(void);
static int postgres_component_close(void);
static int postgres_component_query(opal_db_base_module_t **module,
                                    int *store_priority,
                                    int *fetch_priority,
                                    bool restrict_local);
static int postgres_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_db_postgres_component_t mca_db_postgres_component = {
    {
        {
            OPAL_DB_BASE_VERSION_1_0_0,

            /* Component name and version */
            "postgres",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */
            postgres_component_open,
            postgres_component_close,
            NULL,
            postgres_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        postgres_component_query
    }
};


static int postgres_component_open(void)
{
    return OPAL_SUCCESS;
}


static int postgres_component_query(opal_db_base_module_t **module,
                                    int *store_priority,
                                    int *fetch_priority,
                                    bool restrict_local)
{

    if (NULL != mca_db_postgres_component.dbname &&
        NULL != mca_db_postgres_component.table &&
        NULL != mca_db_postgres_component.user &&
        NULL != mca_db_postgres_component.pguri) {
        *store_priority = 3; /* ahead of sqlite3 */
        *fetch_priority = 3;
        *module = &opal_db_postgres_module;
        return OPAL_SUCCESS;
    }

    *store_priority = 0;
    *fetch_priority = 0;
    *module = NULL;
    return OPAL_ERROR;
}


static int postgres_component_close(void)
{
    if (NULL != mca_db_postgres_component.dbname) {
        free(mca_db_postgres_component.dbname);
    }
    if (NULL != mca_db_postgres_component.table) {
        free(mca_db_postgres_component.table);
    }
    if (NULL != mca_db_postgres_component.user) {
        free(mca_db_postgres_component.user);
    }
    if (NULL != mca_db_postgres_component.pguri) {
        free(mca_db_postgres_component.pguri);
    }
    if (NULL != mca_db_postgres_component.pgoptions) {
        free(mca_db_postgres_component.pgoptions);
    }
    if (NULL != mca_db_postgres_component.pgtty) {
        free(mca_db_postgres_component.pgtty);
    }
    return OPAL_SUCCESS;
}

static int postgres_component_register(void) {
    mca_base_component_t *c = &mca_db_postgres_component.super.base_version;

    /* retrieve the name of the database to be used */
    mca_db_postgres_component.dbname = NULL;
    (void) mca_base_component_var_register (c, "database", "Name of database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.dbname);

    /* retrieve the name of the table to be used */
    mca_db_postgres_component.table = NULL;
    (void) mca_base_component_var_register (c, "table", "Name of table",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.table);
     
    /* retrieve the name of the user to be used */
    mca_db_postgres_component.user = NULL;
    (void) mca_base_component_var_register (c, "user", "Name of database user:password",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.table);
    
    /* retrieve the server:port */
    mca_db_postgres_component.pguri = NULL;
    (void) mca_base_component_var_register (c, "uri", "Contact info for Postgres server as ip:port",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.pguri);
    
    /* retrieve any options to be used */
    mca_db_postgres_component.pgoptions = NULL;
    (void) mca_base_component_var_register (c, "options", "Options to pass to the database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.pgoptions);
    
    /* retrieve the tty argument */
    mca_db_postgres_component.pgtty = NULL;
    (void) mca_base_component_var_register (c, "tty", "TTY option for database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.pgtty);
    
    /* retrieve the number of worker threads to be used */
     mca_db_postgres_component.num_worker_threads = 0;
    (void) mca_base_component_var_register (c, "num_worker_threads",
                                            "Number of worker threads to be used",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_db_postgres_component.num_worker_threads);

    return OPAL_SUCCESS;
}
