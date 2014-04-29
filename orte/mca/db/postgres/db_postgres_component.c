/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
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
#include "opal/mca/base/mca_base_var.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_postgres.h"

static int component_register(void);
static bool component_avail(void);
static orte_db_base_module_t *component_create(opal_list_t *props);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_base_component_t mca_db_postgres_component = {
    {
        ORTE_DB_BASE_VERSION_2_0_0,

        /* Component name and version */
        "postgres",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        NULL,
        component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    75,
    component_avail,
    component_create,
    NULL
};

static int num_worker_threads;
static char *dbname;
static char *table;
static char *user;
static char *pguri;
static char *pgoptions;
static char *pgtty;

static int component_register(void) {
    mca_base_component_t *c = &mca_db_postgres_component.base_version;

    /* retrieve the name of the database to be used */
    dbname = NULL;
    (void) mca_base_component_var_register (c, "database", "Name of database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &dbname);

    /* retrieve the name of the table to be used */
    table = NULL;
    (void) mca_base_component_var_register (c, "table", "Name of table",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &table);
     
    /* retrieve the name of the user to be used */
    user = NULL;
    (void) mca_base_component_var_register (c, "user", "Name of database user:password",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &user);
    
    /* retrieve the server:port */
    pguri = NULL;
    (void) mca_base_component_var_register (c, "uri", "Contact info for Postgres server as ip:port",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &pguri);
    
    /* retrieve any options to be used */
    pgoptions = NULL;
    (void) mca_base_component_var_register (c, "options", "Options to pass to the database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &pgoptions);
    
    /* retrieve the tty argument */
    pgtty = NULL;
    (void) mca_base_component_var_register (c, "tty", "TTY option for database",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &pgtty);
    
    /* retrieve the number of worker threads to be used */
    num_worker_threads = -1;
    (void) mca_base_component_var_register (c, "num_worker_threads",
                                            "Number of worker threads to be used",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &num_worker_threads);

    return OPAL_SUCCESS;
}

static bool component_avail(void)
{
    /* always available */
    return true;
}

static orte_db_base_module_t *component_create(opal_list_t *props)
{
    mca_db_postgres_module_t *mod;
    opal_value_t *kv;

    mod = (mca_db_postgres_module_t*)malloc(sizeof(mca_db_postgres_module_t));
    if (NULL == mod) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    memset(mod, 0, sizeof(mca_db_postgres_module_t));
    mod->num_worker_threads = -1;

    /* copy the APIs across */
    memcpy(mod, &mca_db_postgres_module.api, sizeof(orte_db_base_module_t));

    /* if the props include db info, then use it */
    OPAL_LIST_FOREACH(kv, props, opal_value_t) {
        if (0 == strcmp(kv->key, "database")) {
            mod->dbname = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "table")) {
            mod->table = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "user")) {
            mod->user = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "uri")) {
            mod->pguri = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "options")) {
            mod->pgoptions = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "tty")) {
            mod->pgtty = strdup(kv->data.string);
        } else if (0 == strcmp(kv->key, "num_worker_threads")) {
            mod->num_worker_threads = kv->data.integer;
        }
    }
    if (NULL == mod->dbname) {
        if (NULL == dbname) {
            /* nothing was provided - opt out */
            free(mod);
            return NULL;
        }
        mod->dbname = strdup(dbname);
    }
    if (NULL == mod->table) {
        if (NULL == table) {
            /* nothing was provided - opt out */
            free(mod);
            return NULL;
        }
        mod->table = strdup(table);
    }
    /* all other entries are optional */
    if (NULL == mod->user && NULL != user) {
        mod->user = strdup(user);
    }
    if (NULL == mod->pguri && NULL != pguri) {
        mod->pguri = strdup(pguri);
    }
    if (NULL == mod->pgoptions && NULL != pgoptions) {
        mod->pgoptions = strdup(pgoptions);
    }
    if (NULL == mod->pgtty && NULL != pgtty) {
        mod->pgtty = strdup(pgtty);
    }
    if (0 > mod->num_worker_threads && 0 < num_worker_threads) {
        mod->num_worker_threads = num_worker_threads;
    }

    /* let the module init */
    if (ORTE_SUCCESS != mod->api.init((struct orte_db_base_module_t*)mod)) {
        mod->api.finalize((struct orte_db_base_module_t*)mod);
        free(mod);
        return NULL;
    }

    return (orte_db_base_module_t*)mod;
}
