/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE Database Framework
 *
 */

#ifndef ORTE_DB_H
#define ORTE_DB_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/db/db_types.h"

/**
 * DATABASE DESIGN
 *
 * Data is always associated with a given orte process name. Individual
 * modules may store the data local to the calling process, or on one
 * or more remote sites. Time lags between when data is written and
 * when it is available at a remote proc will therefore exist.
 */

BEGIN_C_DECLS

/*
 * Initialize the module
 */
typedef int (*orte_db_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef void (*orte_db_base_module_finalize_fn_t)(void);

/*
 * Store a copy of data in the database - overwrites if already present. The data is
 * copied into the database and therefore does not need to be preserved by
 * the caller.
 */
typedef int (*orte_db_base_module_store_fn_t)(const orte_process_name_t *proc,
                                              const char *key, const void *data, opal_data_type_t type);

/*
 * Store a pointer to data in the database - data must be retained by the user.
 * This allows users to share data across the code base without consuming
 * additional memory, but while retaining local access
 */
typedef int (*orte_db_base_module_store_pointer_fn_t)(const orte_process_name_t *proc,
                                                      opal_value_t *kv);

/*
 * Retrieve data
 *
 * Retrieve data for the given proc associated with the specified key. Wildcards
 * are supported here as well. Caller is responsible for releasing any returned
 * object.
 */
typedef int (*orte_db_base_module_fetch_fn_t)(const orte_process_name_t *proc,
                                              const char *key,
                                              void **data, opal_data_type_t type);

/*
 * Retrieve a pointer to data
 *
 * Retrieve a pointer to the data for the given proc associated with the specified key. Wildcards
 * are supported here as well. Callers are cautioned against modifying the data as this
 * will directly alter information in the database! A local copy of the data should be made
 * wherever modification is possible.
 */
typedef int (*orte_db_base_module_fetch_pointer_fn_t)(const orte_process_name_t *proc,
                                                      const char *key,
                                                      void **data, opal_data_type_t type);
/*
 * Retrieve multiple data elements
 *
 * Retrieve data for the given proc associated with the specified key. Wildcards
 * are supported here as well. Caller is responsible for releasing the objects on the list.
 */
typedef int (*orte_db_base_module_fetch_multiple_fn_t)(const orte_process_name_t *proc,
                                                       const char *key,
                                                       opal_list_t *kvs);

/*
 * Delete data
 *
 * Delete the data associated with the specified key. If a NULL key is provided,
 * all data for the given proc will be deleted.
 *
 * This function also supports wildcard values in the proc field. A NULL proc indicates
 * that ALL data in the database is to be purged. A WILDCARD vpid will delete all matching
 * keys from that jobid. Etc.
 */
typedef int (*orte_db_base_module_remove_fn_t)(const orte_process_name_t *proc, const char *key);

/*
 * Log data
 *
 * Insert statistical, non-process oriented data into a logging system.
 */
typedef int (*orte_db_base_module_add_log_fn_t)(const char *table, const opal_value_t *kvs, int nkvs);

/*
 * the standard module data structure
 */
struct orte_db_base_module_1_0_0_t {
    orte_db_base_module_init_fn_t                      init;
    orte_db_base_module_finalize_fn_t                  finalize;
    orte_db_base_module_store_fn_t                     store;
    orte_db_base_module_store_pointer_fn_t             store_pointer;
    orte_db_base_module_fetch_fn_t                     fetch;
    orte_db_base_module_fetch_pointer_fn_t             fetch_pointer;
    orte_db_base_module_fetch_multiple_fn_t            fetch_multiple;
    orte_db_base_module_remove_fn_t                    remove;
    orte_db_base_module_add_log_fn_t                   add_log;
};
typedef struct orte_db_base_module_1_0_0_t orte_db_base_module_1_0_0_t;
typedef struct orte_db_base_module_1_0_0_t orte_db_base_module_t;

/*
 * the standard component data structure
 */
struct orte_db_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_db_base_component_1_0_0_t orte_db_base_component_1_0_0_t;
typedef struct orte_db_base_component_1_0_0_t orte_db_base_component_t;

/*
 * Macro for use in components that are of type db
 */
#define ORTE_DB_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "db", 1, 0, 0

/* Global structure for accessing DB functions */
ORTE_DECLSPEC extern orte_db_base_module_t orte_db;  /* holds selected module's function pointers */

END_C_DECLS

#endif
