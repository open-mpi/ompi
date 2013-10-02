/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Database Framework
 *
 */

#ifndef OPAL_DB_H
#define OPAL_DB_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/db/db_types.h"

/**
 * DATABASE DESIGN
 *
 * Data is always associated with a given opal identifier. Individual
 * modules may store the data local to the calling process, or on one
 * or more remote sites. Time lags between when data is written and
 * when it is available at a remote proc will therefore exist.
 */

BEGIN_C_DECLS

/*
 * Initialize the module
 */
typedef int (*opal_db_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef void (*opal_db_base_module_finalize_fn_t)(void);

/*
 * Set local identifier - pass in an opal_identifier_t value
 * that identifies this process. Used to determine whether or
 * not to publish values outside the process. Values stored
 * for other processes are never published as it is assumed
 * that each process is responsible for determining its own
 * need to publish a given piece of data
 */
typedef void (*opal_db_base_module_set_id_fn_t)(const opal_identifier_t *proc);

/*
 * Store a copy of data in the database - overwrites if already present. The
 * data is copied into the database and therefore does not need to be preserved
 * by the caller. The scope of the data determines where it is stored:
 *
 * - if the proc id is NOT my own, or the scope is INTERNAL, then the
 *   data is stored in my own internal storage system. Data for procs
 *   other than myself is NEVER published to the outside world
 *
 * - if the proc id is my own, and the scope is LOCAL, then the data
 *   is both stored internally AND pushed to the outside world. If the
 *   external API supports node-local operations, then the data will
 *   only be pushed to procs that are on the same node as ourselves.
 *   Otherwise, the data will be published GLOBAL.
 *
 * - if the proc id is my own, and the scope is GLOBAL, then the data
 *   is both stored internally AND pushed to the outside world.
&
 */
typedef int (*opal_db_base_module_store_fn_t)(const opal_identifier_t *proc,
                                              opal_scope_t scope,
                                              const char *key, const void *data,
                                              opal_data_type_t type);

/*
 * Store a pointer to data in the database - data must be retained by the user.
 * This allows users to share data across the code base without consuming
 * additional memory, but while retaining local access. Scope rules are
 * as outlined above
 */
typedef int (*opal_db_base_module_store_pointer_fn_t)(const opal_identifier_t *proc,
                                                      opal_value_t *kv);

/*
 * Commit data to the database - used to generate a commit of data
 * to an external key-value store such as PMI
 */
typedef void (*opal_db_base_module_commit_fn_t)(const opal_identifier_t *proc);

/*
 * Retrieve data
 *
 * Retrieve data for the given proc associated with the specified key. Wildcards
 * are supported here as well. Caller is responsible for releasing any returned
 * object.
 */
typedef int (*opal_db_base_module_fetch_fn_t)(const opal_identifier_t *proc,
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
typedef int (*opal_db_base_module_fetch_pointer_fn_t)(const opal_identifier_t *proc,
                                                      const char *key,
                                                      void **data, opal_data_type_t type);
/*
 * Retrieve multiple data elements
 *
 * Retrieve data for the given proc associated with the specified key. Wildcards
 * are supported here as well (key==NULL implies return all key-value pairs). Caller
 * is responsible for releasing the objects on the list. Scope rules are as described above.
 * A NULL identifer parameter indicates that data for all procs is to be returned.
 * The scope of the data is matched against the scope of the data when
 * stored. Note that a call to fetch data with a GLOBAL scope will return data
 * that was stored as PEER or NON_PEER, but not data stored as INTERNAL. A scope
 * of ALL will return data stored under any scope.
 */
typedef int (*opal_db_base_module_fetch_multiple_fn_t)(const opal_identifier_t *proc,
                                                       opal_scope_t scope,
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
typedef int (*opal_db_base_module_remove_fn_t)(const opal_identifier_t *proc, const char *key);

/*
 * Log data
 *
 * Insert statistical, non-process oriented data into a logging system.
 */
typedef int (*opal_db_base_module_add_log_fn_t)(const char *table, const opal_value_t *kvs, int nkvs);

/*
 * the standard module data structure
 */
struct opal_db_base_module_1_0_0_t {
    opal_db_base_module_init_fn_t                      init;
    opal_db_base_module_finalize_fn_t                  finalize;
    opal_db_base_module_set_id_fn_t                    set_id;
    opal_db_base_module_store_fn_t                     store;
    opal_db_base_module_store_pointer_fn_t             store_pointer;
    opal_db_base_module_commit_fn_t                    commit;
    opal_db_base_module_fetch_fn_t                     fetch;
    opal_db_base_module_fetch_pointer_fn_t             fetch_pointer;
    opal_db_base_module_fetch_multiple_fn_t            fetch_multiple;
    opal_db_base_module_remove_fn_t                    remove;
    opal_db_base_module_add_log_fn_t                   add_log;
};
typedef struct opal_db_base_module_1_0_0_t opal_db_base_module_1_0_0_t;
typedef struct opal_db_base_module_1_0_0_t opal_db_base_module_t;

/* we need to get two priorities back from our components, so
 * define a customized query function for our use
 */
typedef int (*opal_db_component_query_fn_t)(opal_db_base_module_t **module,
                                            int *store_priority,
                                            int *fetch_priority,
                                            bool restrict_local);
/*
 * the standard component data structure
 */
struct opal_db_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
    opal_db_component_query_fn_t query;
};
typedef struct opal_db_base_component_1_0_0_t opal_db_base_component_1_0_0_t;
typedef struct opal_db_base_component_1_0_0_t opal_db_base_component_t;

/*
 * Macro for use in components that are of type db
 */
#define OPAL_DB_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "db", 1, 0, 0

/* Global structure for accessing DB functions */
OPAL_DECLSPEC extern opal_db_base_module_t opal_db;  /* holds base function pointers */

END_C_DECLS

#endif
