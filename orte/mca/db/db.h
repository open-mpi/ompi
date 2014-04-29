/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
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

#ifndef ORTE_DB_H
#define ORTE_DB_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/dss/dss_types.h"

/**
 * DATABASE DESIGN
 *
 * DB APIs are non-blocking and executed by pushing the request onto the ORTE
 * event base. Upon completion, the provided cbfunc will be called to return
 * the status resulting from the operation (a NULL cbfunc is permitted). The
 * cbfunc is responsible for releasing the returned list
 */

BEGIN_C_DECLS

/* forward declare */
struct orte_db_base_module_t;

/* callback function for async requests */
typedef void (*orte_db_callback_fn_t)(int dbhandle, int status,
                                      opal_list_t *kvs, void *cbdata);

/*
 * Initialize the module
 */
typedef int (*orte_db_base_module_init_fn_t)(struct orte_db_base_module_t *imod);

/*
 * Finalize the module
 */
typedef void (*orte_db_base_module_finalize_fn_t)(struct orte_db_base_module_t *imod);

/*
 * Open a database
 * 
 * Open a database for access (read, write, etc.). The request
 * can contain a user-specified name for this database that
 * has nothing to do with the backend database - it is solely
 * for use as a debug tool to help identify the database. The
 * request can also optionally provide a list of opal_value_t
 * properties - this is where one might specify the name of
 * the backend database, a URI for contacting it, the name of
 * a particular table for request, etc. Thus, it is important
 * to note that the returned "handle" is associated solely with
 * the defined request - i.e., if the properties specify a database
 * and table, then the handle will be specific to that combination.
 *
 * NOTE: one special "property" allows you to specify the
 * name(s) of the component(s) you want considered for this
 * handle - i.e., the equivalent of specifying the MCA param
 * "db=list" - using the  reserved property name "components".
 * The components will be queried in the order specified. The ^
 * character is also supported, with the remaining components
 * considered in priority order
 *
 * Just like the standard POSIX file open, the call will return
 * a unique "handle" that must be provided with any subsequent
 * call to store or fetch data from this database. 
 */
typedef void (*orte_db_base_API_open_fn_t)(char *name,
                                           opal_list_t *properties,
                                           orte_db_callback_fn_t cbfunc,
                                           void *cbdata);

/*
 * Close a database handle
 *
 * Close the specified database handle. This may or may not invoke
 * termination of a connection to a remote database or release of
 * memory storage, depending on the precise implementation of the
 * active database components. A -1 handle indicates that ALL open
 * database handles are to be closed.
 */
typedef void (*orte_db_base_API_close_fn_t)(int dbhandle,
                                            orte_db_callback_fn_t cbfunc,
                                            void *cbdata);

/*
 * Store one or more data elements against the primary key  - overwrites any data
 * of matching key that is already present. The data is copied into the database
 * and therefore does not need to be preserved by the caller.
 */
typedef void (*orte_db_base_API_store_fn_t)(int dbhandle,
                                            const char *primary_key,
                                            opal_list_t *kvs,
                                            orte_db_callback_fn_t cbfunc,
                                            void *cbdata);
typedef int (*orte_db_base_module_store_fn_t)(struct orte_db_base_module_t *imod,
                                              const char *primary_key,
                                              opal_list_t *kvs);

/*
 * Commit data to the database - action depends on implementation within
 * each active component
 */
typedef void (*orte_db_base_API_commit_fn_t)(int dbhandle,
                                             orte_db_callback_fn_t cbfunc,
                                             void *cbdata);
typedef void (*orte_db_base_module_commit_fn_t)(struct orte_db_base_module_t *imod);

/*
 * Retrieve data
 *
 * Retrieve data for the given primary key associated with the specified key. Wildcards
 * are supported here as well. Caller is responsible for releasing the returned list
 * of opal_keyval_t objects.
 */
typedef void (*orte_db_base_API_fetch_fn_t)(int dbhandle,
                                            const char *primary_key,
                                            const char *key,
                                            opal_list_t *kvs,
                                            orte_db_callback_fn_t cbfunc,
                                            void *cbdata);
typedef int (*orte_db_base_module_fetch_fn_t)(struct orte_db_base_module_t *imod,
                                              const char *primary_key,
                                              const char *key,
                                              opal_list_t *kvs);
/*
 * Delete data
 *
 * Delete the data for the given primary key that is associated with the specified key.
 * If a NULL key is provided, all data for the given primary key will be deleted.
 */
typedef void (*orte_db_base_API_remove_fn_t)(int dbhandle,
                                             const char *primary_key,
                                             const char *key,
                                             orte_db_callback_fn_t cbfunc,
                                             void *cbdata);
typedef int (*orte_db_base_module_remove_fn_t)(struct orte_db_base_module_t *imod,
                                               const char *primary_key,
                                               const char *key);

/*
 * the standard module data structure
 */
typedef struct  {
    orte_db_base_module_init_fn_t                      init;
    orte_db_base_module_finalize_fn_t                  finalize;
    orte_db_base_module_store_fn_t                     store;
    orte_db_base_module_commit_fn_t                    commit;
    orte_db_base_module_fetch_fn_t                     fetch;
    orte_db_base_module_remove_fn_t                    remove;
} orte_db_base_module_t;

typedef struct {
    orte_db_base_API_open_fn_t                      open;
    orte_db_base_API_close_fn_t                     close;
    orte_db_base_API_store_fn_t                     store;
    orte_db_base_API_commit_fn_t                    commit;
    orte_db_base_API_fetch_fn_t                     fetch;
    orte_db_base_API_remove_fn_t                    remove;
} orte_db_API_module_t;


/* function to determine if this component is available for use.
 * Note that we do not use the standard component open
 * function as we do not want/need return of a module.
 */
typedef bool (*mca_db_base_component_avail_fn_t)(void);

/* create and return a database module */
typedef orte_db_base_module_t* (*mca_db_base_component_create_hdl_fn_t)(opal_list_t *props);

/* provide a chance for the component to finalize */
typedef void (*mca_db_base_component_finalize_fn_t)(void);

typedef struct {
    mca_base_component_t                  base_version;
    mca_base_component_data_t             base_data;
    int                                   priority;
    mca_db_base_component_avail_fn_t      available;
    mca_db_base_component_create_hdl_fn_t create_handle;
    mca_db_base_component_finalize_fn_t   finalize;
} orte_db_base_component_t;

/*
 * Macro for use in components that are of type db
 */
#define ORTE_DB_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "db", 2, 0, 0

/* Global structure for accessing DB functions */
ORTE_DECLSPEC extern orte_db_API_module_t orte_db;  /* holds API function pointers */

END_C_DECLS

#endif
