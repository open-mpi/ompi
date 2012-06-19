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

/**
 * DATABASE DESIGN
 *
 * Data is always associated with a given orte process name. Individual
 * modules may store the data local to the calling process, or on one
 * or more remote sites. Time lags between when data is written and
 * when it is available at a remote proc will therefore exist.
 */

BEGIN_C_DECLS

/**
 * Container for data for a particular key-value pair
 */
typedef struct orte_db_keyval_t {
    /** Structure can be put on lists */
    opal_list_item_t super;
    /** Key */
    char *key;
    /** Byte object containing binary blob of data associated with this proc,key pair */
    opal_byte_object_t value;
} orte_db_keyval_t;
OBJ_CLASS_DECLARATION(orte_db_keyval_t);

/* define the callback function for returning data - note that
 * the memory backing the data belongs to the DB framework. The
 * receiver must NOT release it
 */
typedef void (*orte_db_fetch_callback_fn_t)(orte_process_name_t *src,
                                            char *key,
                                            orte_db_keyval_t *data,
                                            int num_entries);

/*
 * Initialize the module
 */
typedef int (*orte_db_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef void (*orte_db_base_module_finalize_fn_t)(void);

/*
 * Store data in the database - overwrites if already present. The data is
 * copied into the database and therefore does not need to be preserved by
 * the caller. Note that this is a non-blocking call - if data is stored
 * offsite, the transfer will occur in the background.
 */
typedef int (*orte_db_base_module_store_fn_t)(const orte_process_name_t *proc,
                                              const char *key,
                                              const void *object, int32_t size);

/*
 * Retrieve data
 *
 * Retrieve the data for the given proc associated with the specified key. Wildcards
 * are supported here as well. This is a non-blocking
 * call - data will be returned via the callback function ONCE IT BECOMES AVAILABLE. Use
 * of the "timeout" MCA parameter is encouraged to avoid hanging on fetch requests for
 * "blocking" data that can never be resolved.
 *
 * NOTE: INTERIM IMPLEMENTATION WILL SIMPLY LOOKUP EXISTING DATA, RETURNING AN ERROR IF
 * NOT ALREADY PRESENT.
 */
typedef int (*orte_db_base_module_fetch_fn_t)(const orte_process_name_t *proc,
                                              const char *key,
                                              opal_list_t *values);

/*
 * Delete data
 *
 * Delete the data associated with the specified key. If a NULL key is provided,
 * all data for the given proc will be deleted.
 *
 * This function also supports wildcard values in the proc field. A NULL proc indicates
 * that ALL data in the database is to be purged. A WILDCARD vpid will delete all matching
 * keys from that jobid. Etc.
 *
 * Note that this is a non-blocking call - data stored off-site will be deleted asynchronously.
 */
typedef int (*orte_db_base_module_remove_fn_t)(const orte_process_name_t *proc, const char *key);

/*
 * the standard module data structure
 */
struct orte_db_base_module_1_0_0_t {
    orte_db_base_module_init_fn_t           init;
    orte_db_base_module_finalize_fn_t       finalize;
    orte_db_base_module_store_fn_t          store;
    orte_db_base_module_fetch_fn_t          fetch;
    orte_db_base_module_remove_fn_t         remove;
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
