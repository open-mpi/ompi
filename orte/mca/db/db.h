/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE State Save/Recovery Service
 *
 */

#ifndef ORTE_DB_H
#define ORTE_DB_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

/*
 * API functions
 */

typedef uint8_t orte_db_cmd_t;
#define ORTE_DB_CMD_T OPAL_UINT8

#define ORTE_DB_STORE_CMD       0x01
#define ORTE_DB_FETCH_CMD       0X02
#define ORTE_DB_UPDATE_CMD      0x03
#define ORTE_DB_REMOVE_CMD      0x04

/*
 * Initialize the module
 */
typedef int (*orte_db_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef int (*orte_db_base_module_finalize_fn_t)(void);

/*
 * Save the db of the provided object
 */
typedef int (*orte_db_base_module_store_fn_t)(char *key, void *object, opal_data_type_t type);

/*
 * Set the source for recovering db info
 */
typedef int (*orte_db_base_module_set_source_fn_t)(orte_process_name_t *name);

/*
 * Retrieve data
 */
typedef int (*orte_db_base_module_fetch_fn_t)(char *key, void *object, opal_data_type_t type);

/*
 * Update data
 */
typedef int (*orte_db_base_module_update_fn_t)(char *key, void *object, opal_data_type_t type);

/*
 * Delete data
 */
typedef int (*orte_db_base_module_remove_fn_t)(char *key);

/*
 * the standard module data structure
 */
struct orte_db_base_module_1_0_0_t {
    orte_db_base_module_init_fn_t           init;
    orte_db_base_module_finalize_fn_t       finalize;
    orte_db_base_module_store_fn_t          store;
    orte_db_base_module_set_source_fn_t     set_source;
    orte_db_base_module_fetch_fn_t          fetch;
    orte_db_base_module_update_fn_t         update;
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
