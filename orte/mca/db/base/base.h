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
 */

#ifndef MCA_DB_BASE_H
#define MCA_DB_BASE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

/**
 * Open the db framework
 */
ORTE_DECLSPEC int orte_db_base_open(void);

/**
 * Select a db module
 */
ORTE_DECLSPEC int orte_db_base_select(void);

/**
 * Close the db framework
 */
ORTE_DECLSPEC int orte_db_base_close(void);

typedef struct {
    opal_list_item_t super;
    int pri;
    orte_db_base_module_t *module;
    mca_base_component_t *component;
} orte_db_active_module_t;
OBJ_CLASS_DECLARATION(orte_db_active_module_t);

typedef struct {
    int output;
    opal_list_t available_components;
    opal_list_t active_modules;
} orte_db_base_t;
ORTE_DECLSPEC extern orte_db_base_t orte_db_base;

ORTE_DECLSPEC int orte_db_base_store(const orte_process_name_t *proc,
                                     const char *key, const void *object,
                                     opal_data_type_t type);
ORTE_DECLSPEC int orte_db_base_store_pointer(const orte_process_name_t *proc,
                                             opal_value_t *kv);
ORTE_DECLSPEC int orte_db_base_fetch(const orte_process_name_t *proc,
                                     const char *key, void **data,
                                     opal_data_type_t type);
ORTE_DECLSPEC int orte_db_base_fetch_pointer(const orte_process_name_t *proc,
                                             const char *key,
                                             void **data, opal_data_type_t type);
ORTE_DECLSPEC int orte_db_base_fetch_multiple(const orte_process_name_t *proc,
                                              const char *key,
                                              opal_list_t *kvs);
ORTE_DECLSPEC int orte_db_base_remove_data(const orte_process_name_t *proc,
                                           const char *key);

ORTE_DECLSPEC int orte_db_base_add_log(const char *table,
                                       const opal_value_t *kvs, int nkvs);


END_C_DECLS

#endif
