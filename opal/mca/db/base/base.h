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
 */

#ifndef MCA_DB_BASE_H
#define MCA_DB_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "opal/mca/db/db.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_db_base_framework;

/**
 * Select a db module
 */
OPAL_DECLSPEC int opal_db_base_select(bool restrict_local);

typedef struct {
    opal_list_item_t super;
    int pri;
    opal_db_base_module_t *module;
    opal_db_base_component_t *component;
} opal_db_active_module_t;
OBJ_CLASS_DECLARATION(opal_db_active_module_t);

typedef struct {
    opal_identifier_t my_id;
    bool id_set;
    opal_list_t store_order;
    opal_list_t fetch_order;
} opal_db_base_t;

OPAL_DECLSPEC extern opal_db_base_t opal_db_base;

OPAL_DECLSPEC void opal_db_base_set_id(const opal_identifier_t *proc);
OPAL_DECLSPEC int opal_db_base_store(const opal_identifier_t *proc,
                                     opal_scope_t scope,
                                     const char *key, const void *object,
                                     opal_data_type_t type);
OPAL_DECLSPEC int opal_db_base_store_pointer(const opal_identifier_t *proc,
                                             opal_value_t *kv);
OPAL_DECLSPEC int opal_db_base_fetch(const opal_identifier_t *proc,
                                     const char *key, void **data,
                                     opal_data_type_t type);
OPAL_DECLSPEC int opal_db_base_fetch_pointer(const opal_identifier_t *proc,
                                             const char *key,
                                             void **data, opal_data_type_t type);
OPAL_DECLSPEC int opal_db_base_fetch_multiple(const opal_identifier_t *proc,
                                              opal_scope_t scope,
                                              const char *key,
                                              opal_list_t *kvs);
OPAL_DECLSPEC int opal_db_base_remove_data(const opal_identifier_t *proc,
                                           const char *key);
OPAL_DECLSPEC int opal_db_base_add_log(const char *table,
                                       const opal_value_t *kvs, int nkvs);
OPAL_DECLSPEC void opal_db_base_commit(const opal_identifier_t *proc);

END_C_DECLS

#endif
