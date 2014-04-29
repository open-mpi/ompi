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
 */

#ifndef MCA_DB_BASE_H
#define MCA_DB_BASE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/event/event.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

ORTE_DECLSPEC extern mca_base_framework_t orte_db_base_framework;

/**
 * Select db modules for the specified handle
 */
ORTE_DECLSPEC int orte_db_base_select(void);

typedef struct {
    opal_list_t actives;
    opal_pointer_array_t handles;
    opal_event_base_t *ev_base;
} orte_db_base_t;

typedef struct {
    opal_list_item_t super;
    orte_db_base_component_t *component;
} orte_db_base_active_component_t;
OBJ_CLASS_DECLARATION(orte_db_base_active_component_t);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    int dbhandle;
    orte_db_callback_fn_t cbfunc;
    void *cbdata;
    opal_list_t *properties;
    char *primary_key;
    char *key;
    opal_list_t *kvs;
} orte_db_request_t;
OBJ_CLASS_DECLARATION(orte_db_request_t);

typedef struct {
    opal_object_t super;
    orte_db_base_component_t *component;
    orte_db_base_module_t *module;
} orte_db_handle_t;
OBJ_CLASS_DECLARATION(orte_db_handle_t);

ORTE_DECLSPEC extern orte_db_base_t orte_db_base;

ORTE_DECLSPEC void orte_db_base_open(char *name,
                                     opal_list_t *properties,
                                     orte_db_callback_fn_t cbfunc,
                                     void *cbdata);
ORTE_DECLSPEC void orte_db_base_close(int dbhandle,
                                      orte_db_callback_fn_t cbfunc,
                                      void *cbdata);
ORTE_DECLSPEC void orte_db_base_store(int dbhandle,
                                      const char *primary_key,
                                      opal_list_t *kvs,
                                      orte_db_callback_fn_t cbfunc,
                                      void *cbdata);
ORTE_DECLSPEC void orte_db_base_commit(int dbhandle,
                                       orte_db_callback_fn_t cbfunc,
                                       void *cbdata);
ORTE_DECLSPEC void orte_db_base_fetch(int dbhandle,
                                      const char *primary_key,
                                      const char *key,
                                      opal_list_t *kvs,
                                      orte_db_callback_fn_t cbfunc,
                                      void *cbdata);
ORTE_DECLSPEC void orte_db_base_remove_data(int dbhandle,
                                            const char *primary_key,
                                            const char *key,
                                            orte_db_callback_fn_t cbfunc,
                                            void *cbdata);

END_C_DECLS

#endif
