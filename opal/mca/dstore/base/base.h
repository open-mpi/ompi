/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_DSTORE_BASE_H
#define MCA_DSTORE_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/event/event.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/util/proc.h"

#include "opal/mca/dstore/dstore.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_dstore_base_framework;

/**
 * Select a dstore module
 */
OPAL_DECLSPEC int opal_dstore_base_select(void);

/* DSTORE is an oddball framework in that it:
 *
 * has an active storage component that issues handle-specific
 * modules. This is done to provide separate storage areas that
 * are isolated from each other, and thus don't have to worry
 * about overlapping keys
 *
 * a backfill module used to attempt to retrieve data that has
 * been requested, but that the handle-specific storage module
 * does not contain. This is used in situations where data has
 * not been provided at startup, and we need to retrieve it
 * solely on-demand
 */
typedef struct {
    opal_dstore_base_component_t *storage_component;
    opal_dstore_base_module_t *backfill_module;
    opal_pointer_array_t handles;   // array of open datastore handles
    opal_list_t available_components;
} opal_dstore_base_t;

OPAL_DECLSPEC extern opal_dstore_base_t opal_dstore_base;

typedef struct {
    opal_object_t super;
    char *name;
    opal_dstore_base_module_t *module;
    opal_dstore_base_component_t *storage_component;
} opal_dstore_handle_t;
OBJ_CLASS_DECLARATION(opal_dstore_handle_t);

/**
 * Data for a particular opal process
 * The name association is maintained in the
 * proc_data hash table.
 */
typedef struct {
    /** Structure can be put on lists (including in hash tables) */
    opal_list_item_t super;
    bool loaded;
    /* List of opal_value_t structures containing all data
       received from this process, sorted by key. */
    opal_list_t data;
} opal_dstore_proc_data_t;
OBJ_CLASS_DECLARATION(opal_dstore_proc_data_t);

/**
 * Attribute structure to update tracker object
 * (used in dstore sm component)
 */
typedef struct {
    opal_list_item_t super;
    uint32_t jobid;
    char *connection_info;
} opal_dstore_attr_t;
OBJ_CLASS_DECLARATION(opal_dstore_attr_t);

typedef struct {
    int32_t seg_index;
    uint32_t offset;
    int32_t data_size;
} meta_info;

#define META_OFFSET 65536

OPAL_DECLSPEC int opal_dstore_base_open(const char *name, char* desired_components, opal_list_t *attrs);
OPAL_DECLSPEC int opal_dstore_base_update(int dstorehandle, opal_list_t *attrs);
OPAL_DECLSPEC int opal_dstore_base_close(int dstorehandle);
OPAL_DECLSPEC int opal_dstore_base_store(int dstorehandle,
                                         const opal_process_name_t *id,
                                         opal_value_t *kv);
OPAL_DECLSPEC int opal_dstore_base_fetch(int dstorehandle,
                                         const opal_process_name_t *id,
                                         const char *key,
                                         opal_list_t *kvs);
OPAL_DECLSPEC int opal_dstore_base_remove_data(int dstorehandle,
                                               const opal_process_name_t *id,
                                               const char *key);
OPAL_DECLSPEC int opal_dstore_base_get_handle(int dstorehandle, void **dhdl);

/* support */
OPAL_DECLSPEC opal_dstore_proc_data_t* opal_dstore_base_lookup_proc(opal_proc_table_t *jtable,
                                                                    opal_process_name_t id, bool create);

OPAL_DECLSPEC opal_value_t* opal_dstore_base_lookup_keyval(opal_dstore_proc_data_t *proc_data,
                                                           const char *key);


END_C_DECLS

#endif
