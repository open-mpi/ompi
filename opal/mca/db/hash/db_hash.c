/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
  * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <time.h>
#include <string.h>

#include "opal_stdint.h"
#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/db/base/base.h"
#include "db_hash.h"

static int init(void);
static void finalize(void);
static int store(const opal_identifier_t *proc,
                 opal_scope_t scope,
                 const char *key, const void *object,
                 opal_data_type_t type);
static int store_pointer(const opal_identifier_t *proc,
                         opal_value_t *kv);
static int fetch(const opal_identifier_t *proc,
                 const char *key, void **data,
                 opal_data_type_t type);
static int fetch_pointer(const opal_identifier_t *proc,
                         const char *key,
                         void **data, opal_data_type_t type);
static int fetch_multiple(const opal_identifier_t *proc,
                          opal_scope_t scope,
                          const char *key,
                          opal_list_t *kvs);
static int remove_data(const opal_identifier_t *proc, const char *key);

opal_db_base_module_t opal_db_hash_module = {
    init,
    finalize,
    opal_db_base_set_id,
    store,
    store_pointer,
    NULL,
    fetch,
    fetch_pointer,
    fetch_multiple,
    remove_data,
    NULL
};

/* Local "globals" */
static opal_hash_table_t hash_data;

/**
 * Data for a particular opal process
 * The name association is maintained in the
 * proc_data hash table.
 */
typedef struct {
    /** Structure can be put on lists (including in hash tables) */
    opal_list_item_t super;
    /* List of opal_value_t structures containing all data
       received from this process, sorted by key. */
    opal_list_t data;
} proc_data_t;

static void proc_data_construct(proc_data_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->data, opal_list_t);
}

static void proc_data_destruct(proc_data_t *ptr)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&ptr->data))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->data);
}
OBJ_CLASS_INSTANCE(proc_data_t, opal_list_item_t,
                   proc_data_construct, proc_data_destruct);

 
static int init(void)
{
    OBJ_CONSTRUCT(&hash_data, opal_hash_table_t);
    opal_hash_table_init(&hash_data, 256);
    return OPAL_SUCCESS;
}

static void finalize(void)
{
    proc_data_t *proc_data;
    uint64_t key;
    char *node;

    /* to assist in getting a clean valgrind, cycle thru the hash table
     * and release all data stored in it
     */
    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(&hash_data, &key,
                                                             (void**)&proc_data,
                                                             (void**)&node)) {
        if (NULL != proc_data) {
            OBJ_RELEASE(proc_data);
        }
        while (OPAL_SUCCESS == opal_hash_table_get_next_key_uint64(&hash_data, &key,
                                                                   (void**)&proc_data,
                                                                   node, (void**)&node)) {
            if (NULL != proc_data) {
                OBJ_RELEASE(proc_data);
            }
        }
    }
    OBJ_DESTRUCT(&hash_data);
}



/**
 * Find data for a given key in a given proc_data_t
 * container.
 */
static opal_value_t* lookup_keyval(proc_data_t *proc_data,
                                   const char *key)
{
    opal_value_t *kv = NULL;
    for (kv = (opal_value_t *) opal_list_get_first(&proc_data->data);
         kv != (opal_value_t *) opal_list_get_end(&proc_data->data);
         kv = (opal_value_t *) opal_list_get_next(kv)) {
        if (0 == strcmp(key, kv->key)) {
            return kv;
        }
    }

    return NULL;
}


/**
 * Find proc_data_t container associated with given
 * opal_identifier_t.
 */
static proc_data_t* lookup_opal_proc(opal_hash_table_t *jtable, opal_identifier_t id)
{
    proc_data_t *proc_data = NULL;
    
    opal_hash_table_get_value_uint64(jtable, id, (void**)&proc_data);
    if (NULL == proc_data) {
        /* The proc clearly exists, so create a data structure for it */
        proc_data = OBJ_NEW(proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "db:hash:lookup_opal_proc: unable to allocate proc_data_t\n");
            return NULL;
        }
        opal_hash_table_set_value_uint64(jtable, id, proc_data);
    }
    
    return proc_data;
}

static int store(const opal_identifier_t *uid,
                 opal_scope_t scope,
                 const char *key, const void *data,
                 opal_data_type_t type)
{
    proc_data_t *proc_data;
    opal_value_t *kv;
    opal_byte_object_t *boptr;
    opal_identifier_t id;

    /* data must have an assigned scope */
    if (OPAL_SCOPE_UNDEF == scope) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* we are at the bottom of the store priorities, so
     * if this fell to us, we store it
     */
    opal_output_verbose(1, opal_db_base_framework.framework_output,
                        "db:hash:store storing data for proc %" PRIu64 " for scope %d",
                        id, (int)scope);

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* unrecoverable error */
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db:hash:store: storing key %s[%s] for proc %" PRIu64 " unrecoverably failed",
                             key, opal_dss.lookup_data_type(type), id));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    kv = lookup_keyval(proc_data, key);
#if OPAL_ENABLE_DEBUG
    char *_data_type = opal_dss.lookup_data_type(type);
    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:hash:store: %s key %s[%s] for proc %" PRIu64 "",
                         (NULL == kv ? "storing" : "updating"),
                         key, _data_type, id));
    free (_data_type);
#endif
    if (NULL != kv) {
        opal_list_remove_item(&proc_data->data, &kv->super);
        OBJ_RELEASE(kv);
    }
    kv = OBJ_NEW(opal_value_t);
    kv->key = strdup(key);
    kv->scope = scope;
    opal_list_append(&proc_data->data, &kv->super);

    /* the type could come in as an OPAL one (e.g., OPAL_VPID). Since
     * the value is an OPAL definition, it cannot cover OPAL data
     * types, so convert to the underlying OPAL type
     */
    switch (type) {
    case OPAL_STRING:
        kv->type = OPAL_STRING;
        if (NULL != data) {
            kv->data.string = strdup( (const char *) data);
        } else {
            kv->data.string = NULL;
        }
        break;
    case OPAL_UINT64:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT64;
        /* to avoid alignment issues */
        memcpy(&kv->data.uint64, data, 8);
        break;
    case OPAL_UINT32:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT32;
        /* to avoid alignment issues */
        memcpy(&kv->data.uint32, data, 4);
        break;
    case OPAL_UINT16:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT16;
        /* to avoid alignment issues */
        memcpy(&kv->data.uint16, data, 2);
        break;
    case OPAL_INT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_INT;
        /* to avoid alignment issues */
        memcpy(&kv->data.integer, data, sizeof(int));
        break;
    case OPAL_UINT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_UINT;
        /* to avoid alignment issues */
        memcpy(&kv->data.uint, data, sizeof(unsigned int));
        break;
    case OPAL_FLOAT:
        if (NULL == data) {
            OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
            return OPAL_ERR_BAD_PARAM;
        }
        kv->type = OPAL_FLOAT;
        memcpy(&kv->data.fval, data, sizeof(float));
        break;
    case OPAL_BYTE_OBJECT:
        kv->type = OPAL_BYTE_OBJECT;
        boptr = (opal_byte_object_t*)data;
        if (NULL != boptr && NULL != boptr->bytes && 0 < boptr->size) {
            kv->data.bo.bytes = (uint8_t *) malloc(boptr->size);
            memcpy(kv->data.bo.bytes, boptr->bytes, boptr->size);
            kv->data.bo.size = boptr->size;
        } else {
            kv->data.bo.bytes = NULL;
            kv->data.bo.size = 0;
        }
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return OPAL_SUCCESS;
}

static int store_pointer(const opal_identifier_t *uid,
                         opal_value_t *kv)
{
    proc_data_t *proc_data;
    opal_value_t *k2;
    opal_identifier_t id;

    /* data must have an assigned scope */
    if (OPAL_SCOPE_UNDEF == kv->scope) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* we are at the bottom of the store priorities, so
     * if this fell to us, we store it
     */
    opal_output_verbose(1, opal_db_base_framework.framework_output,
                        "db:hash:store storing data for proc %" PRIu64 " for scope %d",
                        id, (int)kv->scope);

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* unrecoverable error */
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db:hash:store: storing key %s[%s] for proc %" PRIu64 " unrecoverably failed",
                             kv->key, opal_dss.lookup_data_type(kv->type), id));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    k2 = lookup_keyval(proc_data, kv->key);
    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:hash:store: %s pointer of key %s[%s] for proc %" PRIu64 "",
                         (NULL == k2 ? "storing" : "updating"),
                         kv->key, opal_dss.lookup_data_type(kv->type), id));
    if (NULL != k2) {
        opal_list_remove_item(&proc_data->data, &k2->super);
        OBJ_RELEASE(k2);
    }
    kv->scope |= OPAL_SCOPE_REFER;  // mark that this value was stored by reference and doesn't belong to us
    opal_list_append(&proc_data->data, &kv->super);
    return OPAL_SUCCESS;
}

static int fetch(const opal_identifier_t *uid,
                 const char *key, void **data,
                 opal_data_type_t type)
{
    proc_data_t *proc_data;
    opal_value_t *kv;
    opal_byte_object_t *boptr;
    opal_identifier_t id;

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:hash:fetch: searching for key %s[%s] on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key,
                         opal_dss.lookup_data_type(type), id));

    /* if the key is NULL, that is an error */
    if (NULL == key) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* maybe they can find it elsewhere */
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db_hash:fetch data for proc %" PRIu64 " not found", id));
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    /* find the value */
    if (NULL == (kv = lookup_keyval(proc_data, key))) {
        /* let them look globally for it */
        OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                             "db_hash:fetch key %s for proc %" PRIu64 " not found",
                             (NULL == key) ? "NULL" : key, id));
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    /* do the copy and check the type */
    switch (type) {
    case OPAL_STRING:
        if (OPAL_STRING != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        if (NULL != kv->data.string) {
            *data = strdup(kv->data.string);
        } else {
            *data = NULL;
        }
        break;
    case OPAL_UINT64:
        if (OPAL_UINT64 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint64, 8);
        break;
    case OPAL_UINT32:
        if (OPAL_UINT32 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint32, 4);
        break;
    case OPAL_UINT16:
        if (OPAL_UINT16 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint16, 2);
        break;
    case OPAL_INT:
        if (OPAL_INT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.integer, sizeof(int));
        break;
    case OPAL_UINT:
        if (OPAL_UINT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.uint, sizeof(unsigned int));
        break;
    case OPAL_FLOAT:
        if (OPAL_FLOAT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        memcpy(*data, &kv->data.fval, sizeof(float));
        break;
    case OPAL_BYTE_OBJECT:
        if (OPAL_BYTE_OBJECT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        boptr = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        if (NULL != kv->data.bo.bytes && 0 < kv->data.bo.size) {
            boptr->bytes = (uint8_t *) malloc(kv->data.bo.size);
            memcpy(boptr->bytes, kv->data.bo.bytes, kv->data.bo.size);
            boptr->size = kv->data.bo.size;
        } else {
            boptr->bytes = NULL;
            boptr->size = 0;
        }
        *data = boptr;
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return OPAL_SUCCESS;
}

static int fetch_pointer(const opal_identifier_t *uid,
                         const char *key,
                         void **data, opal_data_type_t type)
{
    proc_data_t *proc_data;
    opal_value_t *kv;
    opal_identifier_t id;

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:hash:fetch_pointer: searching for key %s on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key, id));

    /* if the key is NULL, that is an error */
    if (NULL == key) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* look elsewhere */
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    /* find the value */
    if (NULL == (kv = lookup_keyval(proc_data, key))) {
        /* let them look globally for it */
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

   switch (type) {
    case OPAL_STRING:
        if (OPAL_STRING != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = kv->data.string;
        break;
    case OPAL_UINT64:
        if (OPAL_UINT64 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.uint64;
        break;
    case OPAL_UINT32:
        if (OPAL_UINT32 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.uint32;
        break;
    case OPAL_UINT16:
        if (OPAL_UINT16 != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.uint16;
        break;
    case OPAL_INT:
        if (OPAL_INT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.integer;
        break;
    case OPAL_UINT:
        if (OPAL_UINT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.uint;
        break;
    case OPAL_BYTE_OBJECT:
        if (OPAL_BYTE_OBJECT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.bo;
        break;
    case OPAL_FLOAT:
        if (OPAL_FLOAT != kv->type) {
            return OPAL_ERR_TYPE_MISMATCH;
        }
        *data = &kv->data.fval;
        break;
    default:
        OPAL_ERROR_LOG(OPAL_ERR_NOT_SUPPORTED);
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return OPAL_SUCCESS;
}

static int fetch_multiple(const opal_identifier_t *uid,
                          opal_scope_t scope,
                          const char *key,
                          opal_list_t *kvs)
{
    proc_data_t *proc_data;
    opal_value_t *kv, *kvnew;
    int rc;
    char *srchkey, *ptr;
    size_t len = 0;
    opal_identifier_t id;

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    OPAL_OUTPUT_VERBOSE((5, opal_db_base_framework.framework_output,
                         "db:hash:fetch_multiple: searching for key %s on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key, id));

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* look elsewhere */
        return OPAL_ERR_TAKE_NEXT_OPTION;
    }

    /* if the key is NULL, then return all the values */
    if (NULL == key) {
        for (kv = (opal_value_t*) opal_list_get_first(&proc_data->data);
             kv != (opal_value_t*) opal_list_get_end(&proc_data->data);
             kv = (opal_value_t*) opal_list_get_next(kv)) {
            /* check for a matching scope */
            if (!(scope & kv->scope)) {
                continue;
            }
            if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&kvnew, kv, OPAL_VALUE))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            opal_list_append(kvs, &kvnew->super);
        }
        return OPAL_SUCCESS;
    }

    /* see if the key includes a wildcard */
    srchkey = strdup(key);
    if (NULL != (ptr = strchr(srchkey, '*'))) {
        *ptr = '\0';
        len = strlen(srchkey);
    }

    /* otherwise, find all matching keys and return them */
    for (kv = (opal_value_t*) opal_list_get_first(&proc_data->data);
         kv != (opal_value_t*) opal_list_get_end(&proc_data->data);
         kv = (opal_value_t*) opal_list_get_next(kv)) {
        /* check for a matching scope */
        if (!(scope & kv->scope)) {
            continue;
        }
        if ((0 < len && 0 == strncmp(srchkey, kv->key, len)) ||
            (0 == len && 0 == strcmp(key, kv->key))) {
            if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&kvnew, kv, OPAL_VALUE))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            opal_list_append(kvs, &kvnew->super);
        }
    }
    free(srchkey);
    return OPAL_SUCCESS;
}

static int remove_data(const opal_identifier_t *uid, const char *key)
{
    proc_data_t *proc_data;
    opal_value_t *kv;
    opal_identifier_t id;

    /* to protect alignment, copy the data across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* lookup the specified proc */
    if (NULL == (proc_data = lookup_opal_proc(&hash_data, id))) {
        /* no data for this proc */
        return OPAL_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (opal_value_t *) opal_list_remove_first(&proc_data->data))) {
            OBJ_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        opal_hash_table_remove_value_uint64(&hash_data, id);
        /* cleanup */
        OBJ_RELEASE(proc_data);
        return OPAL_SUCCESS;
    }

    /* remove this item */
    for (kv = (opal_value_t*) opal_list_get_first(&proc_data->data);
         kv != (opal_value_t*) opal_list_get_end(&proc_data->data);
         kv = (opal_value_t*) opal_list_get_next(kv)) {
        if (0 == strcmp(key, kv->key)) {
            opal_list_remove_item(&proc_data->data, &kv->super);
            if (!(kv->scope & OPAL_SCOPE_REFER)) {
                OBJ_RELEASE(kv);
            }
            break;
        }
    }

    return OPAL_SUCCESS;
}

