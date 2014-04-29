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

#include "opal/mca/dstore/base/base.h"
#include "dstore_hash.h"

static int init(struct opal_dstore_base_module_t *imod);
static void finalize(struct opal_dstore_base_module_t *imod);
static int store(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *proc,
                 opal_value_t *val);
static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *proc,
                 const char *key,
                 opal_list_t *kvs);
static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_identifier_t *proc, const char *key);

mca_dstore_hash_module_t opal_dstore_hash_module = {
    {
        init,
        finalize,
        store,
        NULL,
        fetch,
        remove_data
    }
};

/* Initialize our hash table */
static int init(struct opal_dstore_base_module_t *imod)
{
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;
    OBJ_CONSTRUCT(&mod->hash_data, opal_hash_table_t);
    opal_hash_table_init(&mod->hash_data, 256);
    return OPAL_SUCCESS;
}

static void finalize(struct opal_dstore_base_module_t *imod)
{
    opal_dstore_proc_data_t *proc_data;
    uint64_t key;
    char *node;
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;

    /* to assist in getting a clean valgrind, cycle thru the hash table
     * and release all data stored in it
     */
    if (OPAL_SUCCESS == opal_hash_table_get_first_key_uint64(&mod->hash_data, &key,
                                                             (void**)&proc_data,
                                                             (void**)&node)) {
        if (NULL != proc_data) {
            OBJ_RELEASE(proc_data);
        }
        while (OPAL_SUCCESS == opal_hash_table_get_next_key_uint64(&mod->hash_data, &key,
                                                                   (void**)&proc_data,
                                                                   node, (void**)&node)) {
            if (NULL != proc_data) {
                OBJ_RELEASE(proc_data);
            }
        }
    }
    OBJ_DESTRUCT(&mod->hash_data);
}



static int store(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *uid,
                 opal_value_t *val)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv;
    opal_identifier_t id;
    mca_dstore_hash_module_t *mod;
    int rc;

    mod = (mca_dstore_hash_module_t*)imod;

    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "dstore:hash:store storing data for proc %" PRIu64 "", id);

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        /* unrecoverable error */
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "dstore:hash:store: storing data for proc %" PRIu64 " unrecoverably failed",
                             id));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    kv = opal_dstore_base_lookup_keyval(proc_data, val->key);
    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "dstore:hash:store: %s key %s[%s] for proc %" PRIu64 "",
                         (NULL == kv ? "storing" : "updating"),
                         val->key, opal_dss.lookup_data_type(val->type), id));
    
    if (NULL != kv) {
        opal_list_remove_item(&proc_data->data, &kv->super);
        OBJ_RELEASE(kv);
    }
    /* create the copy */
    if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&kv, val, OPAL_VALUE))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    opal_list_append(&proc_data->data, &kv->super);

    return OPAL_SUCCESS;
}

static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_identifier_t *uid,
                 const char *key, opal_list_t *kvs)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv, *knew;
    opal_identifier_t id;
    mca_dstore_hash_module_t *mod;
    int rc;

    mod = (mca_dstore_hash_module_t*)imod;

    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "dstore:hash:fetch: searching for key %s on proc %" PRIu64 "",
                         (NULL == key) ? "NULL" : key, id));

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "dstore_hash:fetch data for proc %" PRIu64 " not found", id));
        return OPAL_ERR_NOT_FOUND;
    }

    /* if the key is NULL, that we want everything */
    if (NULL == key) {
        OPAL_LIST_FOREACH(kv, &proc_data->data, opal_value_t) {
            /* copy the value */
            if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&knew, kv, OPAL_VALUE))) {
                OPAL_ERROR_LOG(rc);
                return rc;
            }
            /* add it to the output list */
            opal_list_append(kvs, &knew->super);
        }
        return OPAL_SUCCESS;
    }

    /* find the value */
    if (NULL == (kv = opal_dstore_base_lookup_keyval(proc_data, key))) {
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "dstore_hash:fetch key %s for proc %" PRIu64 " not found",
                             (NULL == key) ? "NULL" : key, id));
        return OPAL_ERR_NOT_FOUND;
    }

    /* create the copy */
    if (OPAL_SUCCESS != (rc = opal_dss.copy((void**)&knew, kv, OPAL_VALUE))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    /* add it to the output list */
    opal_list_append(kvs, &knew->super);

    return OPAL_SUCCESS;
}

static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_identifier_t *uid, const char *key)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv;
    opal_identifier_t id;
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;

    /* to protect alignment, copy the identifier across */
    memcpy(&id, uid, sizeof(opal_identifier_t));

    /* lookup the specified proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->hash_data, id))) {
        /* no data for this proc */
        return OPAL_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (opal_value_t *) opal_list_remove_first(&proc_data->data))) {
            OBJ_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        opal_hash_table_remove_value_uint64(&mod->hash_data, id);
        /* cleanup */
        OBJ_RELEASE(proc_data);
        return OPAL_SUCCESS;
    }

    /* remove this item */
    OPAL_LIST_FOREACH(kv, &proc_data->data, opal_value_t) {
        if (0 == strcmp(key, kv->key)) {
            opal_list_remove_item(&proc_data->data, &kv->super);
            OBJ_RELEASE(kv);
            break;
        }
    }

    return OPAL_SUCCESS;
}

