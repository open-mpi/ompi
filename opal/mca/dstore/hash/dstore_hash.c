/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "opal/util/proc.h"
#include "opal/util/show_help.h"

#include "opal/mca/dstore/base/base.h"
#include "dstore_hash.h"

static int init(struct opal_dstore_base_module_t *imod);
static void finalize(struct opal_dstore_base_module_t *imod);
static int store(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *proc,
                 opal_value_t *val);
static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *proc,
                 const char *key,
                 opal_list_t *kvs);
static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_process_name_t *proc, const char *key);

mca_dstore_hash_module_t opal_dstore_hash_module = {
    {
        init,
        finalize,
        store,
        fetch,
        remove_data
    }
};

/* Initialize our hash table */
static int init(struct opal_dstore_base_module_t *imod)
{
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;
    OBJ_CONSTRUCT(&mod->ptable, opal_proc_table_t);
    opal_proc_table_init(&mod->ptable, 16, 256);
    return OPAL_SUCCESS;
}

static void finalize(struct opal_dstore_base_module_t *imod)
{
    opal_dstore_proc_data_t *proc_data;
    opal_process_name_t key;
    void *node1, *node2;
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;

    /* to assist in getting a clean valgrind, cycle thru the hash table
     * and release all data stored in it
     */
    if (OPAL_SUCCESS == opal_proc_table_get_first_key(&mod->ptable, &key,
                                                      (void**)&proc_data,
                                                      &node1, &node2)) {
        if (NULL != proc_data) {
            OBJ_RELEASE(proc_data);
        }
        while (OPAL_SUCCESS == opal_proc_table_get_next_key(&mod->ptable, &key,
                                                            (void**)&proc_data,
                                                            node1, &node1,
                                                            node2, &node2)) {
            if (NULL != proc_data) {
                OBJ_RELEASE(proc_data);
            }
        }
    }
    OBJ_DESTRUCT(&mod->ptable);
}



static int store(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *id,
                 opal_value_t *val)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv;
    mca_dstore_hash_module_t *mod;
    int rc;

    mod = (mca_dstore_hash_module_t*)imod;

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "%s dstore:hash:store storing data for proc %s",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), OPAL_NAME_PRINT(*id));

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->ptable, *id, true))) {
        /* unrecoverable error */
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "%s dstore:hash:store: storing data for proc %s unrecoverably failed",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), OPAL_NAME_PRINT(*id)));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    kv = opal_dstore_base_lookup_keyval(proc_data, val->key);
#if OPAL_ENABLE_DEBUG
    char *_data_type = opal_dss.lookup_data_type(val->type);
    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "%s dstore:hash:store: %s key %s[%s] for proc %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                         (NULL == kv ? "storing" : "updating"),
                         val->key, _data_type, OPAL_NAME_PRINT(*id)));
    free (_data_type);
#endif

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
                 const opal_process_name_t *id,
                 const char *key, opal_list_t *kvs)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv, *knew;
    mca_dstore_hash_module_t *mod;
    int rc;

    mod = (mca_dstore_hash_module_t*)imod;

    OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                         "%s dstore:hash:fetch: searching for key %s on proc %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                         (NULL == key) ? "NULL" : key, OPAL_NAME_PRINT(*id)));

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->ptable, *id, true))) {
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "%s dstore_hash:fetch data for proc %s not found",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                             OPAL_NAME_PRINT(*id)));
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
            OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                                 "%s dstore:hash:fetch: adding data for key %s on proc %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 (NULL == kv->key) ? "NULL" : kv->key,
                                 OPAL_NAME_PRINT(*id)));

            /* add it to the output list */
            opal_list_append(kvs, &knew->super);
        }
        return OPAL_SUCCESS;
    }

    /* find the value */
    if (NULL == (kv = opal_dstore_base_lookup_keyval(proc_data, key))) {
        OPAL_OUTPUT_VERBOSE((5, opal_dstore_base_framework.framework_output,
                             "%s dstore_hash:fetch key %s for proc %s not found",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                             (NULL == key) ? "NULL" : key,
                             OPAL_NAME_PRINT(*id)));
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
                       const opal_process_name_t *id, const char *key)
{
    opal_dstore_proc_data_t *proc_data;
    opal_value_t *kv;
    mca_dstore_hash_module_t *mod;

    mod = (mca_dstore_hash_module_t*)imod;

    /* lookup the specified proc */
    if (NULL == (proc_data = opal_dstore_base_lookup_proc(&mod->ptable, *id, false))) {
        /* no data for this proc */
        return OPAL_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (opal_value_t *) opal_list_remove_first(&proc_data->data))) {
            OBJ_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        opal_proc_table_remove_value(&mod->ptable, *id);
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

