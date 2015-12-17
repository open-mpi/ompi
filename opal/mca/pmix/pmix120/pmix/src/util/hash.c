/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/pmix_stdint.h>
#include <private/hash_string.h>

#include <string.h>

#include "src/include/pmix_globals.h"
#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_pointer_array.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include "src/util/hash.h"

/**
 * Data for a particular pmix process
 * The name association is maintained in the
 * proc_data hash table.
 */
typedef struct {
    /** Structure can be put on lists (including in hash tables) */
    pmix_list_item_t super;
    /* List of pmix_kval_t structures containing all data
       received from this process */
    pmix_list_t data;
} pmix_proc_data_t;
static void pdcon(pmix_proc_data_t *p)
{
    PMIX_CONSTRUCT(&p->data, pmix_list_t);
}
static void pddes(pmix_proc_data_t *p)
{
    PMIX_LIST_DESTRUCT(&p->data);
}
static PMIX_CLASS_INSTANCE(pmix_proc_data_t,
                           pmix_list_item_t,
                           pdcon, pddes);

static pmix_kval_t* lookup_keyval(pmix_list_t *data,
                                  const char *key);
static pmix_proc_data_t* lookup_proc(pmix_hash_table_t *jtable,
                                     uint64_t id, bool create);

int pmix_hash_store(pmix_hash_table_t *table,
                    int rank, pmix_kval_t *kin)
{
    pmix_proc_data_t *proc_data;
    uint64_t id;
    pmix_kval_t *kv;

    pmix_output_verbose(10, pmix_globals.debug_output,
                        "HASH:STORE rank %d key %s",
                        rank, kin->key);

    if (PMIX_RANK_WILDCARD == rank) {
        id = UINT64_MAX;
    } else {
        id = (uint64_t)rank;
    }

    /* lookup the proc data object for this proc - create
     * it if we don't */
    if (NULL == (proc_data = lookup_proc(table, id, true))) {
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

     /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    kv = lookup_keyval(&proc_data->data, kin->key);
    if (NULL != kv) {
        pmix_list_remove_item(&proc_data->data, &kv->super);
        PMIX_RELEASE(kv);
    }
    /* store the new value */
    PMIX_RETAIN(kin);
    pmix_list_append(&proc_data->data, &kin->super);

    return PMIX_SUCCESS;
}

pmix_status_t pmix_hash_fetch(pmix_hash_table_t *table, int rank,
                              const char *key, pmix_value_t **kvs)
{
    pmix_proc_data_t *proc_data;
    pmix_kval_t *hv;
    uint64_t id;
    pmix_status_t rc;

    pmix_output_verbose(10, pmix_globals.debug_output,
                        "HASH:FETCH rank %d key %s",
                        rank, (NULL == key) ? "NULL" : key);

    /* NULL keys are not supported */
    if (NULL == key) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (PMIX_RANK_WILDCARD == rank) {
        id = UINT64_MAX;
    } else {
        id = (uint64_t)rank;
    }

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_proc(table, id, false))) {
        pmix_output_verbose(10, pmix_globals.debug_output,
                            "HASH:FETCH proc data for rank %d not found",
                            rank);
        return PMIX_ERR_PROC_ENTRY_NOT_FOUND;
    }

    /* find the value from within this proc_data object */
    if (NULL == (hv = lookup_keyval(&proc_data->data, key))) {
        pmix_output_verbose(10, pmix_globals.debug_output,
                            "HASH:FETCH data for key %s not found", key);
        return PMIX_ERR_NOT_FOUND;
    }

    /* create the copy */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)kvs, hv->value, PMIX_VALUE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}

int pmix_hash_remove_data(pmix_hash_table_t *table,
                          int rank, const char *key)
{
    pmix_proc_data_t *proc_data;
    pmix_kval_t *kv;
    uint64_t id;
    char *node;

    /* if the rank is wildcard, we want to apply this to
     * all rank entries */
    if (PMIX_RANK_WILDCARD == rank) {
        id = UINT64_MAX;
        if (PMIX_SUCCESS == pmix_hash_table_get_first_key_uint64(table, &id,
                                                                 (void**)&proc_data,
                                                                 (void**)&node)) {
            if (NULL != proc_data) {
                if (NULL == key) {
                    PMIX_RELEASE(proc_data);
                } else {
                    PMIX_LIST_FOREACH(kv, &proc_data->data, pmix_kval_t) {
                        if (0 == strcmp(key, kv->key)) {
                            pmix_list_remove_item(&proc_data->data, &kv->super);
                            PMIX_RELEASE(kv);
                            break;
                        }
                    }
                }
            }
            while (PMIX_SUCCESS == pmix_hash_table_get_next_key_uint64(table, &id,
                                                                       (void**)&proc_data,
                                                                       node, (void**)&node)) {
                if (NULL != proc_data) {
                    if (NULL == key) {
                        PMIX_RELEASE(proc_data);
                    } else {
                        PMIX_LIST_FOREACH(kv, &proc_data->data, pmix_kval_t) {
                            if (0 == strcmp(key, kv->key)) {
                                pmix_list_remove_item(&proc_data->data, &kv->super);
                                PMIX_RELEASE(kv);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    /* lookup the specified proc */
    id = (uint64_t)rank;
    if (NULL == (proc_data = lookup_proc(table, id, false))) {
        /* no data for this proc */
        return PMIX_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (pmix_kval_t*)pmix_list_remove_first(&proc_data->data))) {
            PMIX_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        pmix_hash_table_remove_value_uint64(table, id);
        /* cleanup */
        PMIX_RELEASE(proc_data);
        return PMIX_SUCCESS;
    }

    /* remove this item */
    PMIX_LIST_FOREACH(kv, &proc_data->data, pmix_kval_t) {
        if (0 == strcmp(key, kv->key)) {
            pmix_list_remove_item(&proc_data->data, &kv->super);
            PMIX_RELEASE(kv);
            break;
        }
    }

    return PMIX_SUCCESS;
}

/**
 * Find data for a given key in a given pmix_list_t.
 */
static pmix_kval_t* lookup_keyval(pmix_list_t *data,
                                  const char *key)
{
    pmix_kval_t *kv;

    PMIX_LIST_FOREACH(kv, data, pmix_kval_t) {
        if (0 == strcmp(key, kv->key)) {
            return kv;
        }
    }
    return NULL;
}


/**
 * Find proc_data_t container associated with given
 * pmix_identifier_t.
 */
static pmix_proc_data_t* lookup_proc(pmix_hash_table_t *jtable,
                                     uint64_t id, bool create)
{
    pmix_proc_data_t *proc_data = NULL;

    pmix_hash_table_get_value_uint64(jtable, id, (void**)&proc_data);
    if (NULL == proc_data && create) {
        /* The proc clearly exists, so create a data structure for it */
        proc_data = PMIX_NEW(pmix_proc_data_t);
        if (NULL == proc_data) {
            pmix_output(0, "pmix:client:hash:lookup_pmix_proc: unable to allocate proc_data_t\n");
            return NULL;
        }
        pmix_hash_table_set_value_uint64(jtable, id, proc_data);
    }

    return proc_data;
}
