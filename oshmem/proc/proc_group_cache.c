/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"

#define OSHMEM_GROUP_CACHE_SIZE 1024

static opal_hash_table_t group_cache;

typedef struct {
    int pe_start;
    int pe_size;
    int pe_stride;
} oshmem_group_key_t;

static int group_cache_n_hits;
static int group_cache_n_lookups;

int oshmem_group_cache_init(void)
{
    OBJ_CONSTRUCT(&group_cache, opal_hash_table_t);
    if (OPAL_SUCCESS != opal_hash_table_init(&group_cache, OSHMEM_GROUP_CACHE_SIZE)) {
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

void oshmem_group_cache_destroy(void)
{
    OBJ_DESTRUCT(&group_cache);
}

oshmem_group_t *oshmem_group_cache_find(int pe_start, int pe_stride, int pe_size)
{
    oshmem_group_key_t key;
    oshmem_group_t *group;

    if (!oshmem_group_cache_enabled()) {
        return NULL;
    }

    key.pe_start  = pe_start;
    key.pe_size   = pe_size;
    key.pe_stride = pe_stride;

    group_cache_n_lookups++;

    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(&group_cache, &key,
                                                      sizeof(key), (void **)&group)) {
        return NULL;
    }

    group_cache_n_hits++;
    return group;
}

int oshmem_group_cache_insert(oshmem_group_t *group, int pe_start,
                              int pe_stride, int pe_size)
{
    oshmem_group_key_t key;

    if (!oshmem_group_cache_enabled()) {
        return OSHMEM_SUCCESS;
    }

    key.pe_start  = pe_start;
    key.pe_size   = pe_size;
    key.pe_stride = pe_stride;

    if (OPAL_SUCCESS != opal_hash_table_set_value_ptr(&group_cache, &key,
                                                      sizeof(key), group)) {
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}

