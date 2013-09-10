/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef _PROC_GROUP_CACHE_H
#define _PROC_GROUP_CACHE_H

#include "oshmem_config.h"
#include "proc.h"

#define OSHMEM_GROUP_CACHE_ENABLED  1
#define ABORT_ON_CACHE_OVERFLOW 1
BEGIN_C_DECLS
struct oshmem_group_cache_t {
    opal_list_item_t item;
    oshmem_group_t *group;
    int cache_id[3];
};

typedef struct oshmem_group_cache_t oshmem_group_cache_t;
OSHMEM_DECLSPEC OBJ_CLASS_DECLARATION(oshmem_group_cache_t);
OSHMEM_DECLSPEC extern opal_list_t oshmem_group_cache_list;

oshmem_group_t* find_group_in_cache(int PE_start, int logPE_stride, int PE_size);

int cache_group(oshmem_group_t *group,
                int PE_start,
                int logPE_stride,
                int PE_size);
int oshmem_group_cache_list_init(void);
int oshmem_group_cache_list_free(void);

extern unsigned int oshmem_group_cache_size;
END_C_DECLS

#endif
