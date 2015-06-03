/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"

OBJ_CLASS_INSTANCE(oshmem_group_cache_t, opal_object_t, NULL, NULL);
opal_list_t oshmem_group_cache_list;
unsigned int oshmem_group_cache_size;
oshmem_group_t* find_group_in_cache(int PE_start, int logPE_stride, int PE_size)
{
    int cache_look_up_id[3] = { PE_start, logPE_stride, PE_size };
    opal_list_item_t *item;
    if (opal_list_is_empty(&oshmem_group_cache_list)) {
        return NULL ;
    }

    for (item = opal_list_get_first(&oshmem_group_cache_list);
            item && (item != opal_list_get_end(&oshmem_group_cache_list));
            item = opal_list_get_next(item)) {
        if (!memcmp(((oshmem_group_cache_t *) item)->cache_id,
                    cache_look_up_id,
                    3 * sizeof(int))) {
            return ((oshmem_group_cache_t *) item)->group;
        }
    }
    return NULL ;
}

int cache_group(oshmem_group_t *group,
                int PE_start,
                int logPE_stride,
                int PE_size)
{
    oshmem_group_cache_t *cached_group = NULL;
    cached_group = OBJ_NEW(oshmem_group_cache_t);
#if OPAL_ENABLE_DEBUG
    cached_group->item.opal_list_item_belong_to = NULL;
    cached_group->item.opal_list_item_refcount = 0;
#endif
    cached_group->group = group;
    cached_group->cache_id[0] = PE_start;
    cached_group->cache_id[1] = logPE_stride;
    cached_group->cache_id[2] = PE_size;
    if (opal_list_get_size(&oshmem_group_cache_list)
            < oshmem_group_cache_size) {
        opal_list_append(&oshmem_group_cache_list,
                         (opal_list_item_t *)cached_group);
    } else {
#if ABORT_ON_CACHE_OVERFLOW
        opal_output(0,
                    "error: group cache overflow on rank %i: cache_size = %u: try encreasing oshmem_group_cache_size mca parameter",
                    group->my_pe,
                    oshmem_group_cache_size);
        oshmem_shmem_abort(-1);
#else
        /*This part of code makes FIFO group cache management. Define ABORT_ON_CACHE_OVERFLOW as 0 to enable this.*/
        oshmem_group_cache_t *cached_group_to_remove = (oshmem_group_cache_t *)opal_list_remove_first(&oshmem_group_cache_list);
        oshmem_proc_group_destroy(cached_group_to_remove->group);
        OBJ_RELEASE(cached_group_to_remove);
        opal_list_append(&oshmem_group_cache_list,(opal_list_item_t *)cached_group);
#endif
    }
    return OSHMEM_SUCCESS;
}

int oshmem_group_cache_list_init(void)
{
    int mca_value;
    int cache_size_default = 100;
    OBJ_CONSTRUCT(&oshmem_group_cache_list, opal_list_t);

    mca_value = cache_size_default;
    (void) mca_base_var_register("oshmem",
                                 "proc",
                                 NULL,
                                 "group_cache_size",
                                 "The depth of the oshmem_group cache list used to speed up collective operations",
                                 MCA_BASE_VAR_TYPE_INT,
                                 NULL,
                                 0,
                                 MCA_BASE_VAR_FLAG_SETTABLE,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &mca_value);
    if (mca_value < 0) {
        opal_output(0,
                    "error: oshmem_group_cache_size mca parameter was set to %i while it has to be positive value. Default value %i will be used.",
                    mca_value,
                    cache_size_default);
        mca_value = cache_size_default;
    }
    oshmem_group_cache_size = (unsigned int) mca_value;
    return OSHMEM_SUCCESS;
}

int oshmem_group_cache_list_free(void)
{
    oshmem_group_cache_t *cached_group = NULL;
    opal_list_item_t *item;
    while (NULL != (item = opal_list_remove_first(&oshmem_group_cache_list))) {
        cached_group = (oshmem_group_cache_t *) item;
        oshmem_proc_group_destroy(cached_group->group);
        OBJ_RELEASE(cached_group);
    }
    return OSHMEM_SUCCESS;
}

