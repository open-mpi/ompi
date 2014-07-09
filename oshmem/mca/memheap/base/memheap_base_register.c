/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/util/oshmem_util.h"
#include "oshmem/proc/proc.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

#include <stdio.h>

static int _dereg_segment(map_segment_t *s);
static int _reg_segment(map_segment_t *s, int *num_btl);

int mca_memheap_base_reg(mca_memheap_map_t *memheap_map)
{
    int ret;
    int i;

    for (i = 0; i < memheap_map->n_segments; i++) {
        map_segment_t *s = &memheap_map->mem_segs[i];

        MEMHEAP_VERBOSE(5,
                        "register seg#%02d: 0x%p - 0x%p %llu bytes type=0x%X id=0x%X",
                        i,
                        s->seg_base_addr,
                        s->end,
                        (long long)((uintptr_t)s->end - (uintptr_t)s->seg_base_addr),
                        s->type,
                        s->seg_id);
        ret = _reg_segment(s, &memheap_map->num_transports);
        if (OSHMEM_SUCCESS != ret) {
            mca_memheap_base_dereg(memheap_map);
            return ret;
        }
    }

    return OSHMEM_SUCCESS;
}

int mca_memheap_base_dereg(mca_memheap_map_t *memheap_map)
{
    int i;

    for (i = 0; i < memheap_map->n_segments; i++) {
        map_segment_t *s = &memheap_map->mem_segs[i];

        if (!MAP_SEGMENT_IS_VALID(s))
            continue;

        MEMHEAP_VERBOSE(5,
                        "deregistering segment#%d: %p - %p %llu bytes",
                        i,
                        s->seg_base_addr,
                        s->end,
                        (long long)((uintptr_t)s->end - (uintptr_t)s->seg_base_addr));
        (void)_dereg_segment(s);
    }

    return OSHMEM_SUCCESS;
}

static int _dereg_segment(map_segment_t *s)
{
    int rc = OSHMEM_SUCCESS;
    int j;
    int nprocs, my_pe;

    nprocs = oshmem_num_procs();
    my_pe = oshmem_my_proc_id();

    MCA_SPML_CALL(deregister(s->mkeys));

    if (s->mkeys_cache) {
        for (j = 0; j < nprocs; j++) {
            if (j == my_pe)
                continue;
            if (s->mkeys_cache[j]) {
                if (s->mkeys_cache[j]->len) {
                    free(s->mkeys_cache[j]->u.data);
                    s->mkeys_cache[j]->len = 0;
                }
                free(s->mkeys_cache[j]);
                s->mkeys_cache[j] = NULL;
            }
        }
        free(s->mkeys_cache);
        s->mkeys_cache = NULL;
    }

    MAP_SEGMENT_INVALIDATE(s);

    return rc;
}

static int _reg_segment(map_segment_t *s, int *num_btl)
{
    int rc = OSHMEM_SUCCESS;
    int my_pe;
    int nprocs;

    nprocs = oshmem_num_procs();
    my_pe = oshmem_my_proc_id();

    s->mkeys_cache = (sshmem_mkey_t **) calloc(nprocs,
                                                 sizeof(sshmem_mkey_t *));
    if (NULL == s->mkeys_cache) {
        MEMHEAP_ERROR("Failed to allocate memory for remote segments");
        rc = OSHMEM_ERROR;
    }

    if (!rc) {
        s->mkeys = MCA_SPML_CALL(register((void *)(unsigned long)s->seg_base_addr,
                        (uintptr_t)s->end - (uintptr_t)s->seg_base_addr,
                        s->seg_id,
                        num_btl));
        if (NULL == s->mkeys) {
            free(s->mkeys_cache);
            s->mkeys_cache = NULL;

            MEMHEAP_ERROR("Failed to register segment");
            rc = OSHMEM_ERROR;
        }
    }

    if (OSHMEM_SUCCESS == rc) {
        s->mkeys_cache[my_pe] = s->mkeys;
        MAP_SEGMENT_SET_VALID(s);
    }

    return rc;
}
