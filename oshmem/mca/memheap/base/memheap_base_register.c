/*
* Copyright (c) 2012      Mellanox Technologies, Inc.
*                         All rights reserved.
* $COPYRIGHT$
*
* Additional copyrights may follow
*
* $HEADER$
*/
#include "oshmem_config.h"

#include "oshmem/proc/proc.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"

#include <stdio.h>


static int __dereg_segment(map_segment_t *s);
static int __reg_segment(map_segment_t *s, int *num_btl);


extern int mca_memheap_base_register(mca_memheap_map_t *memheap_map)
{
    int ret = OSHMEM_SUCCESS;
    int i;

    for (i = 0; i < memheap_map->n_segments; i++) {
        map_segment_t *s = &memheap_map->mem_segs[i];

        MEMHEAP_VERBOSE(5, "register seg#%02d: 0x%llX - 0x%llX %llu bytes type=0x%X id=0x%X",
                i,
                (long long)s->start,
                (long long)s->end,
                (long long)(s->end - s->start),
                s->type,
                s->shmid
                );
        ret = __reg_segment(s, &memheap_map->num_transports);
    }

    return ret;
}


extern int mca_memheap_base_deregister(mca_memheap_map_t *memheap_map)
{
    int ret = OSHMEM_SUCCESS;
    int i;

    for (i = 0; i < memheap_map->n_segments; i++) {
        map_segment_t *s = &memheap_map->mem_segs[i];

        if (!s->is_active)
            continue;

        MEMHEAP_VERBOSE(5, "deregistering segment#%d: %llx - %llx %llu bytes",
                i,
                (long long)s->start,
                (long long)s->end,
                (long long)(s->end - s->start)
                );
        ret  = __dereg_segment(s);
    }

    return ret;
}


static int __dereg_segment(map_segment_t *s)
{
    int rc = OSHMEM_SUCCESS;
    int j;
    int nprocs, my_pe;

    nprocs = oshmem_num_procs();
    my_pe  = oshmem_my_proc_id();

    MCA_SPML_CALL(deregister(s->mkeys));

    if (s->mkeys_cache) {
        for (j = 0; j < nprocs; j++) {
            if (j == my_pe)
                continue;
            if (s->mkeys_cache[j])
            {
                free(s->mkeys_cache[j]);
                s->mkeys_cache[j] = NULL;
            }
        }
        free(s->mkeys_cache);
        s->mkeys_cache = NULL;
    }

    s->is_active = 0;

    return rc;
}


static int __reg_segment(map_segment_t *s, int *num_btl)
{
    int rc = OSHMEM_SUCCESS;
    int my_pe;
    int nprocs;

    nprocs = oshmem_num_procs();
    my_pe  = oshmem_my_proc_id();

    s->mkeys_cache = (mca_spml_mkey_t **)calloc(nprocs, sizeof(mca_spml_mkey_t *));
    if (NULL == s->mkeys_cache) 
    {
        MEMHEAP_ERROR("Failed to allocate memory for remote segments");
        rc = OSHMEM_ERROR;
    }

    if (!rc)
    {
        s->mkeys = MCA_SPML_CALL(register((void *)(unsigned long)s->start, 
                                           s->end - s->start, 
                                           MEMHEAP_SHM_CODE(s->type, s->shmid), 
                                           num_btl));
        if (NULL == s->mkeys) 
        {
            free(s->mkeys_cache);
            s->mkeys_cache = NULL;

            MEMHEAP_ERROR("Failed to register segment");
            rc = OSHMEM_ERROR;
        }
    }

    if (OSHMEM_SUCCESS == rc)
    {
        s->mkeys_cache[my_pe] = s->mkeys;
        s->is_active = 1;
    }

    return rc;
}
