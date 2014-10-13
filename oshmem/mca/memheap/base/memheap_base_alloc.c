/*
 * Copyright (c) 2013-2014 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include "oshmem/util/oshmem_util.h"
#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"


int mca_memheap_base_alloc_init(mca_memheap_map_t *map, size_t size)
{
    int ret = OSHMEM_SUCCESS;
    char * seg_filename = NULL;

    assert(map);
    assert(HEAP_SEG_INDEX == map->n_segments);

    map_segment_t *s = &map->mem_segs[map->n_segments];
    seg_filename = oshmem_get_unique_file_name(oshmem_my_proc_id());
    ret = mca_sshmem_segment_create(s, seg_filename, size);

    if (OSHMEM_SUCCESS == ret) {
        map->n_segments++;
        MEMHEAP_VERBOSE(1,
                        "Memheap alloc memory: %llu byte(s), %d segments by method: %d",
                        (unsigned long long)size, map->n_segments, s->type);
    }

    free(seg_filename);

    return ret;
}

void mca_memheap_base_alloc_exit(mca_memheap_map_t *map)
{
    if (map) {
        map_segment_t *s = &map->mem_segs[HEAP_SEG_INDEX];

        assert(s);

        mca_sshmem_segment_detach(s, NULL);
        mca_sshmem_unlink(s);
    }
}
