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


int mca_memheap_base_alloc_init(mca_memheap_map_t *map, size_t size, long hint)
{
    int ret = OSHMEM_SUCCESS;
    char * seg_filename = NULL;

    assert(map);
    if (hint == 0) {
        assert(HEAP_SEG_INDEX == map->n_segments);
    } else {
        assert(HEAP_SEG_INDEX < map->n_segments);
    }

    map_segment_t *s = &map->mem_segs[map->n_segments];
    seg_filename = oshmem_get_unique_file_name(oshmem_my_proc_id());
    ret = mca_sshmem_segment_create(s, seg_filename, size, hint);

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
    int i;

    if (!map) {
        return;
    }

    for (i = 0; i < map->n_segments; ++i) {
        map_segment_t *s = &map->mem_segs[i];
        if (s->type != MAP_SEGMENT_STATIC) {
            mca_sshmem_segment_detach(s, NULL);
            mca_sshmem_unlink(s);
        }
    }
}

int mca_memheap_alloc_with_hint(size_t size, long hint, void** ptr)
{
    int i;

    for (i = 0; i < mca_memheap_base_map.n_segments; i++) {
        map_segment_t *s = &mca_memheap_base_map.mem_segs[i];
        if (s->allocator && (hint && s->alloc_hints)) {
            /* Do not fall back to default allocator since it will break the
             * symmetry between PEs
             */
            return s->allocator->realloc(s, size, NULL, ptr);
        }
    }

    return MCA_MEMHEAP_CALL(alloc(size, ptr));
}
