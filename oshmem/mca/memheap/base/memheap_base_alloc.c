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
#include "ompi/util/timings.h"
#include "opal/util/minmax.h"


int mca_memheap_base_alloc_init(mca_memheap_map_t *map, size_t size, long hint,
                                char *timing_prefix)
{
    int ret = OSHMEM_SUCCESS;
    char * seg_filename = NULL;

    OPAL_TIMING_ENV_INIT_PREFIX(timing_prefix, timing);

    assert(map);
    if (hint == 0) {
        assert(HEAP_SEG_INDEX == map->n_segments);
    } else {
        assert(HEAP_SEG_INDEX < map->n_segments);
    }

    map_segment_t *s = mca_memheap_base_allocate_segment(map);
    if (NULL == s) {
        MEMHEAP_ERROR("failed to allocate segment");
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    seg_filename = oshmem_get_unique_file_name(oshmem_my_proc_id());

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_get_unique_file_name()");

    ret = mca_sshmem_segment_create(s, seg_filename, size, hint);

    OPAL_TIMING_ENV_NEXT(timing, "mca_sshmem_segment_create()");

    if (OSHMEM_SUCCESS == ret) {
        map->n_segments++;
        MEMHEAP_VERBOSE(1,
                        "Memheap alloc memory: %llu byte(s), %d segments by method: %d",
                        (unsigned long long)size, map->n_segments, s->type);
    }

    free(seg_filename);
    OPAL_TIMING_ENV_NEXT(timing, "DONE");

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

    free(map->mem_segs);
    map->n_segments = 0;
    map->capacity = 0;
    map->mem_segs = NULL;
}

int mca_memheap_alloc_with_hint(size_t size, long hint, void** ptr)
{
    int i;

    for (i = 0; i < mca_memheap_base_map.n_segments; i++) {
        map_segment_t *s = &mca_memheap_base_map.mem_segs[i];
        if (s->allocator && (hint & s->alloc_hints)) {
            /* Do not fall back to default allocator since it will break the
             * symmetry between PEs
             */
            return s->allocator->sa_realloc(s, size, NULL, ptr);
        }
    }

    return MCA_MEMHEAP_CALL(alloc(size, ptr));
}

map_segment_t *mca_memheap_base_allocate_segment(mca_memheap_map_t *map)
{
    static int warned = 0;
    map_segment_t *segments;
    int capacity;

    assert(map->n_segments <= map->capacity);

    if (!warned && (map->n_segments > mca_memheap_num_segments_warn)) {
        MEMHEAP_WARN("too many segments are registered: %d. This may cause "
                     "performance degradation. Pls try adding --mca "
                     "memheap_base_max_segments <NUMBER> to mpirun/oshrun "
                     "command line to suppress this message", map->n_segments);
        warned = 1;
    }

    if (map->n_segments == map->capacity) {
        capacity = opal_max(map->capacity * 2, 4);
        segments = realloc(map->mem_segs, capacity * sizeof(*map->mem_segs));
        if (segments == NULL) {
            return NULL;
        }

        map->capacity = capacity;
        map->mem_segs = segments;
    }

    return &map->mem_segs[map->n_segments];
}
