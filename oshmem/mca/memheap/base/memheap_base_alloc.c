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


int mca_memheap_base_segment_add(mca_memheap_map_t *map)
{
    int index = -1;
    if (MCA_MEMHEAP_MAX_SEGMENTS <= map->n_segments) {
        MEMHEAP_ERROR("FAILED to obtain new segment: max number (%d) of segments descriptors is exhausted",
                      MCA_MEMHEAP_MAX_SEGMENTS);
        goto error;
    }
    index = map->n_segments;
    map->mem_segs_ptr = realloc(map->mem_segs_ptr,
                                sizeof(map->mem_segs_ptr[0]) * ++map->n_segments);
    if (NULL == map->mem_segs_ptr) {
        MEMHEAP_ERROR("FAILED to obtain new segment: OOM - failed to expand the descriptor buffer");
        assert(NULL != map->mem_segs_ptr);
    }
error:
    return index;
}

int mca_memheap_base_segment_add_fail(mca_memheap_map_t *map)
{
    if ( 0>= map->n_segments) {
        return -1;
    }
    map->num_transports--;
    map->mem_segs_ptr = realloc(map->mem_segs_ptr,
                                sizeof(map->mem_segs_ptr[0]) * map->n_segments);
    if (NULL == map->mem_segs_ptr) {
        MEMHEAP_ERROR("FAILED to handle add failure: OOM - failed to reduce the descriptor buffer");
        assert(NULL != map->mem_segs_ptr);
    }
    return 0;
}

void mca_memheap_base_segments_release(mca_memheap_map_t *map)
{
    if (map->n_segments) {
        free(map->mem_segs_ptr);
        map->mem_segs_ptr = NULL;
    }
}

int mca_memheap_base_alloc_init(mca_memheap_map_t *map, size_t size, long hint,
                                char *timing_prefix)
{
    int ret = OSHMEM_SUCCESS;
    char * seg_filename = NULL;
    map_segment_t *s = NULL;
    int index;

    OPAL_TIMING_ENV_INIT_PREFIX(timing_prefix, timing);

    assert(map);

    index = mca_memheap_base_segment_add(map);
    if (0 > index) {
        ret = OSHMEM_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    if (hint == 0) {
        assert(HEAP_SEG_INDEX == index);
    } else {
        assert(HEAP_SEG_INDEX < index);
    }

    s = mca_memheap_base_segment_get(map, index);
    assert(NULL != s);

    seg_filename = oshmem_get_unique_file_name(oshmem_my_proc_id());

    OPAL_TIMING_ENV_NEXT(timing, "oshmem_get_unique_file_name()");

    ret = mca_sshmem_segment_create(s, seg_filename, size, hint);

    OPAL_TIMING_ENV_NEXT(timing, "mca_sshmem_segment_create()");

    if (OSHMEM_SUCCESS != ret) {
        mca_memheap_base_segment_add_fail(map);
        free(seg_filename);
    } else {
        MEMHEAP_VERBOSE(1,
                        "Memheap alloc memory: %llu byte(s), %d segments by method: %d",
                        (unsigned long long)size, map->n_segments, s->type);
    }
    OPAL_TIMING_ENV_NEXT(timing, "DONE");

exit:
    return ret;
}

void mca_memheap_base_alloc_exit(mca_memheap_map_t *map)
{
    int i;

    if (!map) {
        return;
    }

    for (i = 0; i < map->n_segments; ++i) {
        map_segment_t *s = mca_memheap_base_segment_get(map, i);
        assert(NULL != s);
        if (s->type != MAP_SEGMENT_STATIC) {
            mca_sshmem_segment_detach(s, NULL);
            mca_sshmem_unlink(s);
        }
    }
    mca_memheap_base_segments_release(map);
}

int mca_memheap_alloc_with_hint(size_t size, long hint, void** ptr)
{
    int i;

    for (i = 0; i < mca_memheap_base_map.n_segments; i++) {
        map_segment_t *s = mca_memheap_base_segment_get(&mca_memheap_base_map, i);
        assert(NULL != s);
        if (s->allocator && (hint & s->alloc_hints)) {
            /* Do not fall back to default allocator since it will break the
             * symmetry between PEs
             */
            return s->allocator->sa_realloc(s, size, NULL, ptr);
        }
    }

    return MCA_MEMHEAP_CALL(alloc(size, ptr));
}
