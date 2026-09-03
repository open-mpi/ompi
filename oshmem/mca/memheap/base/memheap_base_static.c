/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2023-2026 NVIDIA Corporation.  All rights reserved.
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
#include "oshmem/mca/sshmem/base/base.h"
#include "oshmem/util/oshmem_util.h"
#include "opal/util/minmax.h"

#if defined(__linux__)
#include <link.h>
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#if defined(__linux__)
typedef struct {
    uintptr_t start;
    uintptr_t end;
} writable_load_range_t;

typedef struct {
    uintptr_t load_bias;
    writable_load_range_t *ranges;
    size_t count;
    size_t capacity;
    bool found_main;
    int status;
} executable_layout_t;

static executable_layout_t executable_layout;

static int add_uintptr(uintptr_t left, uintptr_t right, uintptr_t *result)
{
    if (NULL == result || UINTPTR_MAX - right < left) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    *result = left + right;
    return OSHMEM_SUCCESS;
}

static int append_writable_range(executable_layout_t *layout, uintptr_t start,
                                 uintptr_t end)
{
    writable_load_range_t *ranges;
    size_t new_capacity;

    if (NULL == layout || start >= end) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    if (layout->count == layout->capacity) {
        if (0 == layout->capacity) {
            new_capacity = 4;
        } else {
            if (SIZE_MAX / 2 < layout->capacity) {
                return OSHMEM_ERR_BAD_PARAM;
            }
            new_capacity = layout->capacity * 2;
        }
        if (SIZE_MAX / sizeof(*ranges) < new_capacity) {
            return OSHMEM_ERR_BAD_PARAM;
        }

        ranges = realloc(layout->ranges, new_capacity * sizeof(*ranges));
        if (NULL == ranges) {
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        layout->ranges = ranges;
        layout->capacity = new_capacity;
    }

    layout->ranges[layout->count].start = start;
    layout->ranges[layout->count].end = end;
    ++layout->count;
    return OSHMEM_SUCCESS;
}

static int find_main_executable(struct dl_phdr_info *info, size_t size,
                                void *data)
{
    executable_layout_t *layout = data;
    size_t i;

    (void) size;
    if (NULL != info->dlpi_name && '\0' != info->dlpi_name[0]) {
        return 0;
    }

    layout->found_main = true;
    layout->load_bias = (uintptr_t) info->dlpi_addr;
    for (i = 0; i < info->dlpi_phnum; ++i) {
        const ElfW(Phdr) *phdr = &info->dlpi_phdr[i];
        uintptr_t start;
        uintptr_t end;
        int rc;

        if (PT_LOAD != phdr->p_type || 0 == (phdr->p_flags & PF_W)
            || 0 == phdr->p_memsz) {
            continue;
        }
        if ((uintmax_t) UINTPTR_MAX < (uintmax_t) phdr->p_vaddr
            || (uintmax_t) UINTPTR_MAX < (uintmax_t) phdr->p_memsz) {
            layout->status = OSHMEM_ERR_BAD_PARAM;
            return 1;
        }

        rc = add_uintptr(layout->load_bias, (uintptr_t) phdr->p_vaddr,
                         &start);
        if (OSHMEM_SUCCESS == rc) {
            rc = add_uintptr(start, (uintptr_t) phdr->p_memsz, &end);
        }
        if (OSHMEM_SUCCESS == rc && start >= end) {
            rc = OSHMEM_ERR_BAD_PARAM;
        }
        if (OSHMEM_SUCCESS == rc) {
            rc = append_writable_range(layout, start, end);
        }
        if (OSHMEM_SUCCESS != rc) {
            layout->status = rc;
            return 1;
        }
    }

    return 1;
}

static int compare_writable_ranges(const void *left, const void *right)
{
    const writable_load_range_t *left_range = left;
    const writable_load_range_t *right_range = right;

    if (left_range->start < right_range->start) {
        return -1;
    }
    if (left_range->start > right_range->start) {
        return 1;
    }
    if (left_range->end < right_range->end) {
        return -1;
    }
    if (left_range->end > right_range->end) {
        return 1;
    }
    return 0;
}

static void coalesce_writable_ranges(executable_layout_t *layout)
{
    size_t input;
    size_t output = 0;

    if (1 < layout->count) {
        qsort(layout->ranges, layout->count, sizeof(*layout->ranges),
              compare_writable_ranges);
    }

    for (input = 0; input < layout->count; ++input) {
        if (0 == output
            || layout->ranges[output - 1].end < layout->ranges[input].start) {
            layout->ranges[output++] = layout->ranges[input];
            continue;
        }
        layout->ranges[output - 1].end =
            opal_max(layout->ranges[output - 1].end,
                     layout->ranges[input].end);
    }
    layout->count = output;
}

static int discover_executable_layout(void)
{
    int iterate_status;

    free(executable_layout.ranges);
    memset(&executable_layout, 0, sizeof(executable_layout));
    executable_layout.status = OSHMEM_SUCCESS;

    iterate_status = dl_iterate_phdr(find_main_executable,
                                     &executable_layout);
    if (OSHMEM_SUCCESS != executable_layout.status) {
        MEMHEAP_ERROR("failed to discover writable executable ELF ranges: %d",
                      executable_layout.status);
        return executable_layout.status;
    }
    if (!executable_layout.found_main || 0 == iterate_status) {
        MEMHEAP_ERROR("failed to locate the main executable ELF image");
        return OSHMEM_ERR_NOT_FOUND;
    }

    coalesce_writable_ranges(&executable_layout);
    return OSHMEM_SUCCESS;
}

static int append_static_segment(mca_memheap_map_t *map, int original_count,
                                 uintptr_t start, uintptr_t end)
{
    map_segment_t *segment;
    uintptr_t segment_size;

    if (NULL == map || start >= end) {
        return OSHMEM_ERR_BAD_PARAM;
    }
    segment_size = end - start;
    if ((uintmax_t) SIZE_MAX < (uintmax_t) segment_size) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    if (map->n_segments > original_count) {
        segment = &map->mem_segs[map->n_segments - 1];
        if (MAP_SEGMENT_STATIC == segment->type
            && (uintptr_t) segment->super.va_end == start) {
            MEMHEAP_VERBOSE(5, "Coalescing static segment");
            segment->super.va_end = (void *) end;
            segment->seg_size =
                (uintptr_t) segment->super.va_end
                - (uintptr_t) segment->super.va_base;
            return OSHMEM_SUCCESS;
        }
    }

    if (0 > map->n_segments || map->capacity < map->n_segments
        || (map->n_segments == map->capacity
            && INT_MAX / 2 < map->capacity)) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }
    segment = mca_memheap_base_allocate_segment(map);
    if (NULL == segment) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    memset(segment, 0, sizeof(*segment));
    MAP_SEGMENT_RESET_FLAGS(segment);
    segment->seg_id = MAP_SEGMENT_SHM_INVALID;
    segment->super.va_base = (void *) start;
    segment->super.va_end = (void *) end;
    segment->seg_size = (size_t) segment_size;
    segment->type = MAP_SEGMENT_STATIC;
    ++map->n_segments;
    MEMHEAP_VERBOSE(5, "add static intersection: %p-%p",
                    segment->super.va_base, segment->super.va_end);
    return OSHMEM_SUCCESS;
}

static int add_static_interval(mca_memheap_map_t *map, int original_count,
                               uintptr_t start, uintptr_t end)
{
    int i;

    if (start >= end) {
        return OSHMEM_SUCCESS;
    }

    for (i = 0; i < original_count; ++i) {
        uintptr_t excluded_start;
        uintptr_t excluded_end;
        int rc;

        if (MAP_SEGMENT_STATIC == map->mem_segs[i].type) {
            continue;
        }
        excluded_start = (uintptr_t) map->mem_segs[i].super.va_base;
        excluded_end = (uintptr_t) map->mem_segs[i].super.va_end;
        if (excluded_start >= excluded_end) {
            return OSHMEM_ERR_BAD_PARAM;
        }
        if (excluded_end <= start || excluded_start >= end) {
            continue;
        }

        if (start < excluded_start) {
            rc = add_static_interval(map, original_count, start,
                                     opal_min(end, excluded_start));
            if (OSHMEM_SUCCESS != rc) {
                return rc;
            }
        }
        if (excluded_end < end) {
            return add_static_interval(map, original_count,
                                       opal_max(start, excluded_end), end);
        }
        return OSHMEM_SUCCESS;
    }

    return append_static_segment(map, original_count, start, end);
}
#endif /* defined(__linux__) */

int mca_memheap_base_static_init(mca_memheap_map_t *map)
{
#if defined(__linux__)
    int rc;
    int original_count;
    int i;
    uint64_t total_mem = 0;
    FILE *maps = NULL;
    char line[4096];

    assert(map);
    assert(HEAP_SEG_INDEX < map->n_segments);
    original_count = map->n_segments;

    rc = discover_executable_layout();
    if (OSHMEM_SUCCESS != rc) {
        goto out;
    }

    maps = fopen("/proc/self/maps", "r");
    if (NULL == maps) {
        MEMHEAP_ERROR("Failed to open /proc/self/maps");
        rc = OSHMEM_ERROR;
        goto out;
    }

    while (NULL != fgets(line, sizeof(line), maps)) {
        unsigned long long parsed_start;
        unsigned long long parsed_end;
        uintptr_t map_start;
        uintptr_t map_end;
        char perms[5];
        size_t range_index;
        int fields;

        if (NULL == strchr(line, '\n') && !feof(maps)) {
            MEMHEAP_ERROR("truncated /proc/self/maps entry");
            rc = OSHMEM_ERR_BAD_PARAM;
            goto out;
        }
        fields = sscanf(line, "%llx-%llx %4s", &parsed_start, &parsed_end,
                        perms);
        if (3 != fields || parsed_start >= parsed_end
            || (uintmax_t) UINTPTR_MAX < (uintmax_t) parsed_start
            || (uintmax_t) UINTPTR_MAX < (uintmax_t) parsed_end) {
            MEMHEAP_ERROR("invalid /proc/self/maps entry: %s", line);
            rc = OSHMEM_ERR_BAD_PARAM;
            goto out;
        }
        if (4 != strlen(perms) || 'w' != perms[1] || 'p' != perms[3]) {
            continue;
        }

        map_start = (uintptr_t) parsed_start;
        map_end = (uintptr_t) parsed_end;
        for (range_index = 0; range_index < executable_layout.count;
             ++range_index) {
            uintptr_t intersection_start =
                opal_max(map_start,
                         executable_layout.ranges[range_index].start);
            uintptr_t intersection_end =
                opal_min(map_end, executable_layout.ranges[range_index].end);

            if (intersection_start < intersection_end) {
                rc = add_static_interval(map, original_count,
                                         intersection_start,
                                         intersection_end);
                if (OSHMEM_SUCCESS != rc) {
                    MEMHEAP_ERROR("failed to add static executable interval: %d",
                                  rc);
                    goto out;
                }
            }
        }
    }
    if (ferror(maps)) {
        MEMHEAP_ERROR("Failed to read /proc/self/maps");
        rc = OSHMEM_ERROR;
        goto out;
    }

    for (i = original_count; i < map->n_segments; ++i) {
        uint64_t segment_size = (uint64_t) map->mem_segs[i].seg_size;

        if (UINT64_MAX - total_mem < segment_size) {
            MEMHEAP_ERROR("static executable memory size overflow");
            rc = OSHMEM_ERR_BAD_PARAM;
            goto out;
        }
        total_mem += segment_size;
    }

    MEMHEAP_VERBOSE(1,
                    "Memheap static memory: %llu byte(s), %d segments",
                    (unsigned long long) total_mem, map->n_segments);
    rc = OSHMEM_SUCCESS;

out:
    if (NULL != maps) {
        fclose(maps);
    }
    free(executable_layout.ranges);
    executable_layout.ranges = NULL;
    executable_layout.count = 0;
    executable_layout.capacity = 0;
    if (OSHMEM_SUCCESS != rc) {
        executable_layout.found_main = false;
        map->n_segments = original_count;
    }
    return rc;
#else
    assert(map);
    MEMHEAP_ERROR("main-executable writable ELF layout discovery is unsupported on this platform");
    return OSHMEM_ERR_NOT_SUPPORTED;
#endif
}

void mca_memheap_base_static_exit(mca_memheap_map_t *map)
{
    assert(map);
}

int mca_memheap_base_static_segment_offset(const map_segment_t *segment,
                                           uint64_t *offset)
{
#if defined(__linux__)
    uintptr_t base;
    uintmax_t relative;

    if (NULL == segment || NULL == offset
        || MAP_SEGMENT_STATIC != segment->type
        || !executable_layout.found_main) {
        return OSHMEM_ERR_BAD_PARAM;
    }
    base = (uintptr_t) segment->super.va_base;
    if (base < executable_layout.load_bias) {
        return OSHMEM_ERR_BAD_PARAM;
    }
    relative = (uintmax_t) (base - executable_layout.load_bias);
    if (UINT64_MAX < relative) {
        return OSHMEM_ERR_BAD_PARAM;
    }
    *offset = (uint64_t) relative;
    return OSHMEM_SUCCESS;
#else
    (void) segment;
    (void) offset;
    return OSHMEM_ERR_NOT_SUPPORTED;
#endif
}
