/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
#include "oshmem/util/oshmem_util.h"

#include <stdio.h>

struct map_segment_desc {
    void* start;
    void* end;
    char perms[8];
    uint64_t offset;
    char dev[8];
    uint64_t inode;
    char pathname[MAXPATHLEN];
};

typedef struct memheap_static_context {
    struct {
        void* start;
        void* end;
    } mem_segs[MCA_MEMHEAP_MAX_SEGMENTS];
    int n_segments;
} memheap_static_context_t;

static memheap_static_context_t memheap_context;

static int _load_segments(void);
static int _check_perms(struct map_segment_desc *seg);
static int _check_address(struct map_segment_desc *seg);
static int _check_pathname(struct map_segment_desc *seg);

int mca_memheap_base_static_init(mca_memheap_map_t *map)
{
    /* read and parse segments from /proc/self/maps */
    int ret = OSHMEM_SUCCESS;

    assert(map);
    assert(SYMB_SEG_INDEX <= map->n_segments);

    ret = _load_segments();

    if (OSHMEM_SUCCESS == ret) {
        int i;
        size_t total_mem;

        for (i = 0, total_mem = 0; i < memheap_context.n_segments; i++) {
            map_segment_t *s = &map->mem_segs[map->n_segments];

            memset(s, 0, sizeof(*s));
            MAP_SEGMENT_RESET_FLAGS(s);
            s->seg_id = MAP_SEGMENT_SHM_INVALID;
            s->seg_base_addr = memheap_context.mem_segs[i].start;
            s->end = memheap_context.mem_segs[i].end;
            s->seg_size = ((uintptr_t)s->end - (uintptr_t)s->seg_base_addr);
            s->type = MAP_SEGMENT_STATIC;
            map->n_segments++;

            total_mem += ((uintptr_t)s->end - (uintptr_t)s->seg_base_addr);
        }
        MEMHEAP_VERBOSE(1,
                        "Memheap static memory: %llu byte(s), %d segments",
                        (unsigned long long)total_mem, map->n_segments);
    }

    return ret;
}

void mca_memheap_base_static_exit(mca_memheap_map_t *map)
{
    assert(map);
}

static int _check_perms(struct map_segment_desc *seg)
{
    if (!strcmp(seg->perms, "rw-p") || !strcmp(seg->perms, "rwxp"))
        return OSHMEM_SUCCESS;

    return OSHMEM_ERROR;
}

static int _check_address(struct map_segment_desc *seg)
{
    /* FIXME Linux specific code */
#ifdef __linux__
    extern unsigned _end;
    void* data_end = &_end;

    /**
     * SGI shmem only supports globals&static in main program.
     * It does not support them in shared objects or in dlopen()
     * (Clarified on PGAS 2011 tutorial)
     *
     * So ignored any maps that start higher then process _end
     * FIXME: make sure we do not register symmetric heap twice
     * if we decide to allow shared objects
     */
    if ((uintptr_t)seg->start > (uintptr_t)data_end) {
        MEMHEAP_VERBOSE(100,
                        "skip segment: data _end < segment start (%p < %p)",
                        data_end, seg->start);
        return OSHMEM_ERROR;
    }

    if ((uintptr_t)seg->end > (uintptr_t)data_end) {
        MEMHEAP_VERBOSE(100,
                        "adjust segment: data _end < segment end (%p < %p",
                        data_end, seg->end);
         seg->end = data_end;
    }
#endif
    return OSHMEM_SUCCESS;
}

static int _check_pathname(struct map_segment_desc *seg)
{
    /* Probably we need to check found path but
     * To press check coverity issue following code is disabled
     */
#if 0
    char *p;
    if ('\0' == seg->pathname[0])
    return OSHMEM_SUCCESS;

    if (0 == strncmp(seg->pathname, "/lib", 4))
    return OSHMEM_ERROR;

    if (0 == strncmp(seg->pathname, "/usr/lib", 8))
    return OSHMEM_ERROR;

    if (0 == strncmp(seg->pathname, "/dev", 4))
    return OSHMEM_ERROR;

    if (0 == strcmp(seg->pathname, "[stack]"))
    return OSHMEM_ERROR;

    if (0 == strcmp(seg->pathname, "[vdso]"))
    return OSHMEM_ERROR;

    if (0 == strcmp(seg->pathname, "[vsyscall]"))
    return OSHMEM_ERROR;

    p = rindex(seg->pathname, '/');
    if (p) {
        if (0 == strncmp(p+1, "libshmem.so", 11))
        return OSHMEM_ERROR;

        if (0 == strncmp(p+1, "libmpi.so", 9))
        return OSHMEM_ERROR;

        if (0 == strncmp(p+1, "libmca_common_sm.so", 19))
        return OSHMEM_ERROR;
    }
#endif
    return OSHMEM_SUCCESS;
}

static int _load_segments(void)
{
    FILE *fp;
    char line[1024];
    struct map_segment_desc seg;

    memheap_context.n_segments = 0;
    /* FIXME!!! Linux specific code */
    fp = fopen("/proc/self/maps", "r");
    if (NULL == fp) {
        MEMHEAP_ERROR("Failed to open /proc/self/maps");
        return OSHMEM_ERROR;
    }

    while (NULL != fgets(line, sizeof(line), fp)) {
        memset(&seg, 0, sizeof(seg));
        if (3 > sscanf(line,
               "%llx-%llx %s %llx %s %llx %s",
               (unsigned long long *) &seg.start,
               (unsigned long long *) &seg.end,
               seg.perms,
               (unsigned long long *) &seg.offset,
               seg.dev,
               (unsigned long long *) &seg.inode,
               seg.pathname)) {
            MEMHEAP_ERROR("Failed to sscanf /proc/self/maps output %s", line);
            fclose(fp);
            return OSHMEM_ERROR;
        }

        if (OSHMEM_ERROR == _check_address(&seg))
            continue;

        if (OSHMEM_ERROR == _check_pathname(&seg))
            continue;

        if (OSHMEM_ERROR == _check_perms(&seg))
            continue;

        MEMHEAP_VERBOSE(5, "add: %s", line);
        if (MCA_MEMHEAP_MAX_SEGMENTS <= memheap_context.n_segments) {
            MEMHEAP_ERROR("too many segments (max = %d): skip %s",
                          MCA_MEMHEAP_MAX_SEGMENTS, line);
            continue;
        }
        if (memheap_context.n_segments > 0
                && seg.start
                        == memheap_context.mem_segs[memheap_context.n_segments
                                - 1].end) {
            MEMHEAP_VERBOSE(5, "Coalescing segment");
            memheap_context.mem_segs[memheap_context.n_segments - 1].end =
                    seg.end;
        } else {
            memheap_context.mem_segs[memheap_context.n_segments].start =
                    seg.start;
            memheap_context.mem_segs[memheap_context.n_segments].end = seg.end;
            memheap_context.n_segments++;
        }
    }

    fclose(fp);
    return OSHMEM_SUCCESS;
}
