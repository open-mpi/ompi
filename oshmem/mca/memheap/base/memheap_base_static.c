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

struct map_segment_desc {
    uint64_t start;
    uint64_t end;
    char perms[8];
    uint64_t offset;
    char dev[8];
    uint64_t inode;
    char pathname[MAXPATHLEN];
};

typedef struct memheap_static_context
{
    struct
    {
        uint64_t start;
        uint64_t end;
    } mem_segs[MCA_MEMHEAP_MAX_SEGMENTS];
    int n_segments;
} memheap_static_context_t;

static memheap_static_context_t memheap_context;

static int __load_segments(void);
static int __check_perms(struct map_segment_desc *seg);
static int __check_address(struct map_segment_desc *seg);
static int __check_pathname(struct map_segment_desc *seg);


int mca_memheap_base_static_init(mca_memheap_map_t *map)
{
    /* read and parse segments from /proc/self/maps */
    int ret = OSHMEM_SUCCESS;

    assert(map);
    assert(SYMB_SEG_INDEX <= map->n_segments);

    ret = __load_segments();

    if (OSHMEM_SUCCESS == ret)
    {
        int i;
        size_t total_mem;

        for (i = 0, total_mem = 0; i < memheap_context.n_segments; i++) 
        {
            map_segment_t *s = &map->mem_segs[map->n_segments];

            memset(s, 0, sizeof(*s));
            s->is_active = 0;
            s->shmid = MEMHEAP_SHM_INVALID;
            s->start = memheap_context.mem_segs[i].start;
            s->end = memheap_context.mem_segs[i].end;
            s->size = s->end - s->start;
            s->type = MAP_SEGMENT_STATIC;
            s->context = NULL;
            map->n_segments++;

            total_mem += s->end - s->start;
        }
        MEMHEAP_VERBOSE(1, "Memheap static memory: %llu byte(s), %d segments",
                (unsigned long long)total_mem, map->n_segments);
    }

    return ret;
}


void mca_memheap_base_static_exit(mca_memheap_map_t *map)
{
    assert(map);
}


static int __check_perms(struct map_segment_desc *seg)
{
    if (!strcmp(seg->perms, "rw-p") || !strcmp(seg->perms, "rwxp"))
        return OSHMEM_SUCCESS;

    return OSHMEM_ERROR;
}


static int __check_address(struct map_segment_desc *seg)
{
    extern unsigned _end;
    unsigned long data_end = (unsigned long)&_end;

    /**
     * Sasha:
     * SGI shmem only supports globals&static in main program. 
     * It does not support them in shared objects or in dlopen()
     * (Clarified on PGAS 2011 tutorial)
     *
     * So ignored any maps that start higher then process _end
     * FIXME: make sure we do not register symmetric heap twice
     * if we decide to allow shared objects
     */
    if (seg->start > data_end) {
        MEMHEAP_VERBOSE(100, "skip segment: data _end < segment start (%llx < %llx)",
                (unsigned long long)data_end,
                (unsigned long long)seg->start
                );
        return OSHMEM_ERROR;
    }
    return OSHMEM_SUCCESS;
}


static int __check_pathname(struct map_segment_desc *seg)
{

    return OSHMEM_SUCCESS;
#if 0 /* To press check coverity issue */
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

    return OSHMEM_SUCCESS;
#endif
}


static int __load_segments(void)
{
    FILE *fp; 
    char line[1024];
    struct map_segment_desc seg;

    memheap_context.n_segments = 0;

    fp = fopen("/proc/self/maps", "r");
    if (NULL == fp) {
        MEMHEAP_ERROR("Failed to open /proc/self/maps");
        return OSHMEM_ERROR;
    }

    while (NULL != fgets(line, sizeof(line), fp)) {
        memset(&seg, 0, sizeof(seg));
        sscanf(line, "%llx-%llx %s %llx %s %llx %s", 
                (long long *)&seg.start,
                (long long *)&seg.end,
                seg.perms,
                (long long *)&seg.offset,
                seg.dev,
                (long long *)&seg.inode,
                seg.pathname);

        if (OSHMEM_ERROR == __check_address(&seg))
            continue;

        if (OSHMEM_ERROR == __check_pathname(&seg)) 
            continue;

        if (OSHMEM_ERROR == __check_perms(&seg))
            continue;

        MEMHEAP_VERBOSE(5, "add: %s", line);
        if (MCA_MEMHEAP_MAX_SEGMENTS <= memheap_context.n_segments) {
            MEMHEAP_ERROR("too many segments (max = %d): skip %s", 
                    MCA_MEMHEAP_MAX_SEGMENTS, line);
            continue;
        }
        if (memheap_context.n_segments > 0 && 
                seg.start == memheap_context.mem_segs[memheap_context.n_segments-1].end) {
            MEMHEAP_VERBOSE(5, "Coalescing segment");
            memheap_context.mem_segs[memheap_context.n_segments-1].end = seg.end;
        }
        else {
            memheap_context.mem_segs[memheap_context.n_segments].start = seg.start;
            memheap_context.mem_segs[memheap_context.n_segments].end = seg.end;
            memheap_context.n_segments ++;
        }
    }

    fclose(fp);
    return OSHMEM_SUCCESS;
}
