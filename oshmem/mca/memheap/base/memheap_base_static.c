/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2023      NVIDIA Corporation.  All rights reserved.
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

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <pthread.h>

static int _check_perms(const char *perm);
static int _check_non_static_segment(const map_segment_t *mem_segs,
                                     int n_segment,
                                     const void *start, const void *end);
static int _check_address(void *start, void **end);
static int _check_pathname(uint64_t inode, const char *pathname);

int mca_memheap_base_static_init(mca_memheap_map_t *map)
{
    /* read and parse segments from /proc/self/maps */
    int ret = OSHMEM_SUCCESS;
    int n_segments = map->n_segments;
    uint64_t total_mem = 0;
    void* start;
    void* end;
    char perms[8];
    uint64_t offset;
    char dev[8];
    uint64_t inode;
    char pathname[OPAL_PATH_MAX];
    FILE *fp;
    char line[1024];
    map_segment_t *s;

    assert(map);
    assert(HEAP_SEG_INDEX < map->n_segments);

    /* FIXME!!! Linux specific code */
    fp = fopen("/proc/self/maps", "r");
    if (NULL == fp) {
        MEMHEAP_ERROR("Failed to open /proc/self/maps");
        return OSHMEM_ERROR;
    }

    while (NULL != fgets(line, sizeof(line), fp)) {
        if (3 > sscanf(line,
               "%llx-%llx %s %llx %s %llx %s",
               (unsigned long long *) &start,
               (unsigned long long *) &end,
               perms,
               (unsigned long long *) &offset,
               dev,
               (unsigned long long *) &inode,
               pathname)) {
            MEMHEAP_ERROR("Failed to sscanf /proc/self/maps output %s", line);
            ret = OSHMEM_ERROR;
            goto out;
        }

        if (OSHMEM_ERROR == _check_non_static_segment(
                                                   map->mem_segs, n_segments,
                                                   start, end)) {
            continue;
        }

        if (OSHMEM_ERROR == _check_address(start, &end))
            continue;

        if (OSHMEM_ERROR == _check_pathname(inode, pathname))
            continue;

        if (OSHMEM_ERROR == _check_perms(perms))
            continue;

        MEMHEAP_VERBOSE(5, "add: %s", line);

        if ((map->n_segments > 0) &&
            (start == map->mem_segs[map->n_segments - 1].super.va_end)) {
            s = &map->mem_segs[map->n_segments - 1];
            MEMHEAP_VERBOSE(5, "Coalescing segment");
            s->super.va_end = end;
            s->seg_size = ((uintptr_t)s->super.va_end - (uintptr_t)s->super.va_base);
            continue;
        }

        s = mca_memheap_base_allocate_segment(map);
        if (NULL == s) {
            MEMHEAP_ERROR("failed to allocate segment");
            ret = OSHMEM_ERR_OUT_OF_RESOURCE;
            goto out;
        }

        memset(s, 0, sizeof(*s));
        MAP_SEGMENT_RESET_FLAGS(s);
        s->seg_id        = MAP_SEGMENT_SHM_INVALID;
        s->super.va_base = start;
        s->super.va_end  = end;
        s->seg_size      = ((uintptr_t)s->super.va_end - (uintptr_t)s->super.va_base);
        s->type          = MAP_SEGMENT_STATIC;
        map->n_segments++;

        total_mem += ((uintptr_t)s->super.va_end - (uintptr_t)s->super.va_base);
    }

    MEMHEAP_VERBOSE(1,
                    "Memheap static memory: %llu byte(s), %d segments",
                    total_mem, map->n_segments);

out:
    fclose(fp);
    return ret;
}

void mca_memheap_base_static_exit(mca_memheap_map_t *map)
{
    assert(map);
}

static int _check_perms(const char *perms)
{
    if (!strcmp(perms, "rw-p") || !strcmp(perms, "rwxp"))
        return OSHMEM_SUCCESS;

    return OSHMEM_ERROR;
}

static int _check_non_static_segment(const map_segment_t *mem_segs,
                                     int n_segment,
                                     const void *start, const void *end)
{
    int i;

    for (i = 0; i < n_segment; i++) {
        if ((start <= mem_segs[i].super.va_base) &&
            (mem_segs[i].super.va_base < end)) {
            MEMHEAP_VERBOSE(100,
                            "non static segment: %p-%p already exists as %p-%p",
                            start, end, mem_segs[i].super.va_base,
                            mem_segs[i].super.va_end);
            return OSHMEM_ERROR;
        }
    }

    return OSHMEM_SUCCESS;
}

static int _check_address(void *start, void **end)
{
    /* FIXME Linux specific code */
#ifdef __linux__
    extern unsigned _end;
    uintptr_t data_end = (uintptr_t)&_end;

    /**
     * SGI shmem only supports globals&static in main program.
     * It does not support them in shared objects or in dlopen()
     * (Clarified on PGAS 2011 tutorial).
     *
     * So ignored any maps that start higher then process _end.
     */
    if ((uintptr_t)start > data_end) {
        MEMHEAP_VERBOSE(100,
                        "skip segment: data _end < segment start (%p < %p)",
                        data_end, start);
        return OSHMEM_ERROR;
    }

    if ((uintptr_t)*end > data_end) {
        MEMHEAP_VERBOSE(100,
                        "adjust segment: data _end < segment end (%p < %p",
                        data_end, *end);
         *end = (void*)data_end;
    }
#endif
    return OSHMEM_SUCCESS;
}

static int _check_pathname(uint64_t inode, const char *pathname)
{
    static const char *proc_self_exe = "/proc/self/exe";
    static int warned = 0;
    char exe_path[OPAL_PATH_MAX];
    char module_path[OPAL_PATH_MAX];
    char *path;

    if (0 == inode) {
        /* segment is not mapped to file, allow sharing it */
        return OSHMEM_SUCCESS;
    }

    path = realpath(proc_self_exe, exe_path);
    if (NULL == path) {
        if (0 == warned) {
            MEMHEAP_VERBOSE(100, "failed to read link %s: %m", proc_self_exe);
            MEMHEAP_VERBOSE(100, "all segments will be registered");
            warned = 1;
        }

        return OSHMEM_SUCCESS;
    }

    /* for file-mapped segments allow segments from start process only */
    path = realpath(pathname, module_path);
    if (NULL == path) {
        return OSHMEM_ERROR;
    }

    if (!strncmp(exe_path, module_path, sizeof(exe_path))) {
        return OSHMEM_SUCCESS;
    }

    return OSHMEM_ERROR;

    /* Probably we need more accurate path check
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

        if (0 == strncmp(p+1, "lib" OMPI_LIBMPI_NAME ".so", 9))
        return OSHMEM_ERROR;

        if (0 == strncmp(p+1, "libmca_common_sm.so", 19))
        return OSHMEM_ERROR;
    }
#endif
    return OSHMEM_SUCCESS;
}

