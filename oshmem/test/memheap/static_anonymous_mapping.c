/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#define _GNU_SOURCE

#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#include <shmem.h>

static int executable_global;

#if defined(__linux__) && defined(MAP_FIXED_NOREPLACE)
static void *map_anonymous_page(long page_size)
{
    static const uintptr_t candidates[] = {
        UINT64_C(0x200000000000),
        UINT64_C(0x300000000000),
        UINT64_C(0x400000000000),
    };
    size_t candidate;

    for (candidate = 0; candidate < sizeof(candidates) / sizeof(candidates[0]);
         ++candidate) {
        void *address = (void *) candidates[candidate];
        void *mapping = mmap(address, (size_t) page_size,
                             PROT_READ | PROT_WRITE,
                             MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE,
                             -1, 0);

        if (MAP_FAILED == mapping) {
            if (EEXIST == errno) {
                continue;
            }
            return MAP_FAILED;
        }
        if (address != mapping) {
            (void) munmap(mapping, (size_t) page_size);
            return MAP_FAILED;
        }
        return mapping;
    }

    errno = EEXIST;
    return MAP_FAILED;
}
#endif

int main(void)
{
#if !defined(__linux__) || !defined(MAP_FIXED_NOREPLACE)
    return 77;
#else
    long page_size;
    void *mapping;
    int anonymous_accessible;
    int global_accessible;
    int status = 0;

    page_size = sysconf(_SC_PAGESIZE);
    if (0 >= page_size) {
        return 77;
    }
    mapping = map_anonymous_page(page_size);
    if (MAP_FAILED == mapping) {
        return 77;
    }

    shmem_init();
    anonymous_accessible = shmem_addr_accessible(mapping, shmem_my_pe());
    global_accessible = shmem_addr_accessible(&executable_global, shmem_my_pe());
    shmem_finalize();

    if (0 != munmap(mapping, (size_t) page_size)) {
        fprintf(stderr, "munmap failed: %s\n", strerror(errno));
        return 1;
    }
    if (0 != anonymous_accessible) {
        fprintf(stderr, "anonymous mapping %p is reported symmetric\n", mapping);
        status = 1;
    }
    if (0 == global_accessible) {
        fprintf(stderr, "executable global %p is not reported symmetric\n",
                (void *) &executable_global);
        status = 1;
    }

    return status;
#endif
}
