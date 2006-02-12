/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <malloc/malloc.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <dlfcn.h>

#include "opal/constants.h"
#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory_internal.h"

static int opal_memory_darwin_open(void);

static void* opal_memory_darwin_malloc(struct _malloc_zone_t *zone, size_t size);
static void* opal_memory_darwin_calloc(struct _malloc_zone_t *zone, size_t num_items, 
                                       size_t size);
static void* opal_memory_darwin_valloc(struct _malloc_zone_t *zone, size_t size);
static void opal_memory_darwin_free(struct _malloc_zone_t *zone, void *ptr);
static void* opal_memory_darwin_realloc(struct _malloc_zone_t *zone, 
                                        void *ptr, size_t size);

const opal_memory_base_component_1_0_0_t mca_memory_darwin_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a memory v1.0.0 component (which also
           implies a specific MCA version) */
        OPAL_MEMORY_BASE_VERSION_1_0_0,

        /* Component name and version */
        "darwin",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_memory_darwin_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },
};

static void* (*next_malloc)(struct _malloc_zone_t *zone, size_t size);
static void* (*next_calloc)(struct _malloc_zone_t *zone, size_t num_items, size_t size);
static void* (*next_valloc)(struct _malloc_zone_t *zone, size_t size);
static void  (*next_free)(struct _malloc_zone_t *zone, void *ptr);
static void* (*next_realloc)(struct _malloc_zone_t *zone, void *ptr, size_t size);


static int
opal_memory_darwin_open(void)
{
    /* add hooks to intercept free */
    malloc_zone_t *default_zone;

    default_zone = malloc_default_zone();

    /* save the pointers first, so that we can call them as soon as we
       replace the hooks below (think threads) */
    next_malloc = default_zone->malloc;
    next_calloc = default_zone->calloc;
    next_valloc = default_zone->valloc;
    next_free = default_zone->free;
    next_realloc = default_zone->realloc;

    default_zone->malloc = opal_memory_darwin_malloc;
    default_zone->calloc = opal_memory_darwin_calloc;
    default_zone->valloc = opal_memory_darwin_valloc;
    default_zone->free = opal_memory_darwin_free;
    default_zone->realloc = opal_memory_darwin_realloc;

    opal_mem_hooks_set_support(OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT);

    return OPAL_SUCCESS;
}

static void *
opal_memory_darwin_malloc(struct _malloc_zone_t *zone, size_t size)
{
    void *tmp = next_malloc(zone, size);
    opal_mem_hooks_alloc_hook(tmp, malloc_size(tmp), 1);
    return tmp;
}


static void*
opal_memory_darwin_calloc(struct _malloc_zone_t *zone, size_t num_items, 
                          size_t size)
{
    void *tmp = next_calloc(zone, num_items, size);
    opal_mem_hooks_alloc_hook(tmp, malloc_size(tmp), 1);
    return tmp;
}


static void*
opal_memory_darwin_valloc(struct _malloc_zone_t *zone, size_t size)
{
    void *tmp = next_valloc(zone, size);
    opal_mem_hooks_alloc_hook(tmp, malloc_size(tmp), 1);
    return tmp;
}


static void
opal_memory_darwin_free(struct _malloc_zone_t *zone, void *ptr)
{
    opal_mem_hooks_release_hook(ptr, malloc_size(ptr), 1);
    next_free(zone, ptr);
}


static void * 
opal_memory_darwin_realloc(struct _malloc_zone_t *zone, 
                           void *ptr, size_t size)
{
    char *tmp;

    opal_mem_hooks_release_hook(ptr, malloc_size(ptr), 1);
    tmp = next_realloc(zone, ptr, size);
    opal_mem_hooks_alloc_hook(tmp, malloc_size(tmp), 1);

    return tmp;
}


/* only need to catch mmap / munmap for user code, and we should be at the
   far right of the library stack, so this should work.  Darwin 7 and
   later include dlsym as part of libSystem, so no need to do anything
   special for it.  Not sure what would happen if you tried to
   statically link your application (as in -Bstatic, not libmpi.a),
   but since Apple doesn't support that, neither do we. */
void*
mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    static void* (*realmmap)(void *, size_t, int, int, int, off_t);
    void *tmp;

    if (NULL == realmmap) {
        union { 
            void* (*mmap_fp)(void *, size_t, int, int, int, off_t);
            void *mmap_p;
        } tmp;

        tmp.mmap_p = dlsym(RTLD_NEXT, "mmap");
        realmmap = tmp.mmap_fp;
    }

    tmp = realmmap(addr, len, prot, flags, fd, offset);
    opal_mem_hooks_alloc_hook(tmp, len, 0);

    return tmp;
}


int 
munmap(void* addr, size_t len)
{
    static int (*realmunmap)(void*, size_t);

    /* dispatch about the pending release */
    opal_mem_hooks_release_hook(addr, len, 0);

    if (NULL == realmunmap) {
        union { 
            int (*munmap_fp)(void*, size_t);
            void *munmap_p;
        } tmp;

        tmp.munmap_p = dlsym(RTLD_NEXT, "munmap");
        realmunmap = tmp.munmap_fp;
    }

    return realmunmap(addr, len);
}
