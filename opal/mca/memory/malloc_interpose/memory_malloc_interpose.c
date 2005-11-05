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

#define OMPI_DISABLE_ENABLE_MEM_DEBUG 1
#include "ompi_config.h"

#include <sys/mman.h>
#include <stdlib.h>
#include <malloc.h>
#include <dlfcn.h>

#include "opal/include/constants.h"
#include "opal/mca/memory/memory.h"
#include "opal/memory/memory_internal.h"

static int opal_memory_malloc_interpose_open(void);

const opal_memory_base_component_1_0_0_t mca_memory_malloc_interpose_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a memory v1.0.0 component (which also
           implies a specific MCA version) */
        OPAL_MEMORY_BASE_VERSION_1_0_0,

        /* Component name and version */
        "malloc_interpose",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_memory_malloc_interpose_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },
};

#define FIND_REALFREE() \
    do { \
        if (NULL == realfree) { \
            union { \
                void (*free_fp)(void*); \
                void *free_p; \
            } tmp; \
            tmp.free_p = dlsym(RTLD_NEXT, "free"); \
            realfree = tmp.free_fp; \
        } \
    } while (0);

#define FIND_REALREALLOC() \
    do { \
        if (NULL == realrealloc) { \
            union { \
                void* (*realloc_fp)(void*, size_t); \
                void* realloc_p; \
            } tmp; \
            tmp.realloc_p = dlsym(RTLD_NEXT, "realloc"); \
            realrealloc = tmp.realloc_fp; \
        } \
    } while (0);

#define FIND_REALMUNMAP() \
    do { \
        if (NULL == realmunmap) { \
            union { \
                int (*munmap_fp)(void*, size_t); \
                void *munmap_p; \
            } tmp; \
            tmp.munmap_p = dlsym(RTLD_NEXT, "munmap"); \
            realmunmap = tmp.munmap_fp; \
        } \
    } while (0);

static void (*realfree)(void*);
static void* (*realrealloc)(void*, size_t);
static int (*realmunmap)(void*, size_t);

static int
opal_memory_malloc_interpose_open(void)
{
    opal_mem_free_set_free_support(1);

    FIND_REALFREE();
    FIND_REALREALLOC();
    FIND_REALMUNMAP();

    if (NULL == realfree || NULL == realrealloc || NULL == realmunmap) {
        /* this shoudl really never happen */
        fprintf(stderr, 
                "Could not find real memory functions.  Aborting in dispair\n");
        abort();
    }

    return OPAL_SUCCESS;
}


void
free(void *ptr)
{
    FIND_REALFREE();

    /* dispatch about the pending release */
    opal_mem_free_release_hook(ptr, malloc_usable_size(ptr));    
    realfree(ptr);
}


void*
realloc(void *ptr, size_t size)
{
    FIND_REALREALLOC();

    /* dispatch about the pending release */
    opal_mem_free_release_hook(ptr, malloc_usable_size(ptr));
    return realrealloc(ptr, size);
}


int
munmap(void *start, size_t length)
{
    FIND_REALMUNMAP();

    /* dispatch about the pending release */
    opal_mem_free_release_hook(start, length);
    return realmunmap(start, length);
}
