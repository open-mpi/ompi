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

#include <sys/types.h>
#include <sys/mman.h>
#include <stdio.h>
#define __USE_GNU
#include <dlfcn.h>
#include <malloc.h>

#include "opal/memory/memory_internal.h"
     
/* Prototypes for our hooks.  */
void opal_memory_malloc_hooks_init(void);
static void opal_mem_free_free_hook (void*, const void *);
static void* opal_mem_free_realloc_hook (void*, size_t, const void *);
     
/* Override initializing hook from the C library. */
void (*__malloc_initialize_hook) (void) = opal_memory_malloc_hooks_init;


/* local variable - next in stack of free hooks */
static void (*old_free_hook)(void*, const void*);
static void* (*old_realloc_hook)(void*, size_t, const void*);

static int initialized = 0;

void
opal_memory_malloc_hooks_init(void)
{
    /* for some dumb reason, when libopal is compiled statically a C
       application will fire this at malloc initialization time, but a
       C++ application will not.  So we also try to set our hooks from
       the module initialization */
    if (initialized != 0) {
        return;
    }

    initialized = 1;
    old_free_hook = __free_hook;
    old_realloc_hook = __realloc_hook;
    __free_hook = opal_mem_free_free_hook;
    __realloc_hook = opal_mem_free_realloc_hook;
    opal_mem_free_set_free_support(1);
}


static void
opal_mem_free_free_hook (void *ptr, const void *caller)
{
    /* dispatch about the pending free */
    opal_mem_free_release_hook(ptr, malloc_usable_size(ptr));

    __free_hook = old_free_hook;

    /* call the next chain down */
    free(ptr);

    /* save the hooks again and restore our hook again */
    old_free_hook = __free_hook;
    __free_hook = opal_mem_free_free_hook;
}


/* for better or worse, we must assume that the buffer being passed to
   realloc is not going to be expandable and therefore is going to be
   free()ed. */
static void*
opal_mem_free_realloc_hook (void *ptr, size_t size, const void *caller)
{
    void *ret;

    /* dispatch about the pending free */
    opal_mem_free_release_hook(ptr, malloc_usable_size(ptr));

    __realloc_hook = old_realloc_hook;

    /* call the next chain down */
    ret = realloc(ptr, size);

    /* save the hooks again and restore our hook again */
    old_realloc_hook = __realloc_hook;
    __realloc_hook = opal_mem_free_realloc_hook;

    return ret;
}


/* munmap is a weak symbol on any platform that I know of that
   supports malloc hooks, so we can just intercept it like this... */
int 
munmap(void* addr, size_t len)
{
    static int (*realmunmap)(void*, size_t);
    /* dispatch about the pending release */
    opal_mem_free_release_hook(addr, len);

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


