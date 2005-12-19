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
#include <assert.h>
#include <stdlib.h>

#include "opal/memoryhooks/memory_internal.h"
     
/* Prototypes for our hooks.  */
void opal_memory_malloc_hooks_init(void);

static void local_free_hook(void*, const void*);
static void* local_malloc_hook(size_t, const void*);
static void* local_realloc_hook(void*, size_t, const void*);

/* Override initializing hook from the C library. */
void (*__malloc_initialize_hook) (void) = opal_memory_malloc_hooks_init;

/* local variable - next in stack of free hooks */
static void (*old_free_hook)(void*, const void*);
static void* (*old_realloc_hook)(void*, size_t, const void*);
static void* (*old_malloc_hook)(size_t, const void*);

int opal_memory_malloc_hooks_initialized = 0;

void
opal_memory_malloc_hooks_init(void)
{
    if (opal_memory_malloc_hooks_initialized != 0) {
        return;
    }

    opal_memory_malloc_hooks_initialized = 1;

    old_free_hook = __free_hook;
    old_malloc_hook = __malloc_hook;
    old_realloc_hook = __realloc_hook;

    __free_hook = local_free_hook;
    __malloc_hook = local_malloc_hook;
    __realloc_hook = local_realloc_hook;

    opal_mem_hooks_set_support(OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT);
    assert(__malloc_hook == local_malloc_hook);
}


static void
local_free_hook(void *ptr, const void *caller)
{
    if (__malloc_hook != local_malloc_hook) abort();
    /* dispatch about the pending free */
    opal_mem_hooks_release_hook(ptr, malloc_usable_size(ptr), 1);

    __free_hook = old_free_hook;

    /* call the next chain down */
    free(ptr);

    /* save the hooks again and restore our hook again */
    old_free_hook = __free_hook;
    __free_hook = local_free_hook;
}


static  void*
local_malloc_hook(size_t size, const void *caller)
{
    void *ret;

    __malloc_hook = old_malloc_hook;

    /* call the next chain down */
    ret = malloc(size);

    /* save the hooks again and restory our hack again */
    old_malloc_hook = __malloc_hook;
    __malloc_hook = local_malloc_hook;

    opal_mem_hooks_alloc_hook(ret, malloc_usable_size(ret), 1);

    assert(__malloc_hook == local_malloc_hook);

    return ret;
}



/* for better or worse, we must assume that the buffer being passed to
   realloc is not going to be expandable and therefore is going to be
   free()ed. */
static void*
local_realloc_hook(void *ptr, size_t size, const void *caller)
{
    void *ret;
    assert(__malloc_hook == local_malloc_hook);

    /* dispatch about the pending free */
    opal_mem_hooks_release_hook(ptr, malloc_usable_size(ptr), 1);

    /* realloc can call malloc (but not free).  Doing so with the
     * memory hooks causes us some interesting problems and causes
     * the malloc_hook to be left as NULL.  Pop the stack now so
     * that we don't see the memory registration twice.
     */
    __realloc_hook = old_realloc_hook;
    __malloc_hook = old_malloc_hook;

    /* call the next chain down */
    ret = realloc(ptr, size);

    /* save the hooks again and restore our hook again */
    old_realloc_hook = __realloc_hook;
    old_malloc_hook = __malloc_hook;
    __realloc_hook = local_realloc_hook;
    __malloc_hook = local_malloc_hook;

    opal_mem_hooks_alloc_hook(ret, malloc_usable_size(ret), 1);
    assert(__malloc_hook == local_malloc_hook);

    return ret;
}


#if 0
/* mmap is a weak symbol on any platform that I know of that
   supports malloc hooks, so we can just intercept it like this... */
void*
mmap(void* addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    void *ret;

    static void* (*realmmap)(void*, size_t, int, int, int, off_t);

    if (NULL == realmmap) {
        union { 
            void* (*mmap_fp)(void*, size_t, int, int, int, off_t);
            void *mmap_p;
        } tmp;

        tmp.mmap_p = dlsym(RTLD_NEXT, "mmap");
        realmmap = tmp.mmap_fp;
    }

    ret = realmmap(addr, len, prot, flags, fd, offset);

    opal_mem_hooks_alloc_hook(ret, len, 0);

    return ret;
}
#endif


/* munmap is a weak symbol on any platform that I know of that
   supports malloc hooks, so we can just intercept it like this... */
int 
munmap(void* addr, size_t len)
{
    static int (*realmunmap)(void*, size_t);
    /* dispatch about the pending release */
    opal_mem_hooks_release_hook(addr, len, 0);
    assert(__malloc_hook == local_malloc_hook);

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


