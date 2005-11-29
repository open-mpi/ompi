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

#include "opal/memoryhooks/memory_internal.h"

#include <stdlib.h>
#include <sys/mman.h>
#define __USE_GNU
#include <dlfcn.h>

static int (*realmunmap)(void*, size_t);

/* munmap is a weak symbol on any platform that I know of that
   supports malloc hooks, so we can just intercept it like this... */
int 
munmap(void* addr, size_t len)
{
    /* dispatch about the pending release */
    opal_mem_hooks_release_hook(addr, len);

    if (NULL == realmunmap) {
        realmunmap = (int (*)(void*, size_t)) dlsym(RTLD_NEXT, "munmap");
    }

    return realmunmap(addr, len);
}

/* put this here beacuse malloc.c really doesn't like dlfcn.h, but we
   need it for getting the right munmap... */
int
opal_mem_free_ptmalloc2_munmap(void *start, size_t length)
{
    opal_mem_hooks_release_hook(start, length);

    if (NULL == realmunmap) {
        realmunmap = (int (*)(void*, size_t)) dlsym(RTLD_NEXT, "munmap");
    }

    return realmunmap(start, length);
}
