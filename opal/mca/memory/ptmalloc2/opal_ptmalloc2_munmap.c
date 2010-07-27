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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/types.h>
#include <stdlib.h>
#include <sys/mman.h>
#if defined(HAVE___MUNMAP)
/* here so we only include others if we absolutely have to */
#elif defined(HAVE_SYSCALL)
#include <syscall.h>
#include <unistd.h>
#endif
#if defined(HAVE_DLSYM)
#ifndef __USE_GNU
#define __USE_GNU
#endif
#include <dlfcn.h>
#endif

#include "opal/memoryhooks/memory_internal.h"

#include "opal_ptmalloc2_munmap.h"

/*
 * munmap is always intercepted
 */
#if defined(HAVE___MUNMAP)
int  __munmap(void* addr, size_t len);
#endif


/* intercept munmap, as the user can give back memory that way as well. */
OPAL_DECLSPEC int 
munmap(void* addr, size_t len)
{
    return opal_mem_free_ptmalloc2_munmap(addr, len, 0, 1);
}


/* three ways to call munmap.  Prefered is to just call syscall, so
   that we can intercept both munmap and __munmap.  If that isn't
   possible, try calling __munmap from munmap and let __munmap go.  If
   that doesn't work, try dlsym */
int
opal_mem_free_ptmalloc2_munmap(void *start, size_t length, int from_alloc,
                               int call_hooks)
{
    {
      extern bool opal_memory_ptmalloc2_munmap_invoked;
      opal_memory_ptmalloc2_munmap_invoked = true;
    }

    if (call_hooks) {
        opal_mem_hooks_release_hook(start, length, from_alloc);
    }

#if defined(HAVE___MUNMAP)
    return __munmap(start, length);
#elif defined(HAVE_SYSCALL) && defined(__NR_munmap)
    return syscall(__NR_munmap, start, length);
#elif defined(HAVE_DLSYM)
    {
        /* Must use a typedef here because we need volatile to be an
           attribute of the variable, not the function (which would be
           meaningless, anyway). */
        typedef int (*munmap_fn_t)(void*, size_t);
        static volatile munmap_fn_t realmunmap = NULL;

        if (NULL == realmunmap) {
            union { 
                int (*munmap_fp)(void*, size_t);
                void *munmap_p;
            } tmp;
        
            tmp.munmap_p = dlsym(RTLD_NEXT, "munmap");
            realmunmap = tmp.munmap_fp;
        }

        return realmunmap(start, length);
    }
#else
    #error "Can not determine how to call munmap"
#endif
}
