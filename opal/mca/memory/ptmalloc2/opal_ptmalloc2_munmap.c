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

#include "ompi_config.h"

#include <sys/types.h>
#include <stdlib.h>
#include <sys/mman.h>
#if defined(HAVE___MUNMAP)
/* here so we only include dlfcn if we absolutely have to */
#elif defined(HAVE_DLSYM)
#ifndef __USE_GNU
#define __USE_GNU
#endif
#include <dlfcn.h>
#elif defined(HAVE_SYSCALL)
#include <syscall.h>
#include <unistd.h>
#endif

#include "opal/memoryhooks/memory_internal.h"

/*
 * munmap is always intercepted
 */
int opal_mem_free_ptmalloc2_munmap(void *start, size_t length);
#if defined(HAVE___MUNMAP)
int  __munmap(void* addr, size_t len);
#endif


/* intercept munmap, as the user can give back memory that way as well. */
int 
munmap(void* addr, size_t len)
{
    return opal_mem_free_ptmalloc2_munmap(addr, len);
}


/* three ways to call munmap.  Prefered is to just call syscall, so
   that we can intercept both munmap and __munmap.  If that isn't
   possible, try calling __munmap from munmap and let __munmap go.  If
   that doesn't work, try dlsym */
int
opal_mem_free_ptmalloc2_munmap(void *start, size_t length)
{
#if !defined(HAVE___MUNMAP) && defined(HAVE_DLSYM)
    static int (*realmunmap)(void*, size_t);
#endif

    opal_mem_hooks_release_hook(start, length);

#if defined(HAVE___MUNMAP)
    return __munmap(start, length);
#elif defined(HAVE_DLSYM)
    if (NULL == realmunmap) {
        union { 
            int (*munmap_fp)(void*, size_t);
            void *munmap_p;
        } tmp;

        tmp.munmap_p = dlsym(RTLD_NEXT, "munmap");
        realmunmap = tmp.munmap_fp;
    }

    return realmunmap(start, length);
#elif defined(HAVE_SYSCALL)
    return syscall(__NR_munmap, start, length);
#else
    #error "Can not determine how to call munmap"
#endif
}


#if defined(HAVE___MMAP) || defined(HAVE_DLSYM)
/*
 * mmap is only intercepted if we have a chance of finding it (ie, a
 * syscall or weak symbol)
 */
void*  opal_mem_free_ptmalloc2_mmap(void *start, size_t length, 
                                    int prot, int flags, 
                                    int fd, off_t offset);

#if defined(HAVE___MMAP)
void* __mmap(void *start, size_t length, int prot, int flags, 
             int fd, off_t offset);
#endif


void* 
mmap(void *start, size_t length, int prot, int flags, 
     int fd, off_t offset)
{
    return opal_mem_free_ptmalloc2_mmap(start, length, prot, flags,
                                        fd, offset);
}
         

void*  opal_mem_free_ptmalloc2_mmap(void *start, size_t length, 
                                    int prot, int flags, 
                                    int fd, off_t offset)
{
#if !defined(HAVE___MMAP) && defined(HAVE_DLSYM)
    static void* (*realmmap)(void *, size_t, int, int, int, off_t);
#endif
    void *tmp;

#if defined(HAVE___MMAP)
    tmp = __mmap(start, length, prot, flags, fd, offset);
#elif defined(HAVE_DLSYM)
    if (NULL == realmmap) {
        union { 
            void* (*mmap_fp)(void *, size_t, int, int, int, off_t);
            void *mmap_p;
        } tmp;

        tmp.mmap_p = dlsym(RTLD_NEXT, "mmap");
        realmmap = tmp.mmap_fp;
    }
    tmp = realmmap(start, length, prot, flags, fd, offset);

#else
    #error "Can not determine how to call mmap"
#endif

    opal_mem_hooks_alloc_hook(tmp, length);

    return tmp;
}

#endif /* defined(HAVE___MMAP) || defined(HAVE_DLSYM) */
