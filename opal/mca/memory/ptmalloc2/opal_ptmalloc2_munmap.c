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
#if defined(HAVE_SYSCALL)
#include <syscall.h>
#include <unistd.h>
#elif defined(HAVE___MUNMAP)
/* here only so that we only include dlfcn.h if needed */
#elif defined(HAVE_DLSYM)
#define __USE_GNU
#include <dlfcn.h>
#endif

#include "opal/memoryhooks/memory_internal.h"

/*
 * munmap is always intercepted
 */
int opal_mem_free_ptmalloc2_munmap(void *start, size_t length);
#if defined(HAVE_SYSCALL) || defined(HAVE___MUNMAP)
int  __munmap(void* addr, size_t len);
#endif


/* intercept munmap, as the user can give back memory that way as well. */
int 
munmap(void* addr, size_t len)
{
    return opal_mem_free_ptmalloc2_munmap(addr, len);
}


/* with syscall, we can safely intercept this as well */
#if defined(HAVE_SYSCALL)
int 
__munmap(void* addr, size_t len)
{
    return opal_mem_free_ptmalloc2_munmap(addr, len);
}
#endif
         

/* three ways to call munmap.  Prefered is to just call syscall, so
   that we can intercept both munmap and __munmap.  If that isn't
   possible, try calling __munmap from munmap and let __munmap go.  If
   that doesn't work, try dlsym */
int
opal_mem_free_ptmalloc2_munmap(void *start, size_t length)
{
#ifdef HAVE_DLSYM
    static int (*realmunmap)(void*, size_t);
#endif

    opal_mem_hooks_release_hook(start, length);

#if defined(HAVE_SYSCALL)
    return syscall(__NR_munmap, start, length);
#elif defined(HAVE___MUNMAP)
    return __munmap(start, length);
#elif defined(HAVE_DLSYM)
    if (NULL == realmunmap) {
        realmunmap = (int (*)(void*, size_t)) dlsym(RTLD_NEXT, "munmap");
    }

    return realmunmap(start, length);
#else
    #error "Can not determine how to call munmap"
#endif
}


#if defined(HAVE_SYSCALL) || defined(HAVE___MMAP)
/*
 * mmap is only intercepted if we have a chance of finding it (ie, a
 * syscall or weak symbol)
 */
void*  opal_mem_free_ptmalloc2_mmap(void *start, size_t length, 
                                    int prot, int flags, 
                                    int fd, off_t offset);

void* __mmap(void *start, size_t length, int prot, int flags, 
             int fd, off_t offset);


void* 
mmap(void *start, size_t length, int prot, int flags, 
     int fd, off_t offset)
{
    return opal_mem_free_ptmalloc2_mmap(start, length, prot, flags,
                                        fd, offset);
}

#if defined(HAVE_SYSCALL)
/* with syscall, we can safely intercept this as well */
void* 
__mmap(void *start, size_t length, int prot, int flags, 
       int fd, off_t offset)
{
    return opal_mem_free_ptmalloc2_mmap(start, length, prot, flags,
                                        fd, offset);
}
#endif
         

/* three ways to call mmap.  Prefered is to just call syscall, so that
   we can intercept both munmap and __munmap.  If that isn't possible,
   try calling __mmap from mmap and let __mmap go.  Do not try to
   dlsym, as that generally requires calling mmap.
*/
void*  opal_mem_free_ptmalloc2_mmap(void *start, size_t length, 
                                    int prot, int flags, 
                                    int fd, off_t offset)
{
    opal_mem_hooks_alloc_hook(start, length);

#if defined(HAVE_SYSCALL)
    return (void*) syscall(__NR_mmap, start, length, prot, flags, fd, offset);
#elif defined(HAVE___MUNMAP)
    return __mmap(start, length, prot, flags, fd, offset);
#else
    #error "Can not determine how to call mmap"
#endif
}

#endif /* #if defined(HAVE_SYSCALL) || defined(HAVE___MMAP) */
