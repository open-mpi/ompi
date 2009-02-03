/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory_internal.h"
#include "opal/constants.h"

#if defined(HAVE___MUNMAP)
int  __munmap(void* addr, size_t len);
#endif

static int opal_memory_malloc_open(void);

const opal_memory_base_component_2_0_0_t mca_memory_mallopt_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_MEMORY_BASE_VERSION_2_0_0,

        /* Component name and version */
        "mallopt",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        opal_memory_malloc_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};


static int
opal_memory_malloc_open(void)
{
    /* This component is a bit weird, in that it exists only to
       capture munmap.  Real work happens in mpool_base */
    opal_mem_hooks_set_support(OPAL_MEMORY_MUNMAP_SUPPORT);
    return OPAL_SUCCESS;
}


/* three ways to call munmap.  Prefered is to call __munmap, which
   will exist if munmap is a weak symbol.  If that doesn't work, try
   the syscal, and if that doesn't work, try looking in the dynamic
   libc. */
int 
munmap(void* addr, size_t len)
{

#if !defined(HAVE___MUNMAP) && \
    !(defined(HAVE_SYSCALL) && defined(__NR_munmap)) && defined(HAVE_DLSYM)
    static int (*realmunmap)(void*, size_t);
#endif

    opal_mem_hooks_release_hook(addr, len, 0);

#if defined(HAVE___MUNMAP)
    return __munmap(addr, len);
#elif defined(HAVE_SYSCALL) && defined(__NR_munmap)
    return syscall(__NR_munmap, addr, len);
#elif defined(HAVE_DLSYM) && defined(HAVE_RTLD_NEXT)
    if (NULL == realmunmap) {
        union { 
            int (*munmap_fp)(void*, size_t);
            void *munmap_p;
        } tmp;

        tmp.munmap_p = dlsym(RTLD_NEXT, "munmap");
        realmunmap = tmp.munmap_fp;
    }

    return realmunmap(addr, len);
#else
    #error "Can not determine how to call munmap"
#endif
}
