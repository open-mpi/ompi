/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * ******** Add IBM COPYRIGHT HERE ***********
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "memory_patcher.h"

#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/memory/base/empty.h"
#include "opal/memoryhooks/memory.h"

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/syscall.h>

#include "memory_patcher.h"
#undef opal_memory_changed

static int patcher_open(void);
static int patcher_close(void);
static int patcher_register(void);
static int patcher_query (int *);

static int mca_memory_patcher_priority;

opal_memory_patcher_component_t mca_memory_patcher_component = {
    .super = {
        .memoryc_version = {
            OPAL_MEMORY_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "patcher",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = patcher_open,
            .mca_close_component = patcher_close,
            .mca_register_component_params = patcher_register,
        },
        .memoryc_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Memory framework functions. */
        .memoryc_query = patcher_query,
        .memoryc_register = opal_memory_base_component_register_empty,
        .memoryc_deregister = opal_memory_base_component_deregister_empty,
        .memoryc_set_alignment = opal_memory_base_component_set_alignment_empty,
    },

    /* Component-specific data, filled in later (compiler will 0/NULL
       it out) */
};

#if OPAL_MEMORY_PATCHER_HAVE___MMAP && !OPAL_MEMORY_PATCHER_HAVE___MMAP_PROTO
/* prototype for Apple's internal mmap function */
void *__mmap (void *start, size_t length, int prot, int flags, int fd, off_t offset);
#endif

#if OPAL_MEMORY_PATCHER_HAVE___SYSCALL_PROTO && OPAL_MEMORY_PATCHER_HAVE___SYSCALL
/* calling __syscall is preferred on some systems when some arguments may be 64-bit. it also
 * has the benefit of having an off_t return type */
#define memory_patcher_syscall __syscall
#else
#define memory_patcher_syscall syscall
#endif

static void *intercept_mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset)
{
    OPAL_PATCHER_BEGIN;
    void *result = 0;

    if (prot == PROT_NONE) {
        opal_mem_hooks_release_hook (start, length, 0);
    }

#if OPAL_MEMORY_PATCHER_HAVE___MMAP
    /* the darwin syscall returns an int not a long so call the underlying __mmap function */
    result = __mmap (start, length, prot, flags, fd, offset);
#else
    result = (void*)(intptr_t) memory_patcher_syscall(SYS_mmap, start, length, prot, flags, fd, offset);
#endif

// I thought we had some issue in the past with the above line for IA32,
// like maybe syscall() wouldn't handle that many arguments. But just now
// I used gcc -m32 and it worked on a recent system. But there's a possibility
// that older ia32 systems may need some other code to make the above syscall.

    OPAL_PATCHER_END;
    return result;
}

static int intercept_munmap(void *start, size_t length)
{
    OPAL_PATCHER_BEGIN;
    int result = 0;

    opal_mem_hooks_release_hook (start, length, 0);

    result=memory_patcher_syscall(SYS_munmap, start, length);

    OPAL_PATCHER_END;
    return result;
}

#if defined (SYS_mremap)

static void *intercept_mremap (void *start, size_t oldlen, size_t newlen, int flags, ...)
{
    OPAL_PATCHER_BEGIN;
    void *result = 0;
#ifdef MREMAP_FIXED
    va_list ap;
    void *new_address;
#endif

    opal_mem_hooks_release_hook (start, oldlen, 0);

#ifdef MREMAP_FIXED
    if (flags & MREMAP_FIXED) {
        va_start(ap, flags);
        new_address = va_arg(ap, void*);
        result=(void *)(intptr_t) memory_patcher_syscall(
            SYS_mremap, start, oldlen, newlen, flags, new_address);
        va_end(ap);
    } else {
        result=(void*)memory_patcher_syscall(
            SYS_mremap, start, oldlen, newlen, flags);
    }
#else
    result=(void*)(intptr_t) memory_patcher_syscall(SYS_mremap, start, oldlen, newlen, flags);
#endif

    OPAL_PATCHER_END;
    return result;
}

#endif

static int intercept_madvise (void *start, size_t length, int advice)
{
    OPAL_PATCHER_BEGIN;
    int result = 0;

    if (advice == MADV_DONTNEED ||
#ifdef MADV_REMOVE
        advice == MADV_REMOVE ||
#endif
        advice == POSIX_MADV_DONTNEED)
    {
        opal_mem_hooks_release_hook (start, length, 0);
    }
    result = memory_patcher_syscall(SYS_madvise, start, length, advice);

    OPAL_PATCHER_END;
    return result;
}

#if defined SYS_brk

#if OPAL_MEMORY_PATCHER_HAVE___CURBRK
void *__curbrk; /* in libc */
#endif

static int intercept_brk (void *addr)
{
    OPAL_PATCHER_BEGIN;
    int result = 0;
    void *old_addr, *new_addr;

#if OPAL_MEMORY_PATCHER_HAVE___CURBRK
    old_addr = __curbrk;
#else
    old_addr = sbrk (0);
#endif

    /* get the current_addr */
    new_addr = (void *) (intptr_t) memory_patcher_syscall(SYS_brk, addr);

#if OPAL_MEMORY_PATCHER_HAVE___CURBRK
    /*
     * Note: if we were using glibc brk/sbrk, their __curbrk would get
     * updated, but since we're going straight to the syscall, we have
     * to update __curbrk or else glibc won't see it.
     */
    __curbrk = new_addr;
#endif

    if (new_addr < addr) {
        errno = ENOMEM;
        result = -1;
    } else if (new_addr < old_addr) {
        opal_mem_hooks_release_hook (new_addr, (intptr_t) old_addr - (intptr_t) new_addr, 0);
    }
    OPAL_PATCHER_END;
    return result;
}

#endif

static int patcher_register (void)
{
    mca_memory_patcher_priority = 80;
    mca_base_component_var_register (&mca_memory_patcher_component.super.memoryc_version,
                                     "priority", "Priority of the patcher memory hook component",
                                     MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                     MCA_BASE_VAR_SCOPE_CONSTANT, &mca_memory_patcher_priority);

    return OPAL_SUCCESS;
}

static int patcher_query (int *priority)
{
    if (opal_patch_supported ()) {
        *priority = mca_memory_patcher_priority;
    } else {
        *priority = -1;
    }
    return OPAL_SUCCESS;
}

static int patcher_open (void)
{
    static int was_executed_already = 0;
    int rc;

    if (was_executed_already) {
        return OPAL_SUCCESS;
    }

    was_executed_already = 1;

    /* set memory hooks support level */
    opal_mem_hooks_set_support (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT);

    rc = opal_patch_symbol ("mmap", (uintptr_t) intercept_mmap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    rc = opal_patch_symbol ("munmap", (uintptr_t)intercept_munmap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

#if defined (SYS_mremap)
    rc = opal_patch_symbol ("mremap",(uintptr_t)intercept_mremap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
#endif

    rc = opal_patch_symbol ("madvise", (uintptr_t)intercept_madvise);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

#if defined (SYS_brk)
    rc = opal_patch_symbol ("brk", (uintptr_t)intercept_brk);
#endif

    return rc;
}

static int patcher_close(void)
{
    /* NTH: is it possible to unpatch the symbols? */
    return OPAL_SUCCESS;
}
