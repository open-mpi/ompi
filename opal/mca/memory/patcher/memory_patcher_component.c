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
 * Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
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
#include "opal/mca/memory/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/patcher/base/base.h"

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <assert.h>
#include <sys/time.h>
#if defined(HAVE_SYS_SYSCALL_H)
#include <sys/syscall.h>
#endif
#if defined(HAVE_LINUX_MMAN_H)
#include <linux/mman.h>
#endif

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

#if HAVE_DECL___SYSCALL && defined(HAVE___SYSCALL)
/* calling __syscall is preferred on some systems when some arguments may be 64-bit. it also
 * has the benefit of having an off_t return type */
#define memory_patcher_syscall __syscall
#else
#define memory_patcher_syscall syscall
#endif

/* All the hooks in this file have two levels. The first level has the OPAL_PATCHER_* macros
 * around the call to the second level. This was done because with xlc the compiler was
 * generating an access to r2 before the OPAL_PATCHER_* assembly. This was loading invalid
 * data. If this can be resolved the two levels can be joined.
 */

/*
 * The following block of code is #if 0'ed out because we do not need
 * to intercept mmap() any more (mmap() only deals with memory
 * protection; it does not invalidate any rcache entries for a given
 * region).  But if we do someday, this is the code that we'll need.
 * It's a little non-trivial, so we might as well keep it (and #if 0
 * it out).
 */
#if 0

#if defined(HAVE___MMAP) && !HAVE_DECL___MMAP
/* prototype for Apple's internal mmap function */
void *__mmap (void *start, size_t length, int prot, int flags, int fd, off_t offset);
#endif

static void *(*original_mmap)(void *, size_t, int, int, int, off_t);

static void *intercept_mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset)
{
    OPAL_PATCHER_BEGIN;
    void *result = 0;

    if (prot == PROT_NONE) {
        opal_mem_hooks_release_hook (start, length, true);
    }

    if (!original_mmap) {
#ifdef HAVE___MMAP
        /* the darwin syscall returns an int not a long so call the underlying __mmap function */
        result = __mmap (start, length, prot, flags, fd, offset);
#else
        result = (void*)(intptr_t) memory_patcher_syscall(SYS_mmap, start, length, prot, flags, fd, offset);
#endif

        // I thought we had some issue in the past with the above line for IA32,
        // like maybe syscall() wouldn't handle that many arguments. But just now
        // I used gcc -m32 and it worked on a recent system. But there's a possibility
        // that older ia32 systems may need some other code to make the above syscall.
    } else {
        result = original_mmap (start, length, prot, flags, fd, offset);
    }

    OPAL_PATCHER_END;
    return result;
}

#endif

static int (*original_munmap) (void *, size_t);

static int _intercept_munmap(void *start, size_t length)
{
    int result = 0;

    /* could be in a malloc implementation */
    opal_mem_hooks_release_hook (start, length, true);

    if (!original_munmap) {
        result = memory_patcher_syscall(SYS_munmap, start, length);
    } else {
        result = original_munmap (start, length);
    }

    return result;
}

static int intercept_munmap(void *start, size_t length)
{
    OPAL_PATCHER_BEGIN;
    int result = _intercept_munmap (start, length);
    OPAL_PATCHER_END;
    return result;
}

#if defined (SYS_mremap)

#if defined(__linux__)
/* on linux this function has an optional extra argument but ... can not be used here because it
 * causes issues when intercepting a 4-argument mremap call */
static void *(*original_mremap) (void *, size_t, size_t, int, void *);
#else
/* mremap has a different signature on BSD systems */
static void *(*original_mremap) (void *, size_t, void *, size_t, int);
#endif

#if defined(__linux__)
static void *_intercept_mremap (void *start, size_t oldlen, size_t newlen, int flags, void *new_address)
#else
static void *_intercept_mremap (void *start, size_t oldlen, void *new_address, size_t newlen, int flags)
#endif
{
    void *result = MAP_FAILED;

    if (MAP_FAILED != start && oldlen > 0) {
        opal_mem_hooks_release_hook (start, oldlen, true);
    }

#if defined(MREMAP_FIXED)
    if (!(flags & MREMAP_FIXED)) {
        new_address = NULL;
    }
#endif

#if defined(__linux__)
    if (!original_mremap) {
        result = (void *)(intptr_t) memory_patcher_syscall (SYS_mremap, start, oldlen, newlen, flags, new_address);
    } else {
        result = original_mremap (start, oldlen, newlen, flags, new_address);
    }
#else
    if (!original_mremap) {
        result = (void *)(intptr_t) memory_patcher_syscall (SYS_mremap, start, oldlen, new_address, newlen, flags);
    } else {
        result = original_mremap (start, oldlen, new_address, newlen, flags);
    }
#endif

    return result;
}

#if defined(__linux__)
static void *intercept_mremap (void *start, size_t oldlen, size_t newlen, int flags, void *new_address)
{
    OPAL_PATCHER_BEGIN;
    void *result = _intercept_mremap (start, oldlen, newlen, flags, new_address);
    OPAL_PATCHER_END;
    return result;
}
#else
static void *intercept_mremap (void *start, size_t oldlen, void *new_address, size_t newlen, int flags)
{
    OPAL_PATCHER_BEGIN;
    void *result = _intercept_mremap (start, oldlen, new_address, newlen, flags);
    OPAL_PATCHER_END;
    return result;
}
#endif

#endif

#if defined (SYS_madvise)

static int (*original_madvise) (void *, size_t, int);

static int _intercept_madvise (void *start, size_t length, int advice)
{
    int result = 0;

    if (advice == MADV_DONTNEED ||
#ifdef MADV_REMOVE
        advice == MADV_REMOVE ||
#endif
        advice == POSIX_MADV_DONTNEED)
    {
        opal_mem_hooks_release_hook (start, length, false);
    }

    if (!original_madvise) {
        result = memory_patcher_syscall(SYS_madvise, start, length, advice);
    } else {
        result = original_madvise (start, length, advice);
    }

    return result;
}
static int intercept_madvise (void *start, size_t length, int advice)
{
    OPAL_PATCHER_BEGIN;
    int result = _intercept_madvise (start, length, advice);
    OPAL_PATCHER_END;
    return result;
}

#endif

#if defined SYS_brk

#ifdef HAVE___CURBRK
extern void *__curbrk; /* in libc */
#endif

static int (*original_brk) (void *);

static int _intercept_brk (void *addr)
{
    int result = 0;
    void *old_addr, *new_addr;

#ifdef HAVE___CURBRK
    old_addr = __curbrk;
#else
    old_addr = sbrk (0);
#endif

    if (!original_brk) {
        /* get the current_addr */
        new_addr = (void *) (intptr_t) memory_patcher_syscall(SYS_brk, addr);

#ifdef HAVE___CURBRK
        /*
         * Note: if we were using glibc brk/sbrk, their __curbrk would get
         * updated, but since we're going straight to the syscall, we have
         * to update __curbrk or else glibc won't see it.
         */
        __curbrk = new_addr;
#endif
    } else {
        result = original_brk (addr);
#ifdef HAVE___CURBRK
        new_addr = __curbrk;
#else
        new_addr = sbrk (0);
#endif
    }

    if (new_addr < addr) {
        errno = ENOMEM;
        result = -1;
    } else if (new_addr < old_addr) {
        opal_mem_hooks_release_hook (new_addr, (intptr_t) old_addr - (intptr_t) new_addr, true);
    }
    return result;
}

static int intercept_brk (void *addr)
{
    OPAL_PATCHER_BEGIN;
    int result = _intercept_brk (addr);
    OPAL_PATCHER_END;
    return result;
}

#endif

#if defined(SYS_shmdt) && defined(__linux__)

#include <stdio.h>
#include <fcntl.h>
#include <sys/shm.h>

static size_t memory_patcher_get_shm_seg_size (const void *shmaddr)
{
    unsigned long start_addr, end_addr;
    char *ptr, *newline;
    char buffer[1024];
    size_t seg_size = 0;
    int fd;

    seg_size = 0;

    fd = open ("/proc/self/maps", O_RDONLY);
    if (fd < 0) {
        return 0;
    }

    for (size_t read_offset = 0 ; ; ) {
        ssize_t nread = read(fd, buffer + read_offset, sizeof(buffer) - 1 - read_offset);
        if (nread <= 0) {
            if (errno == EINTR) {
                continue;
            }

            break;
        } else {
            buffer[nread + read_offset] = '\0';
        }

        ptr = buffer;
        while ( (newline = strchr(ptr, '\n')) != NULL ) {
            /* 00400000-0040b000 r-xp ... \n */
            int ret = sscanf(ptr, "%lx-%lx ", &start_addr, &end_addr);
            if (ret != 2) {
                continue;
            }

            if (start_addr == (uintptr_t)shmaddr) {
                seg_size = end_addr - start_addr;
                goto out_close;
            }

            newline = strchr(ptr, '\n');
            if (newline == NULL) {
                break;
            }

            ptr = newline + 1;
        }

        read_offset = strlen(ptr);
        memmove(buffer, ptr, read_offset);
    }

 out_close:
    close(fd);
    return seg_size;
}

static int (*original_shmdt) (const void *);

static int _intercept_shmdt (const void *shmaddr)
{
    int result;

    /* opal_mem_hooks_release_hook should probably be updated to take a const void *.
     * for now just cast away the const */
    opal_mem_hooks_release_hook ((void *) shmaddr, memory_patcher_get_shm_seg_size (shmaddr), false);

    if (original_shmdt) {
        result = original_shmdt (shmaddr);
    } else {
        result = memory_patcher_syscall (SYS_shmdt, shmaddr);
    }

    return result;
}

static int intercept_shmdt (const void *shmaddr)
{
    OPAL_PATCHER_BEGIN;
    int result = _intercept_shmdt (shmaddr);
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
    int rc;

    rc = mca_base_framework_open (&opal_patcher_base_framework, 0);
    if (OPAL_SUCCESS != rc) {
        *priority = -1;
        return OPAL_SUCCESS;
    }

    *priority = mca_memory_patcher_priority;

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

    rc = opal_patcher_base_select ();
    if (OPAL_SUCCESS != rc) {
        mca_base_framework_close (&opal_patcher_base_framework);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    /* set memory hooks support level */
    opal_mem_hooks_set_support (OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT);

#if 0
    /* See above block to see why mmap() functionality is #if 0'ed
       out */
    rc = opal_patcher->patch_symbol ("mmap", (uintptr_t) intercept_mmap, (uintptr_t *) &original_mmap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
#endif

    rc = opal_patcher->patch_symbol ("munmap", (uintptr_t)intercept_munmap, (uintptr_t *) &original_munmap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

#if defined (SYS_mremap)
    rc = opal_patcher->patch_symbol ("mremap",(uintptr_t)intercept_mremap, (uintptr_t *) &original_mremap);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
#endif

#if defined (SYS_madvise)
    rc = opal_patcher->patch_symbol ("madvise", (uintptr_t)intercept_madvise, (uintptr_t *) &original_madvise);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
#endif

#if defined(SYS_shmdt) && defined(__linux__)
    rc = opal_patcher->patch_symbol ("shmdt", (uintptr_t) intercept_shmdt, (uintptr_t *) &original_shmdt);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
#endif

#if defined (SYS_brk)
    rc = opal_patcher->patch_symbol ("brk", (uintptr_t)intercept_brk, (uintptr_t *) &original_brk);
#endif

    return rc;
}

static int patcher_close(void)
{
    mca_base_framework_close (&opal_patcher_base_framework);

    /* Note that we don't need to unpatch any symbols here; the
       patcher framework will take care of all of that for us. */
    return OPAL_SUCCESS;
}
