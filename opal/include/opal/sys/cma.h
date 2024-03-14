/*
 * Copyright (c) 2011-2012 IBM Corporation.  All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 */

/** @file
 *
 * Cross Memory Attach syscall definitions.
 *
 * These are only needed temporarily until these new syscalls
 * are incorporated into glibc
 */

#ifndef OPAL_SYS_CMA_H
#define OPAL_SYS_CMA_H 1

#include "opal/opal_portable_platform.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#    include <sys/unistd.h>
#endif

#ifdef PLATFORM_OS_LINUX

/* Cross Memory Attach is so far only supported under linux */

#    if defined(PLATFORM_ARCH_x86_64)
#        define __NR_process_vm_readv  310
#        define __NR_process_vm_writev 311
#    elif defined(PLATFORM_ARCH_X86)
#        define __NR_process_vm_readv  347
#        define __NR_process_vm_writev 348
#    elif defined(PLATFORM_ARCH_POWERPC) && defined(PLATFORM_ARCH_32)
#        define __NR_process_vm_readv  351
#        define __NR_process_vm_writev 352
#    elif defined(PLATFORM_ARCH_POWERPC) && defined(PLATFORM_ARCH_64)
#        define __NR_process_vm_readv  351
#        define __NR_process_vm_writev 352
#    elif defined(PLATFORM_ARCH_ARM)

#        define __NR_process_vm_readv  376
#        define __NR_process_vm_writev 377

#    elif defined(PLATFORM_ARCH_AARCH64)

/* ARM64 uses the asm-generic syscall numbers */

#        define __NR_process_vm_readv  270
#        define __NR_process_vm_writev 271

#    else
#        error "Unsupported architecture for process_vm_readv and process_vm_writev syscalls"
#    endif

static inline ssize_t process_vm_readv(pid_t pid, const struct iovec *lvec, unsigned long liovcnt,
                                       const struct iovec *rvec, unsigned long riovcnt,
                                       unsigned long flags)
{
    return syscall(__NR_process_vm_readv, pid, lvec, liovcnt, rvec, riovcnt, flags);
}

static inline ssize_t process_vm_writev(pid_t pid, const struct iovec *lvec, unsigned long liovcnt,
                                        const struct iovec *rvec, unsigned long riovcnt,
                                        unsigned long flags)
{
    return syscall(__NR_process_vm_writev, pid, lvec, liovcnt, rvec, riovcnt, flags);
}

#endif /* __linux__ */

#endif /* OPAL_SYS_CMA_H */
