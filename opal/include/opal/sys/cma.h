/*
 * Copyright (c) 2011-2012      IBM Corporation.  All rights reserved.
 *
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

#include "opal_config.h"

#include "opal/sys/architecture.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <sys/unistd.h>
#endif

#ifdef __linux__

/* Cross Memory Attach is so far only supported under linux */

#if OPAL_ASSEMBLY_ARCH == OMPI_AMD64
#define __NR_process_vm_readv 310
#define __NR_process_vm_writev 311
#elif OPAL_ASSEMBLY_ARCH == OMPI_IA32
#define __NR_process_vm_readv 347
#define __NR_process_vm_writev 348
#elif OPAL_ASSEMBLY_ARCH == OMPI_IA64
#define __NR_process_vm_readv 1332
#define __NR_process_vm_writev 1333
#elif OPAL_ASSEMBLY_ARCH == OMPI_POWERPC32
#define __NR_process_vm_readv 351
#define __NR_process_vm_writev 352
#elif OPAL_ASSEMBLY_ARCH == OMPI_POWERPC64
#define __NR_process_vm_readv 351
#define __NR_process_vm_writev 352
#else
#error "Unsupported architecture for process_vm_readv and process_vm_writev syscalls"
#endif


inline ssize_t process_vm_readv(pid_t pid, 
                                const struct iovec  *lvec, 
                                unsigned long liovcnt,
                                const struct iovec *rvec,
                                unsigned long riovcnt,
                                unsigned long flags)
{
  return syscall(__NR_process_vm_readv, pid, lvec, liovcnt, rvec, riovcnt, flags);
}

inline ssize_t process_vm_writev(pid_t pid, 
                          const struct iovec  *lvec, 
                          unsigned long liovcnt,
                          const struct iovec *rvec,
                          unsigned long riovcnt,
                          unsigned long flags)
{
  return syscall(__NR_process_vm_writev, pid, lvec, liovcnt, rvec, riovcnt, flags);
}

#endif /* __linux__ */

#endif /* OPAL_SYS_CMA_H */
