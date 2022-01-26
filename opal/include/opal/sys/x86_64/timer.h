/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. ALl rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_TIMER_H
#define OPAL_SYS_ARCH_TIMER_H 1

typedef uint64_t opal_timer_t;

/* Using RDTSC(P) results in non-monotonic timers across cores */
#undef OPAL_TIMER_MONOTONIC
#define OPAL_TIMER_MONOTONIC 0

#if OPAL_C_GCC_INLINE_ASSEMBLY

#    if defined(PLATFORM_ARCH_X86_64)

/* TODO: add AMD mfence version and dispatch at init */
static inline opal_timer_t opal_sys_timer_get_cycles(void)
{
    uint32_t l, h;
    __asm__ __volatile__("lfence\n\t"
                         "rdtsc\n\t"
                         : "=a"(l), "=d"(h));
    return ((opal_timer_t) l) | (((opal_timer_t) h) << 32);
}

static inline bool opal_sys_timer_is_monotonic(void)
{
    int64_t tmp;
    int32_t cpuid1, cpuid2;
    const int32_t level = 0x80000007;

    /* cpuid clobbers ebx but it must be restored for -fPIC so save
     * then restore ebx */
    __asm__ volatile("xchg %%rbx, %2\n"
                     "cpuid\n"
                     "xchg %%rbx, %2\n"
                     : "=a"(cpuid1), "=d"(cpuid2), "=r"(tmp)
                     : "a"(level)
                     : "ecx", "ebx");
    /* bit 8 of edx contains the invariant tsc flag */
    return !!(cpuid2 & (1 << 8));
}

#    define OPAL_HAVE_SYS_TIMER_IS_MONOTONIC 1
#    else

static inline opal_timer_t opal_sys_timer_get_cycles(void)
{
    opal_timer_t ret;
    int tmp;

    __asm__ __volatile__("xchgl %%ebx, %1\n"
                         "cpuid\n"
                         "xchgl %%ebx, %1\n"
                         "rdtsc\n"
                         : "=A"(ret), "=r"(tmp)::"ecx");

    return ret;
}

#    endif

#    define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#else

#    define OPAL_HAVE_SYS_TIMER_GET_CYCLES 0

#endif /* OPAL_C_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_TIMER_H */
