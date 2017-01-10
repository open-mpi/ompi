/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2016      Los Alamos National Security, LLC. ALl rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_TIMER_H
#define OMPI_SYS_ARCH_TIMER_H 1


typedef uint64_t opal_timer_t;

/* Using RDTSC(P) results in non-monotonic timers across cores */
#undef OPAL_TIMER_MONOTONIC
#define OPAL_TIMER_MONOTONIC 0

#if OMPI_GCC_INLINE_ASSEMBLY

#if 0
static inline opal_timer_t
opal_sys_timer_get_cycles(void)
{
    opal_timer_t ret;

    __asm__ __volatile__("rdtsc" : "=A"(ret));

    return ret;
}

#else

static inline opal_timer_t 
opal_sys_timer_get_cycles(void)
{
     unsigned a, d; 
     __asm__ __volatile__ ("rdtsc" : "=a" (a), "=d" (d));
     return ((opal_timer_t)a) | (((opal_timer_t)d) << 32);
}

#endif

static inline bool opal_sys_timer_is_monotonic (void)
{
    int64_t tmp;
    int32_t cpuid1, cpuid2;
    const int32_t level = 0x80000007;

    /* cpuid clobbers ebx but it must be restored for -fPIC so save
     * then restore ebx */
    __asm__ volatile ("xchg %%rbx, %2\n"
                      "cpuid\n"
                      "xchg %%rbx, %2\n":
                      "=a" (cpuid1), "=d" (cpuid2), "=r" (tmp) :
                      "a" (level) :
                      "ecx", "ebx");
    /* bit 8 of edx contains the invariant tsc flag */
    return !!(cpuid2 & (1 << 8));
}

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1
#define OPAL_HAVE_SYS_TIMER_IS_MONOTONIC 1

#else

opal_timer_t opal_sys_timer_get_cycles(void);

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* ! OMPI_SYS_ARCH_TIMER_H */
