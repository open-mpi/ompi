/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2008      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2016      Broadcom Limited. All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_TIMER_H
#define OPAL_SYS_ARCH_TIMER_H 1

#if OPAL_ASSEMBLY_ARCH == OPAL_ARM64
typedef uint64_t opal_timer_t;
#else
typedef uint32_t opal_timer_t;
#endif

static inline opal_timer_t opal_sys_timer_get_cycles(void)
{
    opal_timer_t ret;

    __asm__ __volatile__("isb" ::: "memory");
#if OPAL_ASSEMBLY_ARCH == OPAL_ARM64
    __asm__ __volatile__("mrs %0,  CNTVCT_EL0" : "=r"(ret));
#else
    __asm__ __volatile__("mrs %0,  CNTVCT" : "=r"(ret));
#endif

    return ret;
}

static inline opal_timer_t opal_sys_timer_get_freq(void)
{
    opal_timer_t freq;
#if OPAL_ASSEMBLY_ARCH == OPAL_ARM64
    __asm__ __volatile__("mrs %0,  CNTFRQ_EL0" : "=r"(freq));
#else
    __asm__ __volatile__("mrs %0,  CNTFRQ" : "=r"(freq));
#endif
    return (opal_timer_t)(freq);
}

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1
#define OPAL_HAVE_SYS_TIMER_GET_FREQ   1

#endif /* ! OPAL_SYS_ARCH_TIMER_H */
