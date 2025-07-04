/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Software System Team, SANECHIPS.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_TIMER_H
#define OPAL_SYS_ARCH_TIMER_H 1

typedef uint64_t opal_timer_t;

#if OPAL_C_GCC_INLINE_ASSEMBLY

static inline opal_timer_t opal_sys_timer_get_cycles(void)
{
    opal_timer_t ret;
    __asm__ __volatile__("fence.i");
    __asm__ __volatile__("fence r, r" ::: "memory");
    __asm__ __volatile__("rdtime %0" : "=r"(ret));

    return ret;
}

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#endif /* OPAL_C_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_TIMER_H */

