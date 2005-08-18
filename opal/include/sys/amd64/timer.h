/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_SYS_ARCH_TIMER_H
#define OMPI_SYS_ARCH_TIMER_H 1


typedef uint64_t opal_timer_t;


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

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#else

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 0

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* ! OMPI_SYS_ARCH_TIMER_H */
