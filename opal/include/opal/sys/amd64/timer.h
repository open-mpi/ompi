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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_TIMER_H
#define OPAL_SYS_ARCH_TIMER_H 1


typedef uint64_t opal_timer_t;


#if OPAL_GCC_INLINE_ASSEMBLY

/**
 * http://www.intel.com/content/www/us/en/intelligent-systems/embedded-systems-training/ia-32-ia-64-benchmark-code-execution-paper.html
 */
static inline opal_timer_t
opal_sys_timer_get_cycles(void)
{
     unsigned a, d;
#if 0
     __asm__ __volatile__ ("cpuid\n\t"
                           "rdtsc\n\t"
                           : "=a" (a), "=d" (d)
                           :: "%rax", "%rbx", "%rcx", "%rdx");
#else
     /* If we need higher accuracy we should implement the algorithm proposed
      * on the Intel document referenced above. However, in the context of MPI
      * this function will be used as the backend for MPI_Wtime and as such
      * can afford a small inaccuracy.
      */
     __asm__ __volatile__ ("rdtscp\n\t"
                           "cpuid"
                           : "=a" (a), "=d" (d)
                           :: "%rax", "%rbx", "%rcx", "%rdx");
#endif
     return ((opal_timer_t)a) | (((opal_timer_t)d) << 32);
}

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#else

opal_timer_t opal_sys_timer_get_cycles(void);

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_TIMER_H */
