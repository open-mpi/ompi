/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2016      HfT Stuttgart, University of Applied Science.
 *                         All rights reserved.
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

/* Using RDTSC(P) results in non-monotonic timers across cores */
#undef OPAL_TIMER_MONOTONIC
#define OPAL_TIMER_MONOTONIC 0

#if OPAL_GCC_INLINE_ASSEMBLY

/**
 * http://www.intel.com/content/www/us/en/intelligent-systems/embedded-systems-training/ia-32-ia-64-benchmark-code-execution-paper.html
 * The above document has two mistakes:
 * - Above author claims cpuid uses varying number of cycles -- sure the input
 *   is not fixed (eax from rdtsc); author recommends calling rdtsc 3 times...
 *   Fix by calling with leaf eax=0.
 * - multi-line asm doesn't work with gcc's (good) work of register-scheduling,
 *   leaving instructions like "mov %eax, %eax" in the stream...
 *   GAS does not eliminate.
 */
static inline opal_timer_t
opal_sys_timer_get_cycles(void)
{
     unsigned l, h;
#if !OPAL_ASSEMBLY_SUPPORTS_RDTSCP
     __asm__ __volatile__ ("cpuid\n\t"
                           "rdtsc\n\t"
                           : "=a" (l), "=d" (h) // Output
                           : "a" (0)            // Input
                           : "rbx", "rcx");     // Clobber
#else
     /* If we need higher accuracy we should implement the algorithm proposed
      * on the Intel document referenced above. However, in the context of MPI
      * this function will be used as the backend for MPI_Wtime and as such
      * can afford a small inaccuracy.
      */
     __asm__ __volatile__ ("rdtscp\n\t"
                           : "=a" (l), "=d" (h) // Output
                           :                    // Input
                           : "rcx");            // Clobber
     __asm__ __volatile__ ("cpuid\n\t"
                           :                    // Output
                           : "a" (0)            // Input
                           : "rbx", "rcx", "rdx");

#endif
     return ((opal_timer_t)l) | (((opal_timer_t)h) << 32);
}

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#else

opal_timer_t opal_sys_timer_get_cycles(void);

#define OPAL_HAVE_SYS_TIMER_GET_CYCLES 1

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_TIMER_H */
