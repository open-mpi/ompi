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
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SYS_ARCH_ATOMIC_H
#define PMIX_SYS_ARCH_ATOMIC_H 1


/* BWB - FIX ME! */
#ifdef __linux__
#define PMIXMB() __asm__ __volatile__(".set mips2; sync; .set mips0": : :"memory")
#define PMIXRMB() __asm__ __volatile__(".set mips2; sync; .set mips0": : :"memory")
#define PMIXWMB() __asm__ __volatile__(".set mips2; sync; .set mips0": : :"memory")
#define PMIXSMP_SYNC ".set mips2; sync; .set mips0"
#else
#define PMIXMB() __asm__ __volatile__("sync": : :"memory")
#define PMIXRMB() __asm__ __volatile__("sync": : :"memory")
#define PMIXWMB() __asm__ __volatile__("sync": : :"memory")
#define PMIXSMP_SYNC "sync"
#endif


/**********************************************************************
 *
 * Define constants for MIPS
 *
 *********************************************************************/
#define PMIX_HAVE_ATOMIC_MEM_BARRIER 1

#define PMIX_HAVE_ATOMIC_CMPSET_32 1

#ifdef __mips64
#define PMIX_HAVE_ATOMIC_CMPSET_64 1
#endif

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if PMIX_GCC_INLINE_ASSEMBLY

static inline
void pmix_atomic_mb(void)
{
    PMIXMB();
}


static inline
void pmix_atomic_rmb(void)
{
    PMIXRMB();
}


static inline
void pmix_atomic_wmb(void)
{
    PMIXWMB();
}

static inline
void pmix_atomic_isync(void)
{
}

#endif

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if PMIX_GCC_INLINE_ASSEMBLY

static inline int pmix_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
    int32_t ret;

   __asm__ __volatile__ (".set noreorder        \n"
                         ".set noat             \n"
                         "1:                    \n"
#ifdef __linux__
                         ".set mips2         \n\t"
#endif
                         "ll     %0, %2         \n" /* load *addr into ret */
                         "bne    %0, %z3, 2f    \n" /* done if oldval != ret */
                         "or     $1, %z4, 0     \n" /* tmp = newval (delay slot) */
                         "sc     $1, %2         \n" /* store tmp in *addr */
#ifdef __linux__
                         ".set mips0         \n\t"
#endif
                         /* note: ret will be 0 if failed, 1 if succeeded */
                         "beqz   $1, 1b         \n" /* if 0 jump back to 1b */
                         "nop                   \n" /* fill delay slots */
                         "2:                    \n"
                         ".set reorder          \n"
                         : "=&r"(ret), "=m"(*addr)
                         : "m"(*addr), "r"(oldval), "r"(newval)
                         : "cc", "memory");
   return (ret == oldval);
}


/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_32 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int pmix_atomic_cmpset_acq_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    int rc;

    rc = pmix_atomic_cmpset_32(addr, oldval, newval);
    pmix_atomic_rmb();

    return rc;
}


static inline int pmix_atomic_cmpset_rel_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    pmix_atomic_wmb();
    return pmix_atomic_cmpset_32(addr, oldval, newval);
}

#ifdef PMIX_HAVE_ATOMIC_CMPSET_64
static inline int pmix_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
    int64_t ret;

   __asm__ __volatile__ (".set noreorder        \n"
                         ".set noat             \n"
                         "1:                    \n\t"
                         "lld    %0, %2         \n\t" /* load *addr into ret */
                         "bne    %0, %z3, 2f    \n\t" /* done if oldval != ret */
                         "or     $1, %4, 0      \n\t" /* tmp = newval (delay slot) */
                         "scd    $1, %2         \n\t" /* store tmp in *addr */
                         /* note: ret will be 0 if failed, 1 if succeeded */
                         "beqz   $1, 1b         \n\t" /* if 0 jump back to 1b */
                         "nop                   \n\t" /* fill delay slot */
                         "2:                    \n\t"
                         ".set reorder          \n"
                         : "=&r" (ret), "=m" (*addr)
                         : "m" (*addr), "r" (oldval), "r" (newval)
                         : "cc", "memory");

   return (ret == oldval);
}


/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_64 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int pmix_atomic_cmpset_acq_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    int rc;

    rc = pmix_atomic_cmpset_64(addr, oldval, newval);
    pmix_atomic_rmb();

    return rc;
}


static inline int pmix_atomic_cmpset_rel_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    pmix_atomic_wmb();
    return pmix_atomic_cmpset_64(addr, oldval, newval);
}
#endif /* PMIX_HAVE_ATOMIC_CMPSET_64 */

#endif /* PMIX_GCC_INLINE_ASSEMBLY */

#endif /* ! PMIX_SYS_ARCH_ATOMIC_H */
