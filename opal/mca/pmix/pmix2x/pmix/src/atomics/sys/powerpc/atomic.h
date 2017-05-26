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
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SYS_ARCH_ATOMIC_H
#define PMIX_SYS_ARCH_ATOMIC_H 1

/*
 * On powerpc ...
 */

#define PMIXMB()  __asm__ __volatile__ ("sync" : : : "memory")
#define PMIXRMB() __asm__ __volatile__ ("lwsync" : : : "memory")
#define PMIXWMB() __asm__ __volatile__ ("eieio" : : : "memory")
#define PMIXISYNC() __asm__ __volatile__ ("isync" : : : "memory")
#define PMIXSMP_SYNC  "sync \n\t"
#define PMIXSMP_ISYNC "\n\tisync"


/**********************************************************************
 *
 * Define constants for PowerPC 32
 *
 *********************************************************************/
#define PMIX_HAVE_ATOMIC_MEM_BARRIER 1

#define PMIX_HAVE_ATOMIC_CMPSET_32 1
#define PMIX_HAVE_ATOMIC_SWAP_32 1
#define PMIX_HAVE_ATOMIC_LLSC_32 1

#define PMIX_HAVE_ATOMIC_MATH_32 1
#define PMIX_HAVE_ATOMIC_ADD_32 1
#define PMIX_HAVE_ATOMIC_SUB_32 1


#if (PMIX_ASSEMBLY_ARCH == PMIX_POWERPC64) || PMIX_ASM_SUPPORT_64BIT
#define PMIX_HAVE_ATOMIC_CMPSET_64 1
#define PMIX_HAVE_ATOMIC_SWAP_64 1
#define PMIX_HAVE_ATOMIC_LLSC_64 1
#define PMIX_HAVE_ATOMIC_MATH_64 1
#define PMIX_HAVE_ATOMIC_ADD_64 1
#define PMIX_HAVE_ATOMIC_SUB_64 1
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
    PMIXRMB();
}

static inline
void pmix_atomic_isync(void)
{
    PMIXISYNC();
}

#elif PMIX_XLC_INLINE_ASSEMBLY /* end PMIX_GCC_INLINE_ASSEMBLY */

/* Yeah, I don't know who thought this was a reasonable syntax for
 * inline assembly.  Do these because they are used so often and they
 * are fairly simple (aka: there is a tech pub on IBM's web site
 * containing the right hex for the instructions).
 */

#undef PMIX_HAVE_INLINE_ATOMIC_MEM_BARRIER
#define PMIX_HAVE_INLINE_ATOMIC_MEM_BARRIER 0

#pragma mc_func pmix_atomic_mb { "7c0004ac" }          /* sync  */
#pragma reg_killed_by pmix_atomic_mb                   /* none */

#pragma mc_func pmix_atomic_rmb { "7c2004ac" }         /* lwsync  */
#pragma reg_killed_by pmix_atomic_rmb                  /* none */

#pragma mc_func pmix_atomic_wmb { "7c0006ac" }         /* eieio */
#pragma reg_killed_by pmix_atomic_wmb                  /* none */

#endif

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if PMIX_GCC_INLINE_ASSEMBLY

#ifdef __xlC__
/* work-around bizzare xlc bug in which it sign-extends
   a pointer to a 32-bit signed integer */
#define PMIX_ASM_ADDR(a) ((uintptr_t)a)
#else
#define PMIX_ASM_ADDR(a) (a)
#endif

#if defined(__PGI)
/* work-around for bug in PGI 16.5-16.7 where the compiler fails to
 * correctly emit load instructions for 64-bit operands. without this
 * it will emit lwz instead of ld to load the 64-bit operand. */
#define PMIX_ASM_VALUE64(x) (void *)(intptr_t) (x)
#else
#define PMIX_ASM_VALUE64(x) x
#endif


static inline int pmix_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
   int32_t ret;

   __asm__ __volatile__ (
                         "1: lwarx   %0, 0, %2  \n\t"
                         "   cmpw    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stwcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" PMIX_ASM_ADDR(addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");

   return (ret == oldval);
}

static inline int32_t pmix_atomic_ll_32 (volatile int32_t *addr)
{
   int32_t ret;

   __asm__ __volatile__ ("lwarx   %0, 0, %1  \n\t"
                         : "=&r" (ret)
                         : "r" (addr)
                         );
   return ret;
}

static inline int pmix_atomic_sc_32 (volatile int32_t *addr, int32_t newval)
{
    int32_t ret, foo;

    __asm__ __volatile__ ("   stwcx.  %4, 0, %3  \n\t"
                          "   li      %0,0       \n\t"
                          "   bne-    1f         \n\t"
                          "   ori     %0,%0,1    \n\t"
                          "1:"
                          : "=r" (ret), "=m" (*addr), "=r" (foo)
                          : "r" (addr), "r" (newval)
                          : "cc", "memory");
    return ret;
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

static inline int32_t pmix_atomic_swap_32(volatile int32_t *addr, int32_t newval)
{
    int32_t ret;

    __asm__ __volatile__ ("1: lwarx   %0, 0, %2  \n\t"
                          "   stwcx.  %3, 0, %2  \n\t"
                          "   bne-    1b         \n\t"
                          : "=&r" (ret), "=m" (*addr)
                          : "r" (addr), "r" (newval)
                          : "cc", "memory");

   return ret;
}

#endif /* PMIX_GCC_INLINE_ASSEMBLY */


#if (PMIX_ASSEMBLY_ARCH == PMIX_POWERPC64)

#if  PMIX_GCC_INLINE_ASSEMBLY

static inline int64_t pmix_atomic_add_64 (volatile int64_t* v, int64_t inc)
{
   int64_t t;

   __asm__ __volatile__("1:   ldarx   %0, 0, %3    \n\t"
                        "     add     %0, %2, %0   \n\t"
                        "     stdcx.  %0, 0, %3    \n\t"
                        "     bne-    1b           \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (PMIX_ASM_VALUE64(inc)), "r" PMIX_ASM_ADDR(v), "m" (*v)
                        : "cc");

   return t;
}


static inline int64_t pmix_atomic_sub_64 (volatile int64_t* v, int64_t dec)
{
   int64_t t;

   __asm__ __volatile__(
                        "1:   ldarx   %0,0,%3      \n\t"
                        "     subf    %0,%2,%0     \n\t"
                        "     stdcx.  %0,0,%3      \n\t"
                        "     bne-    1b           \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (PMIX_ASM_VALUE64(dec)), "r" PMIX_ASM_ADDR(v), "m" (*v)
                        : "cc");

   return t;
}

static inline int pmix_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
   int64_t ret;

   __asm__ __volatile__ (
                         "1: ldarx   %0, 0, %2  \n\t"
                         "   cmpd    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stdcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (PMIX_ASM_VALUE64(oldval)), "r" (PMIX_ASM_VALUE64(newval)), "m" (*addr)
                         : "cc", "memory");

   return (ret == oldval);
}

static inline int64_t pmix_atomic_ll_64(volatile int64_t *addr)
{
   int64_t ret;

   __asm__ __volatile__ ("ldarx   %0, 0, %1  \n\t"
                         : "=&r" (ret)
                         : "r" (addr)
                         );
   return ret;
}

static inline int pmix_atomic_sc_64(volatile int64_t *addr, int64_t newval)
{
    int32_t ret;

    __asm__ __volatile__ ("   stdcx.  %2, 0, %1  \n\t"
                          "   li      %0,0       \n\t"
                          "   bne-    1f         \n\t"
                          "   ori     %0,%0,1    \n\t"
                          "1:"
                          : "=r" (ret)
                          : "r" (addr), "r" (PMIX_ASM_VALUE64(newval))
                          : "cc", "memory");
    return ret;
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

static inline int64_t pmix_atomic_swap_64(volatile int64_t *addr, int64_t newval)
{
   int64_t ret;

   __asm__ __volatile__ ("1: ldarx   %0, 0, %2  \n\t"
                         "   stdcx.  %3, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (PMIX_ASM_VALUE64(newval))
                         : "cc", "memory");

   return ret;
}

#endif /* PMIX_GCC_INLINE_ASSEMBLY */

#elif (PMIX_ASSEMBLY_ARCH == PMIX_POWERPC32) && PMIX_ASM_SUPPORT_64BIT

#ifndef ll_low /* GLIBC provides these somewhere, so protect */
#define ll_low(x)       *(((unsigned int*)&(x))+0)
#define ll_high(x)      *(((unsigned int*)&(x))+1)
#endif

#if  PMIX_GCC_INLINE_ASSEMBLY

static inline int pmix_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
    int ret;

    /*
     * We force oldval and newval into memory because PPC doesn't
     * appear to have a way to do a move register with offset.  Since
     * this is 32-bit code, a 64 bit integer will be loaded into two
     * registers (assuming no inlining, addr will be in r3, oldval
     * will be in r4 and r5, and newval will be r6 and r7.  We need
     * to load the whole thing into one register.  So we have the
     * compiler push the values into memory and load the double word
     * into registers.  We use r4,r5 so that the main block of code
     * is very similar to the pure 64 bit version.
     */
   __asm__ __volatile__ (
                         "ld r4,%2         \n\t"
                         "ld r5,%3        \n\t"
                         "1: ldarx   r9, 0, %1  \n\t"
                         "   cmpd    0, r9, r4  \n\t"
                         "   bne-    2f         \n\t"
                         "   stdcx.  r5, 0, %1  \n\t"
                         "   bne-    1b         \n\t"
                         "2:                    \n\t"
                         "xor r5,r4,r9          \n\t"
                         "subfic r9,r5,0        \n\t"
                         "adde %0,r9,r5         \n\t"
                         : "=&r" (ret)
                         : "r"PMIX_ASM_ADDR(addr),
                           "m"(oldval), "m"(newval)
                         : "r4", "r5", "r9", "cc", "memory");

     return ret;
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

#endif /* PMIX_GCC_INLINE_ASSEMBLY */

#endif /* PMIX_ASM_SUPPORT_64BIT */


#if PMIX_GCC_INLINE_ASSEMBLY

static inline int32_t pmix_atomic_add_32(volatile int32_t* v, int inc)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx   %0, 0, %3    \n\t"
                        "     add     %0, %2, %0   \n\t"
                        "     stwcx.  %0, 0, %3    \n\t"
                        "     bne-    1b           \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (inc), "r" PMIX_ASM_ADDR(v), "m" (*v)
                        : "cc");

   return t;
}


static inline int32_t pmix_atomic_sub_32(volatile int32_t* v, int dec)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx   %0,0,%3      \n\t"
                        "     subf    %0,%2,%0     \n\t"
                        "     stwcx.  %0,0,%3      \n\t"
                        "     bne-    1b           \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (dec), "r" PMIX_ASM_ADDR(v), "m" (*v)
                        : "cc");

   return t;
}


#endif /* PMIX_GCC_INLINE_ASSEMBLY */

#endif /* ! PMIX_SYS_ARCH_ATOMIC_H */
