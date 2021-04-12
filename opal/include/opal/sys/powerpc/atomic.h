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
 * Copyright (c) 2010-2021 IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/*
 * On powerpc ...
 */

#define MB()    __asm__ __volatile__("sync" : : : "memory")
#define RMB()   __asm__ __volatile__("lwsync" : : : "memory")
#define WMB()   __asm__ __volatile__("lwsync" : : : "memory")
#define ISYNC() __asm__ __volatile__("isync" : : : "memory")

/**********************************************************************
 *
 * Define constants for PowerPC 64
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 1
#define OPAL_HAVE_ATOMIC_SWAP_32             1
#define OPAL_HAVE_ATOMIC_LLSC_32             1

#define OPAL_HAVE_ATOMIC_ADD_32  1
#define OPAL_HAVE_ATOMIC_AND_32  1
#define OPAL_HAVE_ATOMIC_OR_32   1
#define OPAL_HAVE_ATOMIC_XOR_32  1
#define OPAL_HAVE_ATOMIC_SUB_32  1
#define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64 1
#define OPAL_HAVE_ATOMIC_SWAP_64             1
#define OPAL_HAVE_ATOMIC_LLSC_64             1
#define OPAL_HAVE_ATOMIC_ADD_64              1
#define OPAL_HAVE_ATOMIC_AND_64              1
#define OPAL_HAVE_ATOMIC_OR_64               1
#define OPAL_HAVE_ATOMIC_XOR_64              1
#define OPAL_HAVE_ATOMIC_SUB_64              1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

static inline void opal_atomic_mb(void)
{
    MB();
}

static inline void opal_atomic_rmb(void)
{
    RMB();
}

static inline void opal_atomic_wmb(void)
{
    WMB();
}

static inline void opal_atomic_isync(void)
{
    ISYNC();
}

#endif /* end OPAL_GCC_INLINE_ASSEMBLY */

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

#    if defined(__xlC__) || defined(__IBMC__) || defined(__IBMCPP__) || defined(__ibmxl__)
/* work-around bizzare xlc bug in which it sign-extends
   a pointer to a 32-bit signed integer */
#        define OPAL_ASM_ADDR(a) ((uintptr_t) a)
#    else
#        define OPAL_ASM_ADDR(a) (a)
#    endif

#    if defined(__PGI)
/* work-around for bug in PGI 16.5-16.7 where the compiler fails to
 * correctly emit load instructions for 64-bit operands. without this
 * it will emit lwz instead of ld to load the 64-bit operand. */
#        define OPAL_ASM_VALUE64(x) (void *) (intptr_t)(x)
#    else
#        define OPAL_ASM_VALUE64(x) x
#    endif

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    int32_t prev;
    bool ret;

    __asm__ __volatile__("1: lwarx   %0, 0, %2  \n\t"
                         "   cmpw    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stwcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r"(prev), "=m"(*addr)
                         : "r" OPAL_ASM_ADDR(addr), "r"(*oldval), "r"(newval), "m"(*addr)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

/* NTH: the LL/SC support is done through macros due to issues with non-optimized builds. The reason
 * is that even with an always_inline attribute the compiler may still emit instructions to store
 * then load the arguments to/from the stack. This sequence may cause the ll reservation to be
 * cancelled. */
#    define opal_atomic_ll_32(addr, ret)                                                \
        do {                                                                            \
            opal_atomic_int32_t *_addr = (addr);                                        \
            int32_t _ret;                                                               \
            __asm__ __volatile__("lwarx   %0, 0, %1  \n\t" : "=&r"(_ret) : "r"(_addr)); \
            ret = (typeof(ret)) _ret;                                                   \
        } while (0)

#    define opal_atomic_sc_32(addr, value, ret)                         \
        do {                                                            \
            opal_atomic_int32_t *_addr = (addr);                        \
            int32_t _ret, _foo, _newval = (int32_t) value;              \
                                                                        \
            __asm__ __volatile__("   stwcx.  %4, 0, %3  \n\t"           \
                                 "   li      %0,0       \n\t"           \
                                 "   bne-    1f         \n\t"           \
                                 "   ori     %0,%0,1    \n\t"           \
                                 "1:"                                   \
                                 : "=r"(_ret), "=m"(*_addr), "=r"(_foo) \
                                 : "r"(_addr), "r"(_newval)             \
                                 : "cc", "memory");                     \
            ret = _ret;                                                 \
        } while (0)

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_32 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    bool rc;

    rc = opal_atomic_compare_exchange_strong_32(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}

static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_compare_exchange_strong_32(addr, oldval, newval);
}

static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval)
{
    int32_t ret;

    __asm__ __volatile__("1: lwarx   %0, 0, %2  \n\t"
                         "   stwcx.  %3, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         : "=&r"(ret), "=m"(*addr)
                         : "r"(addr), "r"(newval)
                         : "cc", "memory");

    return ret;
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#if OPAL_GCC_INLINE_ASSEMBLY

#    define OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(type, instr)                                    \
        static inline int64_t opal_atomic_fetch_##type##_64(opal_atomic_int64_t *v, int64_t val) \
        {                                                                                        \
            int64_t t, old;                                                                      \
                                                                                                 \
            __asm__ __volatile__("1:   ldarx   %1, 0, %4    \n\t"                                \
                                 "     " #instr "     %0, %3, %1   \n\t"                         \
                                 "     stdcx.  %0, 0, %4    \n\t"                                \
                                 "     bne-    1b           \n\t"                                \
                                 : "=&r"(t), "=&r"(old), "=m"(*v)                                \
                                 : "r"(OPAL_ASM_VALUE64(val)), "r" OPAL_ASM_ADDR(v), "m"(*v)     \
                                 : "cc");                                                        \
                                                                                                 \
            return old;                                                                          \
        }

OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(add, add)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(and, and)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(or, or)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(xor, xor)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(sub, subf)

static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr,
                                                          int64_t *oldval, int64_t newval)
{
    int64_t prev;
    bool ret;

    __asm__ __volatile__("1: ldarx   %0, 0, %2  \n\t"
                         "   cmpd    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stdcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r"(prev), "=m"(*addr)
                         : "r"(addr), "r"(OPAL_ASM_VALUE64(*oldval)), "r"(OPAL_ASM_VALUE64(newval)),
                           "m"(*addr)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

#        define opal_atomic_ll_64(addr, ret)                                                \
            do {                                                                            \
                opal_atomic_int64_t *_addr = (addr);                                        \
                int64_t _ret;                                                               \
                __asm__ __volatile__("ldarx   %0, 0, %1  \n\t" : "=&r"(_ret) : "r"(_addr)); \
                ret = (typeof(ret)) _ret;                                                   \
            } while (0)

#        define opal_atomic_sc_64(addr, value, ret)                               \
            do {                                                                  \
                opal_atomic_int64_t *_addr = (addr);                              \
                int64_t _newval = (int64_t) value;                                \
                int32_t _ret;                                                     \
                                                                                  \
                __asm__ __volatile__("   stdcx.  %2, 0, %1  \n\t"                 \
                                     "   li      %0,0       \n\t"                 \
                                     "   bne-    1f         \n\t"                 \
                                     "   ori     %0,%0,1    \n\t"                 \
                                     "1:"                                         \
                                     : "=r"(_ret)                                 \
                                     : "r"(_addr), "r"(OPAL_ASM_VALUE64(_newval)) \
                                     : "cc", "memory");                           \
                ret = _ret;                                                       \
            } while (0)

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t ret;

    __asm__ __volatile__("1: ldarx   %0, 0, %2  \n\t"
                         "   stdcx.  %3, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         : "=&r"(ret), "=m"(*addr)
                         : "r"(addr), "r"(OPAL_ASM_VALUE64(newval))
                         : "cc", "memory");

    return ret;
}

#    endif /* OPAL_GCC_INLINE_ASSEMBLY */

#if OPAL_GCC_INLINE_ASSEMBLY

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_64 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    bool rc;

    rc = opal_atomic_compare_exchange_strong_64(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}

static inline bool opal_atomic_compare_exchange_strong_rel_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_compare_exchange_strong_64(addr, oldval, newval);
}

#    define OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(type, instr)                                \
        static inline int32_t opal_atomic_fetch_##type##_32(opal_atomic_int32_t *v, int val) \
        {                                                                                    \
            int32_t t, old;                                                                  \
                                                                                             \
            __asm__ __volatile__("1:   lwarx   %1, 0, %4    \n\t"                            \
                                 "     " #instr "     %0, %3, %1   \n\t"                     \
                                 "     stwcx.  %0, 0, %4    \n\t"                            \
                                 "     bne-    1b           \n\t"                            \
                                 : "=&r"(t), "=&r"(old), "=m"(*v)                            \
                                 : "r"(val), "r" OPAL_ASM_ADDR(v), "m"(*v)                   \
                                 : "cc");                                                    \
                                                                                             \
            return old;                                                                      \
        }

OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(add, add)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(and, and)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(or, or)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(xor, xor)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(sub, subf)

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
