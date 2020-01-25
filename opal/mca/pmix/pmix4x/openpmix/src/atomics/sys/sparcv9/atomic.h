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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserverd.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SYS_ARCH_ATOMIC_H
#define PMIX_SYS_ARCH_ATOMIC_H 1

/*
 * On sparc v9, use casa and casxa (compare and swap) instructions.
 */

#define ASI_P "0x80"

#define MEPMIXMBAR(type) __asm__  __volatile__ ("membar " type : : : "memory")


/**********************************************************************
 *
 * Define constants for Sparc v9 (Ultra Sparc)
 *
 *********************************************************************/
#define PMIX_HAVE_ATOMIC_MEM_BARRIER 1

#define PMIX_HAVE_ATOMIC_COMPARE_EXCHANGE_32 1

#define PMIX_HAVE_ATOMIC_COMPARE_EXCHANGE_64 1


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if PMIX_GCC_INLINE_ASSEMBLY

static inline void pmix_atomic_mb(void)
{
    MEPMIXMBAR("#LoadLoad | #LoadStore | #StoreStore | #StoreLoad");
}


static inline void pmix_atomic_rmb(void)
{
    MEPMIXMBAR("#LoadLoad");
}


static inline void pmix_atomic_wmb(void)
{
    MEPMIXMBAR("#StoreStore");
}

static inline void pmix_atomic_isync(void)
{
}


#endif /* PMIX_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if PMIX_GCC_INLINE_ASSEMBLY

static inline bool pmix_atomic_compare_exchange_strong_32 (pmix_atomic_int32_t *addr, int32_t *oldval, int32_t newval)
{
    /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
     *
     * if (*(reg(rs1)) == reg(rs2) )
     *    swap reg(rd), *(reg(rs1))
     * else
     *    reg(rd) = *(reg(rs1))
     */

    int32_t prev = newval;
    bool ret;

    __asm__ __volatile__("casa [%1] " ASI_P ", %2, %0"
                         : "+r" (prev)
                         : "r" (addr), "r" (*oldval));
    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}


static inline bool pmix_atomic_compare_exchange_strong_acq_32 (pmix_atomic_int32_t *addr, int32_t *oldval, int32_t newval)
{
    bool rc;

    rc = pmix_atomic_compare_exchange_strong_32 (addr, oldval, newval);
    pmix_atomic_rmb();

    return rc;
}


static inline bool pmix_atomic_compare_exchange_strong_rel_32 (pmix_atomic_int32_t *addr, int32_t *oldval, int32_t newval)
{
    pmix_atomic_wmb();
    return pmix_atomic_compare_exchange_strong_32 (addr, oldval, newval);
}


#if PMIX_ASSEMBLY_ARCH == PMIX_SPARCV9_64

static inline bool pmix_atomic_compare_exchange_strong_64 (pmix_atomic_int64_t *addr, int64_t *oldval, int64_t newval)
{
    /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
     *
     * if (*(reg(rs1)) == reg(rs1) )
     *    swap reg(rd), *(reg(rs1))
     * else
     *    reg(rd) = *(reg(rs1))
     */
    int64_t prev = newval;
    bool ret;

    __asm__ __volatile__("casxa [%1] " ASI_P ", %2, %0"
                         : "+r" (prev)
                         : "r" (addr), "r" (*oldval));
    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

#else /* PMIX_ASSEMBLY_ARCH == PMIX_SPARCV9_64 */

static inline bool pmix_atomic_compare_exchange_strong_64 (pmix_atomic_int64_t *addr, int64_t *oldval, int64_t newval)
{
    /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
     *
     * if (*(reg(rs1)) == reg(rs1) )
     *    swap reg(rd), *(reg(rs1))
     * else
     *    reg(rd) = *(reg(rs1))
     *
     */
    int64_t prev = newval;
    bool ret;

    __asm__ __volatile__(
                       "ldx %0, %%g1               \n\t" /* g1 = ret */
                       "ldx %2, %%g2               \n\t" /* g2 = oldval */
                       "casxa [%1] " ASI_P ", %%g2, %%g1 \n\t"
                       "stx %%g1, %0               \n"
                       : "+m"(prev)
                       : "r"(addr), "m"(*oldval)
                       : "%g1", "%g2"
                       );

   ret = (prev == *oldval);
   *oldval = prev;
   return ret;
}

#endif /* PMIX_ASSEMBLY_ARCH == PMIX_SPARCV9_64 */

static inline bool pmix_atomic_compare_exchange_strong_acq_64 (pmix_atomic_int64_t *addr, int64_t *oldval, int64_t newval)
{
    bool rc;

    rc = pmix_atomic_compare_exchange_strong_64 (addr, oldval, newval);
    pmix_atomic_rmb();

    return rc;
}


static inline bool pmix_atomic_compare_exchange_strong_rel_64 (pmix_atomic_int64_t *addr, int64_t *oldval, int64_t newval)
{
    pmix_atomic_wmb();
    return pmix_atomic_compare_exchange_strong_64 (addr, oldval, newval);
}

#endif /* PMIX_GCC_INLINE_ASSEMBLY */


#endif /* ! PMIX_SYS_ARCH_ATOMIC_H */
