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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserverd.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/*
 * On sparc v9, use casa and casxa (compare and swap) instructions.
 */

#define ASI_P "0x80"

#define MEMBAR(type) __asm__  __volatile__ ("membar " type : : : "memory")


/**********************************************************************
 *
 * Define constants for Sparc v9 (Ultra Sparc)
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 1

#define OPAL_HAVE_ATOMIC_CMPSET_64 1

#define OPAL_HAVE_ATOMIC_SWAP_64 0  /* updated later */

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

static inline void opal_atomic_mb(void)
{
    MEMBAR("#LoadLoad | #LoadStore | #StoreStore | #StoreLoad");
}


static inline void opal_atomic_rmb(void)
{
    MEMBAR("#LoadLoad");
}


static inline void opal_atomic_wmb(void)
{
    MEMBAR("#StoreStore");
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

static inline int32_t opal_atomic_cmpset_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
   /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
    *
    * if (*(reg(rs1)) == reg(rs1) )
    *    swap reg(rd), *(reg(rs1))
    * else
    *    reg(rd) = *(reg(rs1))
    */

   int32_t ret = newval;

   __asm__ __volatile__("casa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (oldval));
   return ret;
}


static inline int32_t opal_atomic_cmpset_acq_32( volatile int32_t *addr,
                                                 int32_t oldval, int32_t newval)
{
   int rc;

   rc = opal_atomic_cmpset_32(addr, oldval, newval);
   opal_atomic_rmb();

   return rc;
}


static inline int32_t opal_atomic_cmpset_rel_32( volatile int32_t *addr,
                                                 int32_t oldval, int32_t newval)
{
   opal_atomic_wmb();
   return opal_atomic_cmpset_32(addr, oldval, newval);
}


#if OPAL_ASSEMBLY_ARCH == OPAL_SPARCV9_64

static inline int64_t opal_atomic_cmpset_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
     *
     * if (*(reg(rs1)) == reg(rs1) )
     *    swap reg(rd), *(reg(rs1))
     * else
     *    reg(rd) = *(reg(rs1))
     */
   int64_t ret = newval;

   __asm__ __volatile__("casxa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (oldval));
   return ret;
}

#else /* OPAL_ASSEMBLY_ARCH == OPAL_SPARCV9_64 */

static inline int64_t opal_atomic_cmpset_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    /* casa [reg(rs1)] %asi, reg(rs2), reg(rd)
     *
     * if (*(reg(rs1)) == reg(rs1) )
     *    swap reg(rd), *(reg(rs1))
     * else
     *    reg(rd) = *(reg(rs1))
     *
     */
    int64_t ret = newval;

    __asm__ __volatile__(
                       "ldx %0, %%g1               \n\t" /* g1 = ret */
                       "ldx %2, %%g2               \n\t" /* g2 = oldval */
                       "casxa [%1] " ASI_P ", %%g2, %%g1 \n\t"
                       "stx %%g1, %0               \n"
                       : "+m"(ret)
                       : "r"(addr), "m"(oldval)
                       : "%g1", "%g2"
                       );

   return ret;
}

#endif /* OPAL_ASSEMBLY_ARCH == OPAL_SPARCV9_64 */

static inline int64_t opal_atomic_cmpset_acq_64( volatile int64_t *addr,
                                                 int64_t oldval, int64_t newval)
{
   int64_t rc;

   rc = opal_atomic_cmpset_64(addr, oldval, newval);
   opal_atomic_rmb();

   return rc;
}

static inline int64_t opal_atomic_cmpset_rel_64( volatile int64_t *addr,
                                                 int64_t oldval, int64_t newval)
{
   opal_atomic_wmb();
   return opal_atomic_cmpset_64(addr, oldval, newval);
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#if OPAL_GCC_INLINE_ASSEMBLY

#define OPAL_HAVE_ATOMIC_SWAP_32 1

static inline int32_t
opal_atomic_swap_32( volatile int32_t *addr,
                     int32_t newval)
{
    int32_t prev;
    __asm__ __volatile__(
             "swap %0, %1"
             : "=m" (*addr), "=r" (prev)
             : "1" (newval), "m" (*addr)
             : "memory");
    return prev;
}

#if OPAL_ASSEMBLY_ARCH == OPAL_SPARCV9_64

#undef OPAL_HAVE_ATOMIC_SWAP_64
#define OPAL_HAVE_ATOMIC_SWAP_64 1

static inline int64_t
opal_atomic_swap_64( volatile int64_t *addr,
                     int64_t newval)
{
    int64_t prev;
    __asm__ __volatile__(
             "swap %0, %1"
             : "=m" (*addr), "=r" (prev)
             : "1" (newval), "m" (*addr)
             : "memory");
    return prev;
}
#endif  /* OPAL_ASSEMBLY_ARCH == OPAL_SPARCV9_64 */

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
