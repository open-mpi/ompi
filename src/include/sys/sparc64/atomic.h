/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On sparc64, use casa and casxa (compare and swap) instructions.
 */

#define ASI_P "0x80"

#if OMPI_WANT_SMP_LOCKS
#define MEMBAR(type) __asm__  __volatile__ ("membar " type : : : "memory")
#else
#define MEMBAR(type)
#endif


/**********************************************************************
 *
 * Define constants for UltraSparc 64
 *
 *********************************************************************/
#define OMPI_HAVE_ATOMIC_MEM_BARRIER 1

#define OMPI_HAVE_ATOMIC_CMPSET_32 1

#define OMPI_HAVE_ATOMIC_CMPSET_64 1


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline void ompi_atomic_mb(void)
{
    MEMBAR("#LoadLoad | #LoadStore | #StoreStore | #StoreLoad");
}


static inline void ompi_atomic_rmb(void)
{
    MEMBAR("#LoadLoad");
}


static inline void ompi_atomic_wmb(void)
{
    MEMBAR("#StoreStore");
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline int ompi_atomic_cmpset_32( volatile int32_t *addr,
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

   __asm__ __volatile("casa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (oldval));
   return (ret == oldval);
}


static inline int ompi_atomic_cmpset_acq_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
   int rc;

   rc = ompi_atomic_cmpset_32(addr, oldval, newval);
   ompi_atomic_rmb();

   return rc;
}


static inline int ompi_atomic_cmpset_rel_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
   ompi_atomic_wmb();
   return ompi_atomic_cmpset_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_64( volatile int64_t *addr,
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

   __asm__ __volatile("casxa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (oldval));
   return (ret == oldval);
}


static inline int ompi_atomic_cmpset_acq_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
   int rc;
   
   rc = ompi_atomic_cmpset_64(addr, oldval, newval);
   ompi_atomic_rmb();
   
   return rc;
}


static inline int ompi_atomic_cmpset_rel_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
   ompi_atomic_wmb();
   return ompi_atomic_cmpset_64(addr, oldval, newval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
