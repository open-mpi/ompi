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

#ifdef HAVE_SMP
#define MEMBAR(type) __asm__  __volatile__ ("membar" type : : : "memory")
#else
#define MEMBAR(type)
#endif


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

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_32
static inline int ompi_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
   int32_t ret = oldval;

   __asm__ __volatile("casa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (newval));
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

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_64
static inline int ompi_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
   int64_t ret = oldval;

   __asm__ __volatile("casxa [%1] " ASI_P ", %2, %0"
                      : "+r" (ret)
                      : "r" (addr), "r" (newval));
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


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
