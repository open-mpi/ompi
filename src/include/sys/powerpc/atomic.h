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
 * On powerpc ...
 */

#ifdef HAVE_SMP

#define MB()  __asm__ __volatile__ ("sync" : : : "memory")
#define RMB() __asm__ __volatile__ ("lwsync" : : : "memory")
#define WMB() __asm__ __volatile__ ("eieio" : : : "memory")
#define SMP_SYNC  "sync \n\t"
#define SMP_ISYNC "\n\tisync"

#else

#define MB()
#define RMB()
#define WMB()
#define SMP_SYNC  ""
#define SMP_ISYNC

#endif


static inline void ompi_atomic_mb(void)
{
    MB();
}


static inline void ompi_atomic_rmb(void)
{
    RMB();
}


static inline void ompi_atomic_wmb(void)
{
    WMB();
}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_32
static inline int ompi_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
   int32_t ret;

   __asm__ __volatile__ (
                         "1: lwarx   %0, 0, %2  \n\t"
                         "   cmpw    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stwcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         SMP_SYNC
                         "2:"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");

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

#if defined(HOW_TO_DECIDE_IF_THE_ARCHI_SUPPORT_64_BITS_ATOMICS)
#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_64
static inline int ompi_atomic_cmpset_64( volatile int64_t *addr,
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
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");
    
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
#endif  /* HOW_TO_DECIDE_IF_THE_ARCHI_SUPPORT_64_BITS_ATOMICS */

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_32
static inline int32_t ompi_atomic_add_32(volatile int32_t* v, int inc)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx %0, 0, %3 \n\t"
                        "     add  %0, %2, %0                \n\t"
                        "     stwcx.   %0, 0, %3              \n\t"
                        "     bne-  1b                      \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (inc), "r" (v), "m" (*v)
                        : "cc");

   return *v;
}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_32
static inline int32_t ompi_atomic_sub_32(volatile int32_t* v, int dec)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx %0,0,%3\n\t"
                        "     subf  %0,%2,%0                \n\t"
                        "     stwcx.   %0,0,%3              \n\t"
                        "     bne-  1b                      \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (dec), "r" (v), "m" (*v)
                        : "cc");

   return *v;
}

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
