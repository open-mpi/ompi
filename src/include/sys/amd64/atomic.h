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
 * On amd64, we use cmpxchg.
 */


#if OMPI_WANT_SMP_LOCKS
#define SMPLOCK "lock; "
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define SMPLOCK
#define MB()
#endif


/**********************************************************************
 *
 * Define constants for AMD64 / x86_64 / EM64T / ...
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
    MB();
}


static inline void ompi_atomic_rmb(void)
{
    MB();
}


static inline void ompi_atomic_wmb(void)
{
    MB();
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
    unsigned long prev;
    __asm__ __volatile__(SMPLOCK "cmpxchgl %1,%2"
                         : "=a"(prev)
                         : "q"(newval), "m"(*addr), "0"(oldval)
                         : "cc", "memory");
    return ((int32_t)prev == oldval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#define ompi_atomic_cmpset_acq_32 ompi_atomic_cmpset_32
#define ompi_atomic_cmpset_rel_32 ompi_atomic_cmpset_32

#if OMPI_GCC_INLINE_ASSEMBLY

static inline int ompi_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
   int64_t prev;
    
   __asm__ __volatile (
                       SMPLOCK "cmpxchgq %1,%2   \n\t"
                       : "=a" (prev)
                       : "q" (newval), "m" (*(addr)), "0"(oldval)
                       : "cc", "memory");
    
   return (prev == oldval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#define ompi_atomic_cmpset_acq_64 ompi_atomic_cmpset_64
#define ompi_atomic_cmpset_rel_64 ompi_atomic_cmpset_64

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
