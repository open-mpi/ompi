/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On ia64, we use cmpxchg, which supports acquire/release semantics natively.
 */


#ifdef HAVE_SMP
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define MB()
#endif


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

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_32
static inline int ompi_atomic_cmpset_acq_32( volatile uint32_t *addr,
                                             uint32_t oldval, uint32_t newval)
{
    uint32_t ret;

    __asm__ __volatile(
                       "mov ar.ccv=%2                \n\t"
                       "cmpxchg4.acq %0=%4,%3,ar.ccv \n\t"
                       : "=r"(ret), "=m"(*addr)
                       : "r"(oldval), "r"(newval), "m"(*addr)
                       : "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_rel_32( volatile uint32_t *addr,
                                             uint32_t oldval, uint32_t newval)
{
   uint32_t ret;

   __asm__ __volatile(
                      "mov ar.ccv=%2                \n\t"
                      "cmpxchg4.rel %0=%4,%3,ar.ccv \n\t"
                      : "=r"(ret), "=m"(*addr)
                      : "r"(oldval), "r"(newval), "m"(*addr)
                      : "memory");
    
   return (ret == oldval);
}


#define ompi_atomic_cmpset_32 ompi_atomic_cmpset_acq_32

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_CMPSET_64
static inline int ompi_atomic_cmpset_acq_64( volatile uint64_t *addr,
                                             uint64_t oldval, uint64_t newval)
{
   uint64_t ret;

   __asm__ __volatile(
                      "mov ar.ccv=%2                \n\t"
                      "cmpxchg8.acq %0=%4,%3,ar.ccv \n\t"
                      : "=r"(ret), "=m"(*addr)
                      : "r"(oldval), "r"(newval), "m"(*addr)
                      : "memory");
    
   return (ret == oldval);
}


static inline int ompi_atomic_cmpset_rel_64( volatile uint64_t *addr,
                                             uint64_t oldval, uint64_t newval)
{
   uint64_t ret;

   __asm__ __volatile(
                      "mov ar.ccv=%2                \n\t"
                      "cmpxchg8.rel %0=%4,%3,ar.ccv \n\t"
                      : "=r"(ret), "=m"(*addr)
                      : "r"(oldval), "r"(newval), "m"(*addr)
                      : "memory");
   return (ret);
}


#define ompi_atomic_cmpset_64 ompi_atomic_cmpset_acq_64

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
