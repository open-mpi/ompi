/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ATOMIC_H_INCLUDED
#define OMPI_SYS_ATOMIC_H_INCLUDED

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


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval)
{
    uint32_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg4.acq %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(oldval), "r"(newval), "m"(*addr)
    : "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval)
{
    uint32_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg4.rel %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(oldval), "r"(newval), "m"(*addr)
    : "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t oldval,
                                       uint32_t newval)
{
    return ompi_atomic_cmpset_acq_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    uint64_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg8.acq %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(oldval), "r"(newval), "m"(*addr)
    : "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    uint64_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg8.rel %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(oldval), "r"(newval), "m"(*addr)
    : "memory");
    return (ret);
}


static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t oldval,
                                       uint64_t newval)
{
    return ompi_atomic_cmpset_acq_64(addr, oldval, newval);
}


#endif /* ! OMPI_SYS_ATOMIC_H_INCLUDED */
