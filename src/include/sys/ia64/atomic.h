/*
 * $HEADER$
 */

#ifndef LAM_SYS_ATOMIC_H_INCLUDED
#define LAM_SYS_ATOMIC_H_INCLUDED

/*
 * On ia64, we use cmpxchg, which supports acquire/release semantics natively.
 */


#ifdef HAVE_SMP
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define MB()
#endif


static inline lam_atomic_mb(void)
{
    MB();
}


static inline lam_atomic_rmb(void)
{
    MB();
}


static inline lam_atomic_wmb(void)
{
    MB();
}


static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    uint32_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg4.acq %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(old), "r"(new), "m"(*addr)
    : "memory");

    return (ret == old);
}


static inline int lam_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    uint32_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg4.rel %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(old), "r"(new), "m"(*addr)
    : "memory");

    return (ret == old);
}


static inline int lam_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t old,
                                       uint32_t new)
{
    return lam_atomic_cmpset_acq_32(addr, old, new);
}


static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    uint64_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg8.acq %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(old), "r"(new), "m"(*addr)
    : "memory");

    return (ret == old);
}


static inline int lam_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    uint64_t ret;

    __asm__ __volatile(
"   mov ar.ccv=%2                \n\
    cmpxchg8.rel %0=%4,%3,ar.ccv \n"
    : "=r"(ret), "=m"(*addr)
    : "r"(old), "r"(new), "m"(*addr)
    : "memory");
    return (ret);
}


static inline int lam_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t old,
                                       uint64_t new)
{
    return lam_atomic_cmpset_acq_64(addr, old, new);
}


#endif /* ! LAM_SYS_ATOMIC_H_INCLUDED */
