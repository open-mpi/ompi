/*
 * $HEADER$
 */

#ifndef LAM_SYS_ATOMIC_H_INCLUDED
#define LAM_SYS_ATOMIC_H_INCLUDED

/*
 * On ia64, we use cmpxchg, which supports acquire/release semantics natively.
 */

static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t cmp,
                                           uint32_t new)
{
    uint32_t ret;

    __asm __volatile("mov ar.ccv=%2; "
                     "cmpxchg4.acq %0=%4,%3,ar.ccv; "
                     : "=r"(ret), "=m"(*addr)
                     : "r"(cmp), "r"(new), "m"(*addr)
                     : "memory");
    return (ret == cmp);
}


static inline int lam_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t cmp,
                                           uint32_t new)
{
    uint32_t ret;

    __asm __volatile("mov ar.ccv=%2; "
                     "cmpxchg4.rel %0=%4,%3,ar.ccv; "
                     : "=r"(ret), "=m"(*addr)
                     : "r"(cmp), "r"(new), "m"(*addr)
                     : "memory");
    return (ret == cmp);
}


static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t cmp,
                                           uint64_t new)
{
    uint64_t ret;

    __asm __volatile("mov ar.ccv=%2; "
                     "cmpxchg8.acq %0=%4,%3,ar.ccv; "
                     : "=r"(ret), "=m"(*addr)
                     : "r"(cmp), "r"(new), "m"(*addr)
                     : "memory");

    return (ret == cmp);
}


static inline int lam_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t cmp,
                                           uint64_t new)
{
    uint64_t ret;

    __asm __volatile("mov ar.ccv=%2; "
                     "cmpxchg8.rel %0=%4,%3,ar.ccv; "
                     : "=r"(ret), "=m"(*addr)
                     : "r"(cmp), "r"(new), "m"(*addr)
                     : "memory");
    return (ret);
}

#endif /* ! LAM_SYS_ATOMIC_H_INCLUDED */
