/*
 * $HEADER$
 */

#ifndef LAM_SYS_ATOMIC_H_INCLUDED
#define LAM_SYS_ATOMIC_H_INCLUDED

#ifdef SMP
#define LOCK "lock; "
#else
#define LOCK
#endif

/*
 * On amd64, we use cmpxchg.
 */

static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                            uint32_t cmp,
                                            uint32_t new)
{
    uint32_t ret = cmp;

    __asm __volatile (
        LOCK "cmpxchgl %1,%2;   "
        "     setz     %%al; "
        "     movzbl   %%al,%0; "
        : "+a" (ret)
        : "r" (new), "m" (*(addr))
        : "memory");

    return (ret == cmp);
}


static inline int lam_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                            uint32_t cmp,
                                            uint32_t new)
{
    return lam_atomic_cmpset_acq_32(addr, cmp, new);
}


static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                            uint64_t cmp,
                                            uint64_t new)
{
    uint64_t ret = cmp;

    __asm __volatile (
        LOCK "cmpxchgq %1,%2; "
        "     setz     %%al; "
        "     movzbl   %%al,%0; "
        : "+a" (ret)
        : "r" (new), "m" (*(addr))
        : "memory");

    return (ret == cmp);
}


static inline int lam_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                            uint64_t cmp,
                                            uint64_t new)
{
    return lam_atomic_cpmset_acq_64(addr, cmp, new);
}

#endif /* ! LAM_SYS_ATOMIC_H_INCLUDED */
