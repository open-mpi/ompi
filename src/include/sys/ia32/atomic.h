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
 * On ia32, we use cmpxchg.
 */

static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t cmp,
                                           uint32_t new)
{
    uint32_t ret = cmp;

    __asm __volatile (
        LOCK "cmpxchgl %1,%2; "
        "setz     %%al; "
        "movzbl   %%al,%0; "
        : "+a" (ret)
        : "r" (new), "m" (*addr)
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
    /* 
     * Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into
     * m64. Else, clear ZF and load m64 into EDX:EAX.
     */

    uint64_t ret = cmp;
    struct { uint32_t lo; uint32_t hi; } *p = (struct lwords *) &new;

    __asm __volatile(
        LOCK "cmpxchg8b %1"
        : "+A" (ret) 
        : "m" (*addr), "b" (p->lo), "c" (p->hi)
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
