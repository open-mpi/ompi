/*
 * $HEADER$
 */

#ifndef LAM_SYS_ATOMIC_H_INCLUDED
#define LAM_SYS_ATOMIC_H_INCLUDED

/*
 * On powerpc ...
 */

#define lam_atomic_mb()  __asm__ __volatile__ ("sync" : : : "memory")
#define lam_atomic_rmb() __asm__ __volatile__ ("lwsync" : : : "memory")
#define lam_atomic_wmb() __asm__ __volatile__ ("eieio" : : : "memory")


static inline int lam_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t old,
                                       uint32_t new)
{
    uint32_t ret;

    __asm__ __volatile__ (
"1: lwarx   %0, 0, %2  \n\
    cmpw    0, %0, %3  \n\
    bne-    2f         \n\
    stwcx.  %4, 0, %2  \n\
    bne-    1b         \n\
2:"
    : "=&r" (ret), "=m" (*addr)
    : "r" (addr), "r" (old), "r" (new), "m" (*addr)
    : "cc", "memory");

    return (ret == old);
}


static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    int rc;

    rc = lam_atomic_cmpset_32(addr, old, new);
    lam_atomic_rmb();

    return rc;
}


static inline int lam_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    lam_atomic_wmb();
    return lam_atomic_cmpset_32(addr, old, new);
}


#if 
static inline int lam_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t old,
                                       uint64_t new)
{
    uint64_t ret;

    __asm__ __volatile__ (
"1: ldarx   %0, 0, %2  \n\
    cmpd    0, %0, %3  \n\
    bne-    2f         \n\
    stdcx.  %4, 0, %2  \n\
    bne-    1b         \n\
2:"
    : "=&r" (ret), "=m" (*addr)
    : "r" (addr), "r" (old), "r" (new), "m" (*addr)
    : "cc", "memory");

    return (ret == old);
}


static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    int rc;

    rc = lam_atomic_cmpset_64(addr, old, new);
    lam_atomic_rmb();

    return rc;
}


static inline int lam_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    lam_atomic_wmb();
    return lam_atomic_cmpset_64(addr, old, new);
}


#endif /* ! LAM_SYS_ATOMIC_H_INCLUDED */
