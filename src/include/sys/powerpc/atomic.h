/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ATOMIC_H_INCLUDED
#define OMPI_SYS_ATOMIC_H_INCLUDED

/*
 * On powerpc ...
 */

#ifdef HAVE_SMP

#define MB()  __asm__ __volatile__ ("sync" : : : "memory")
#define RMB() __asm__ __volatile__ ("lwsync" : : : "memory")
#define WMB() __asm__ __volatile__ ("eieio" : : : "memory")

#else

#define MB()
#define RMB()
#define WMB()

#endif


static inline ompi_atomic_mb(void)
{
    MB();
}


static inline ompi_atomic_rmb(void)
{
    RMB();
}


static inline ompi_atomic_wmb(void)
{
    WMB();
}


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
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


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    int rc;

    rc = ompi_atomic_cmpset_32(addr, old, new);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_32(addr, old, new);
}


static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
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


static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    int rc;

    rc = ompi_atomic_cmpset_64(addr, old, new);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_64(addr, old, new);
}


#endif /* ! OMPI_SYS_ATOMIC_H_INCLUDED */
