/*
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

#else

#define MB()
#define RMB()
#define WMB()

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


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t oldval,
                                       uint32_t newval)
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
    : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
    : "cc", "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_32(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t oldval,
                                       uint64_t newval)
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
    : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
    : "cc", "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_64(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_64(addr, oldval, newval);
}


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
