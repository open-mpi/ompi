/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ATOMIC_H_INCLUDED
#define OMPI_SYS_ATOMIC_H_INCLUDED

/*
 * On sparc64, use casa and casxa (compare and swap) instructions.
 */

#ifdef HAVE_SMP
#define MEMBAR(type) __asm__  __volatile__ ("membar" type : : : "memory")
#else
#define MEMBAR(type)
#endif


static inline void ompi_atomic_mb(void)
{
    MEMBAR("#LoadLoad | #LoadStore | #StoreStore | #StoreLoad");
}


static inline void ompi_atomic_rmb(void)
{
    MEMBAR("#LoadLoad");
}


static inline void ompi_atomic_wmb(void)
{
    MEMBAR("#StoreStore");
}


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t old,
                                       uint32_t new)
{
    uint32_t ret = old;

    __asm__ __volatile("casa [%1] ASI_P, %2, %0"
                       : "+r" (ret)
                       : "r" (addr), "r" (new));
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
    uint64_t ret = old;

    __asm__ __volatile("casxa [%1] ASI_P, %2, %0"
                       : "+r" (ret)
                       : "r" (addr), "r" (new));
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
