/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ATOMIC_H_INCLUDED
#define OMPI_SYS_ATOMIC_H_INCLUDED

/*
 * On ia32, we use cmpxchg.
 */


#ifdef HAVE_SMP
#define SMPLOCK "lock; "
#define MB() __asm__ __volatile__("": : :"memory")
#else
#define SMPLOCK
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


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t old,
                                       uint32_t new)
{
    uint32_t ret = old;

    __asm__ __volatile (
SMPLOCK "cmpxchgl %1,%2   \n\
      setz     %%al    \n\
      movzbl   %%al,%0 \n"
    : "+a" (ret)
    : "r" (new), "m" (*addr)
    : "memory");

    return (ret == old);
}


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    return ompi_atomic_cmpset_32(addr, old, new);
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t old,
                                           uint32_t new)
{
    return ompi_atomic_cmpset_32(addr, old, new);
}


static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t old,
                                       uint64_t new)
{
    /* 
     * Compare EDX:EAX with m64. If equal, set ZF and load ECX:EBX into
     * m64. Else, clear ZF and load m64 into EDX:EAX.
     */

    uint64_t ret = old;
#if 0
    struct { uint32_t lo; uint32_t hi; } *p = (struct lwords *) &new;

    __asm__ __volatile(
SMPLOCK "cmpxchg8b %1\n"
    : "+A" (ret) 
    : "m" (*addr), "b" (p->lo), "c" (p->hi)
    : "memory");
#endif
    return (ret == old);
}


static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    return ompi_atomic_cmpset_64(addr, old, new);
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t old,
                                           uint64_t new)
{
    return ompi_atomic_cmpset_64(addr, old, new);
}

#endif /* ! OMPI_SYS_ATOMIC_H_INCLUDED */
