/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On amd64, we use cmpxchg.
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
                                       uint32_t oldval,
                                       uint32_t newval)
{
   unsigned long prev;
   __asm__ __volatile__(SMPLOCK "cmpxchgl %k1,%2"
                 : "=a"(prev)
                 : "q"(newval), "m"(*addr), "0"(oldval)
                 : "memory");
   return prev == oldval;
#if 0
    uint32_t ret = oldval;

    __asm__ __volatile (
SMPLOCK "cmpxchgl %1,%2   \n\
      setz     %%al    \n\
      movzbl   %%al,%0 \n"
    : "+a" (ret)
    : "r" (newval), "m" (*(addr))
    : "memory");

    return (ret == oldval);
#endif
}


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                            uint32_t oldval,
                                            uint32_t newval)
{
    return ompi_atomic_cmpset_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                            uint32_t oldval,
                                            uint32_t newval)
{
    return ompi_atomic_cmpset_32(addr, oldval, newval);
}

static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t oldval,
                                       uint64_t newval)
{
    uint64_t ret = oldval;
    
    __asm__ __volatile (
SMPLOCK "cmpxchgq %1,%2   \n\
      setz     %%al    \n\
      movzbl   %%al,%0 \n"
    : "+a" (ret)
    : "r" (newval), "m" (*(addr))
    : "memory");

    return (ret == oldval);
}


static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    return ompi_atomic_cmpset_64( addr, oldval, newval );
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval)
{
    return ompi_atomic_cmpset_64( addr, oldval, newval );
}

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
