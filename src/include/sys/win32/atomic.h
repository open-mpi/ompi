/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

#if 1
/*
 * On ia64, we use cmpxchg, which supports acquire/release semantics natively.
 */

static inline void ompi_atomic_mb(void) {
    
#if 0
    return KeMemoryBarrier();
#endif
}


static inline void ompi_atomic_rmb(void) {
    
#if 0
    return KeMemoryBarrier();
#endif
}


static inline void ompi_atomic_wmb(void) {
    
#if 0
    return KeMemoryBarrier();
#endif
}


static inline int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval) {

#if 0
    LONG ret = InterlockedCompareExchangeAcquire ((LONG volatile*) addr,
                                                  (LONG) newval,
                                                  (LONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t oldval,
                                           uint32_t newval) {

#if 0
    LONG ret = InterlockedCompareExchangeRelease ((LONG volatile*) addr,
                                                  (LONG) newval,
                                                  (LONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t oldval,
                                       uint32_t newval) {
#if 0
    LONG ret = InterlockedCompareExchange ((LONG volatile*) addr,
                                           (LONG) newval,
                                           (LONG) oldval);

    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}



static inline int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval) {
    
#if 0
    LONGLONG ret = InterlockedCompareExchangeAcquire64 ((LONGLONG volatile*) addr,
                                                        (LONGLONG) newval,
                                                        (LONGLONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t oldval,
                                           uint64_t newval) {

#if 0
    LONGLONG ret = InterlockedCompareExchangeRelease64 ((LONGLONG volatile*) addr,
                                                        (LONGLONG) newval,
                                                        (LONGLONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t oldval,
                                       uint64_t newval) {
#if 0
    LONGLONG ret = InterlockedCompareExchange64 ((LONGLONG volatile*) addr,
                                                 (LONGLONG) newval,
                                                 (LONGLONG) oldval);

    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}

static inline uint32_t ompi_atomic_add_32(volatile uint32_t *addr, int delta) {

    return InterlockedExchangeAdd ((LONG volatile *) addr,
                                    (LONG) delta);

}

static inline uint64_t ompi_atomic_add_64(volatile uint64_t *addr, int delta) {

#if 0
    return InterlockedExchangeAdd64 ((LONGLONG volatile *) addr,
                                      (LONGLONG) delta);
#else
    return 0;
#endif

}

static inline int ompi_atomic_add_int (volatile int *addr, int delta) {

#if 0
#if SIZEOF_INT == 4
    return InterlockedExchangeAdd ((LONG volatile *) addr,
                                    (LONG) delta);
#elif SIZEOF_INT == 8
    return InterlockedExchangeAdd64 ((LONG volatile *) addr,
                                          (LONG) delta);
#else
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_fetch_and_set_int(volatile int *addr, int newval) {

#if 0
#if SIZEOF_INT == 4
    return InterlockedExchangeAdd ((LONG volatile *) addr,
                                    (LONG) newval);
#elif SIZEOF_INT == 8
    return InterlockedExchangeAdd64 ((LONG volatile *) addr,
                                          (LONG) newval);
#else
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_cmpset_int(volatile int *addr,
                                         int oldval,
                                         int newval) {
#if 0
#if SIZEOF_INT == 4
    return ompi_atomic_cmpset_32 (addr, oldval, newval);
#elif SIZEOF_INT == 8
    return ompi_atomic_cmpset_64 (addr, oldval, newval);
#else
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_cmpset_acq_int(volatile int *addr,
                                             int oldval,
                                             int newval) {
#if 0
#if SIZEOF_INT == 4
    return ompi_atomic_cmpset_acq_32 (addr, oldval, newval);
#elif SIZEOF_INT == 8
    return ompi_atomic_cmpset_acq_64 (addr, oldval, newval);
#else
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_cmpset_rel_int(volatile int *addr,
                                             int oldval,
                                             int newval) {
#if 0
#if SIZEOF_INT == 4
    return ompi_atomic_cmpset_rel_32 (addr, oldval, newval);
#elif SIZEOF_INT == 8
    return ompi_atomic_cmpset_rel_64 (addr, oldval, newval);
#else
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_cmpset_ptr(volatile void *addr,
                                         void *oldval,
                                         void *newval) {

    PVOID ret = InterlockedCompareExchangePointer ((PVOID volatile *) addr,
                                                   (PVOID) newval,
                                                   (PVOID) oldval);
    return (ret == oldval)? 1: 0;
}

static inline int ompi_atomic_cmpset_acq_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval) {
#if 0
#if SIZEOF_INT == 4
    return ompi_atomic_cmpset_acq_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval, 
                                     (uint32_t) newval);
#elif SIZEOF_INT == 8
    return ompi_atomic_cmpset_acq_64((volatile uint32_t *) addr,
                                     (uint64_t) oldval, 
                                     (uint64_t) newval);
#else 
#error
#endif
#else
    return 0;
#endif
}

static inline int ompi_atomic_cmpset_rel_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval) {

#if 0
#if SIZEOF_INT == 4
    return ompi_atomic_cmpset_rel_ecq_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval, 
                                     (uint32_t) newval);
#elif SIZEOF_INT == 8
    return ompi_atomic_cmpset_rel__64((volatile uint32_t *) addr,
                                     (uint64_t) oldval, 
                                     (uint64_t) newval);
#else 
#error
#endif
#else
    return 0;
#endif
}
#endif
#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
