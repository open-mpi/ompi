/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

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


static inline int ompi_atomic_cmpset_acq_32(volatile int32_t *addr,
                                           int32_t oldval,
                                           int32_t newval) {

#if 0
    LONG ret = InterlockedCompareExchangeAcquire ((LONG volatile*) addr,
                                                  (LONG) newval,
                                                  (LONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_rel_32(volatile int32_t *addr,
                                           int32_t oldval,
                                           int32_t newval) {

#if 0
    LONG ret = InterlockedCompareExchangeRelease ((LONG volatile*) addr,
                                                  (LONG) newval,
                                                  (LONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_32(volatile int32_t *addr,
                                       int32_t oldval,
                                       int32_t newval) {
#if 0
    LONG ret = InterlockedCompareExchange ((LONG volatile*) addr,
                                           (LONG) newval,
                                           (LONG) oldval);

    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}



static inline int ompi_atomic_cmpset_acq_64(volatile int64_t *addr,
                                           int64_t oldval,
                                           int64_t newval) {
    
#if 0
    LONGLONG ret = InterlockedCompareExchangeAcquire64 ((LONGLONG volatile*) addr,
                                                        (LONGLONG) newval,
                                                        (LONGLONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_rel_64(volatile int64_t *addr,
                                           int64_t oldval,
                                           int64_t newval) {

#if 0
    LONGLONG ret = InterlockedCompareExchangeRelease64 ((LONGLONG volatile*) addr,
                                                        (LONGLONG) newval,
                                                        (LONGLONG) oldval);
    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}


static inline int ompi_atomic_cmpset_64(volatile int64_t *addr,
                                       int64_t oldval,
                                       int64_t newval) {
#if 0
    LONGLONG ret = InterlockedCompareExchange64 ((LONGLONG volatile*) addr,
                                                 (LONGLONG) newval,
                                                 (LONGLONG) oldval);

    return (oldval == ret) ? 1: 0;
#else
    return 0;
#endif
}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_32
static inline int32_t ompi_atomic_add_32(volatile int32_t *addr, int32_t delta) {

    return InterlockedExchangeAdd ((LONG volatile *) addr,
                                    (LONG) delta);

}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_64
static inline int64_t ompi_atomic_add_64(volatile int64_t *addr, int64_t delta) {

#if 0
    return InterlockedExchangeAdd64 ((LONGLONG volatile *) addr,
                                      (LONGLONG) delta);
#else
    return 0;
#endif

}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_32
static inline int32_t ompi_atomic_sub_32(volatile int32_t *addr, int32_t delta) {

    return InterlockedExchangeAdd ((LONG volatile *) addr,
                                    (LONG) (-delta));

}

#define OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_64
static inline int64_t ompi_atomic_sub_64(volatile int64_t *addr, int64_t delta) {

#if 0
    return InterlockedExchangeAdd64 ((LONGLONG volatile *) addr,
                                      (LONGLONG) (-delta));
#else
    return 0;
#endif

}
#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
