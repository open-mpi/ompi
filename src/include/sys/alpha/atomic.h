/*
 * $HEADER$
 */

#ifndef LAM_SYS_ATOMIC_H_INCLUDED
#define LAM_SYS_ATOMIC_H_INCLUDED

/*
 * On alpha, everything is load-locked, store-conditional...
 */

#ifdef HAVE_SMP

#define MB()  __asm__ __volatile__ ("mb");
#define RMB() __asm__ __volatile__ ("mb");
#define WMB() __asm__ __volatile__ ("wmb");

#else

#define MB()
#define RMB()
#define WMB()

#endif


static inline lam_atomic_mb(void)
{
    MB();
}


static inline lam_atomic_rmb(void)
{
    RMB();
}


static inline lam_atomic_wmb(void)
{
    WMB();
}


static inline int lam_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t old,
                                       uint32_t new)
{
    uint32_t ret;

    __asm __volatile__ (
"1:  ldl_l %0, %1        // load old value            \n\
     cmpeq %0, %2, %0    // compare                   \n\
     beq %0, 2f          // exit if not equal         \n\
     mov %3, %0          // value to store            \n\
     stl_c %0, %1        // attempt to store          \n\
     beq %0, 3f          // if failed, try again      \n\
2:                       // done                      \n\
3:   br 1b               // try again                 \n\
.previous               \n"
    : "=&r" (ret), "+m" (*addr)
    : "r" (old), "r" (new)
    : "memory");

    return ret;
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


static inline int lam_atomic_cmpset_64(volatile uint64_t *addr,
                                       uint64_t old,
                                       uint64_t new)
{
    uint32_t ret;

    __asm__ __volatile__ (
"1:  ldq_l %0, %1        // load old value            \n\
     cmpeq %0, %2, %0    // compare                   \n\
     beq %0, 2f          // exit if not equal         \n\
     mov %3, %0          // value to store            \n\
     stq_c %0, %1        // attempt to store          \n\
     beq %0, 3f          // if failed, try again      \n\
2:                       // done                      \n\
3:   br 1b               // try again                 \n\
.previous               \n"
    : "=&r" (ret), "+m" (*addr)
    : "r" (old), "r" (new)
    : "memory");

    return ret;
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
