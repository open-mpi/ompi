/*
 * $HEADER$
 */

#ifndef OMPI_SYS_ATOMIC_H_INCLUDED
#define OMPI_SYS_ATOMIC_H_INCLUDED

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

    __asm __volatile__ (
"1:  ldl_l %0, %1        // load oldval value            \n\
     cmpeq %0, %2, %0    // compare                   \n\
     beq %0, 2f          // exit if not equal         \n\
     mov %3, %0          // value to store            \n\
     stl_c %0, %1        // attempt to store          \n\
     beq %0, 3f          // if failed, try again      \n\
2:                       // done                      \n\
3:   br 1b               // try again                 \n\
.previous               \n"
    : "=&r" (ret), "+m" (*addr)
    : "r" (oldval), "r" (newval)
    : "memory");

    return ret;
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
    uint32_t ret;

    __asm__ __volatile__ (
"1:  ldq_l %0, %1        // load oldval value            \n\
     cmpeq %0, %2, %0    // compare                   \n\
     beq %0, 2f          // exit if not equal         \n\
     mov %3, %0          // value to store            \n\
     stq_c %0, %1        // attempt to store          \n\
     beq %0, 3f          // if failed, try again      \n\
2:                       // done                      \n\
3:   br 1b               // try again                 \n\
.previous               \n"
    : "=&r" (ret), "+m" (*addr)
    : "r" (oldval), "r" (newval)
    : "memory");

    return ret;
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


#endif /* ! OMPI_SYS_ATOMIC_H_INCLUDED */
