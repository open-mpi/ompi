/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On alpha, everything is load-locked, store-conditional...
 */

#if OMPI_WANT_SMP_LOCKS

#define MB()  __asm__ __volatile__ ("mb");
#define RMB() __asm__ __volatile__ ("mb");
#define WMB() __asm__ __volatile__ ("wmb");

#else

#define MB()
#define RMB()
#define WMB()

#endif


/**********************************************************************
 *
 * Define constants for PowerPC 32
 *
 *********************************************************************/
#define OMPI_HAVE_ATOMIC_MEM_BARRIER 1

#define OMPI_HAVE_ATOMIC_CMPSET_32 1

#define OMPI_HAVE_ATOMIC_CMPSET_64 1


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

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

#endif /* OMPI_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline int ompi_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
   int32_t ret;
   
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


static inline int ompi_atomic_cmpset_acq_32(volatile int32_t *addr,
                                           int32_t oldval,
                                           int32_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_32(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_32(volatile int32_t *addr,
                                           int32_t oldval,
                                           int32_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
    int32_t ret;

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


static inline int ompi_atomic_cmpset_acq_64(volatile int64_t *addr,
                                           int64_t oldval,
                                           int64_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_64(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_64(volatile int64_t *addr,
                                           int64_t oldval,
                                           int64_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_64(addr, oldval, newval);
}


#endif /* OMPI_GCC_INLINE_ASSEMBLY */


#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
