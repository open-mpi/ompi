/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_SYS_ARCH_ATOMIC_H
#define OMPI_SYS_ARCH_ATOMIC_H 1

/*
 * On powerpc ...
 */

#if OMPI_WANT_SMP_LOCKS

#define MB() 
#define RMB()
#define WMB()
#define SMP_SYNC "sync"

#else

#define MB()
#define RMB()
#define WMB()
#define SMP_SYNC  ""

#endif


/**********************************************************************
 *
 * Define constants for MIPS
 *
 *********************************************************************/
#define OMPI_HAVE_ATOMIC_MEM_BARRIER 0

#define OMPI_HAVE_ATOMIC_CMPSET_32 0
#define OMPI_HAVE_ATOMIC_CMPSET_64 0


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline
void ompi_atomic_mb(void)
{
    MB();
}


static inline
void ompi_atomic_rmb(void)
{
    RMB();
}


static inline
void ompi_atomic_wmb(void)
{
    WMB();
}

#endif

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OMPI_GCC_INLINE_ASSEMBLY

static inline int ompi_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
    int32_t ret;

   __asm__ __volatile__ ("\t"
                         ".set noreorder        \n"
                         "retry:                \n\t"
                         "ll     %0, %2         \n\t" /* load *addr into ret */
                         "bne    %0, %3, done   \n\t" /* done if oldval != ret */
                         "or     %0, 0, %4      \n\t" /* ret = newval */
                         "sc     %0, %2         \n\t" /* store ret in *addr */
                         /* note: ret will be 0 if failed, 1 if succeeded */
                         "done:                 \n\t"
                         ".set reorder          \n"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");
   return (int) ret;
}


/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_32 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int ompi_atomic_cmpset_acq_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_32(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_32(volatile int32_t *addr,
                                            int32_t oldval, int32_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_32(addr, oldval, newval);
}


static inline int ompi_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
    int64_t ret;

   __asm__ __volatile__ ("\t"
                         ".set noreorder        \n"
                         "retry:                \n\t"
                         "ll     %0, %2         \n\t" /* load *addr into ret */
                         "bne    %0, %3, done   \n\t" /* done if oldval != ret */
                         "or     %0, 0, %4      \n\t" /* ret = newval */
                         "sc     %0, %2         \n\t" /* store ret in *addr */
                         /* note: ret will be 0 if failed, 1 if succeeded */
                         "done:                 \n\t"
                         ".set reorder          \n"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");
   return (int) ret;
}


/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_64 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline int ompi_atomic_cmpset_acq_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    int rc;

    rc = ompi_atomic_cmpset_64(addr, oldval, newval);
    ompi_atomic_rmb();

    return rc;
}


static inline int ompi_atomic_cmpset_rel_64(volatile int64_t *addr,
                                            int64_t oldval, int64_t newval)
{
    ompi_atomic_wmb();
    return ompi_atomic_cmpset_64(addr, oldval, newval);
}

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
