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

#ifdef HAVE_SMP

#define MB()  __asm__ __volatile__ ("sync" : : : "memory")
#define RMB() __asm__ __volatile__ ("lwsync" : : : "memory")
#define WMB() __asm__ __volatile__ ("eieio" : : : "memory")
#define SMP_SYNC  "sync \n\t"
#define SMP_ISYNC "\n\tisync"

#else

#define MB()
#define RMB()
#define WMB()
#define SMP_SYNC  ""
#define SMP_ISYNC

#endif


/**********************************************************************
 *
 * Define constants for PowerPC 32
 *
 *********************************************************************/
#define OMPI_HAVE_ATOMIC_MEM_BARRIER 1

#define OMPI_HAVE_ATOMIC_CMPSET_32 1

#define OMPI_HAVE_ATOMIC_MATH_32 1
#define OMPI_HAVE_ATOMIC_ADD_32 1
#define OMPI_HAVE_ATOMIC_SUB_32 1

#if (OMPI_ASSEMBLY_ARCH == OMPI_POWERPC64) || (OMPI_POWERPC_SUPPORT_64BIT && OMPI_GCC_INLINE_ASSEMBLY)
#define OMPI_HAVE_ATOMIC_CMPSET_64 1
#endif


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

#elif OMPI_XLC_INLINE_ASSEMBLY /* end OMPI_GCC_INLINE_ASSEMBLY */

/* Yeah, I don't know who thought this was a reasonable syntax for
 * inline assembly.  Do these because they are used so often and they
 * are fairly simple (aka: there is a tech pub on IBM's web site
 * containing the right hex for the instructions).
 */

void ompi_atomic_mb(void);
#pragma mc_func ompi_atomic_mb { "7c0004ac" }          /* sync  */
#pragma reg_killed_by ompi_atomic_mb                   /* none */

void ompi_atomic_rmb(void);
#pragma mc_func ompi_atomic_rmb { "7c2004ac" }         /* lwsync  */
#pragma reg_killed_by ompi_atomic_rmb                  /* none */

void ompi_atomic_wmb(void);
#pragma mc_func ompi_atomic_wmb { "7c0006ac" }         /* eieio */
#pragma reg_killed_by ompi_atomic_wmb                  /* none */

#else /* end OMPI_XLC_INLINE_ASSEMBLY */

void ompi_atomic_mb(void);
void ompi_atomic_rmb(void);
void ompi_atomic_wmb(void);

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

   __asm__ __volatile__ (
                         "1: lwarx   %0, 0, %2  \n\t"
                         "   cmpw    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stwcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         SMP_SYNC
                         "2:"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");

   return (ret == oldval);
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

#else
int ompi_atomic_cmpset_32(volatile int32_t *addr,
                          int32_t oldval, int32_t newval);
int ompi_atomic_cmpset_acq_32(volatile int32_t *addr,
                              int32_t oldval, int32_t newval);
int ompi_atomic_cmpset_rel_32(volatile int32_t *addr,
                              int32_t oldval, int32_t newval);
#endif /* OMPI_GCC_INLINE_ASSEMBLY */


#if OMPI_POWERPC_SUPPORT_64BIT

#if  OMPI_GCC_INLINE_ASSEMBLY
static inline int ompi_atomic_cmpset_64(volatile int64_t *addr,
                                        int64_t oldval, int64_t newval)
{
   int64_t ret;

   __asm__ __volatile__ (
                         "1: ldarx   %0, 0, %2  \n\t"
                         "   cmpd    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stdcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r" (ret), "=m" (*addr)
                         : "r" (addr), "r" (oldval), "r" (newval), "m" (*addr)
                         : "cc", "memory");
    
   return (ret == oldval);
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

#elif OMPI_ASSEMBLY_ARCH == OMPI_POWERPC64
/* currently, don't have 64 bit apps for non-inline assembly */

int ompi_atomic_cmpset_64(volatile int64_t *addr,
                          int64_t oldval, int64_t newval);
int ompi_atomic_cmpset_acq_64(volatile int64_t *addr,
                              int64_t oldval, int64_t newval);
int ompi_atomic_cmpset_rel_64(volatile int64_t *addr,
                              int64_t oldval, int64_t newval);

#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* OMPI_POWERPC_SUPPORT_64BIT */


#if OMPI_GCC_INLINE_ASSEMBLY

static inline int32_t ompi_atomic_add_32(volatile int32_t* v, int inc)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx %0, 0, %3 \n\t"
                        "     add  %0, %2, %0                \n\t"
                        "     stwcx.   %0, 0, %3              \n\t"
                        "     bne-  1b                      \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (inc), "r" (v), "m" (*v)
                        : "cc");

   return *v;
}


static inline int32_t ompi_atomic_sub_32(volatile int32_t* v, int dec)
{
   int32_t t;

   __asm__ __volatile__(
                        "1:   lwarx %0,0,%3\n\t"
                        "     subf  %0,%2,%0                \n\t"
                        "     stwcx.   %0,0,%3              \n\t"
                        "     bne-  1b                      \n\t"
                        : "=&r" (t), "=m" (*v)
                        : "r" (dec), "r" (v), "m" (*v)
                        : "cc");

   return *v;
}


#endif /* OMPI_GCC_INLINE_ASSEMBLY */

#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
