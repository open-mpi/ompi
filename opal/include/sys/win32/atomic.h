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

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

static inline void opal_atomic_mb(void)
{    
#if 0
   return KeMemoryBarrier();
#endif
}


static inline void opal_atomic_rmb(void)
{
#if 0
   return KeMemoryBarrier();
#endif
}


static inline void opal_atomic_wmb(void)
{    
#if 0
   return KeMemoryBarrier();
#endif
}


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define OPAL_HAVE_ATOMIC_CMPSET_32 1
static inline int opal_atomic_cmpset_acq_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
#if 0
   int32_t ret = InterlockedCompareExchangeAcquire ((int32_t volatile*) addr,
                                                    (int32_t) newval, (int32_t) oldval);
   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}


static inline int opal_atomic_cmpset_rel_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
#if 0
   int32_t ret = InterlockedCompareExchangeRelease ((int32_t volatile*) addr,
                                                    (int32_t) newval, (int32_t) oldval);
   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}

static inline int opal_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
#if 0
   int32_t ret = InterlockedCompareExchange ((int32_t volatile*) addr,
                                             (int32_t) newval, (int32_t) oldval);

   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}

#define OPAL_HAVE_ATOMIC_CMPSET_64 1
static inline int opal_atomic_cmpset_acq_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
#if 0
   int64_t ret = InterlockedCompareExchangeAcquire64 ((int64_t volatile*) addr,
                                                      (int64_t) newval, (int64_t) oldval);
   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}

static inline int opal_atomic_cmpset_rel_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
#if 0
   int64_t ret = InterlockedCompareExchangeRelease64 ((int64_t volatile*) addr,
                                                      (int64_t) newval, (int64_t) oldval);
   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}


static inline int opal_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
#if 0
   int64_t ret = InterlockedCompareExchange64 ((int64_t volatile*) addr,
                                                (int64_t) newval, (int64_t) oldval);
   return (oldval == ret) ? 1: 0;
#else
   return 0;
#endif
}

#define OPAL_HAVE_ATOMIC_MATH_32 1

#define OPAL_HAVE_ATOMIC_ADD_32 1
static inline int32_t opal_atomic_add_32(volatile int32_t *addr, int32_t delta)
{
   return InterlockedExchangeAdd ((LONG volatile *) addr,
                                  (int32_t) delta);
   
}

#define OPAL_HAVE_ATOMIC_MATH_64 1

#define OPAL_HAVE_ATOMIC_ADD_64 1
static inline int64_t opal_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
#if 0
   return InterlockedExchangeAdd64 ((int64_t volatile *) addr,
                                    (int64_t) delta);
#else
   return 0;
#endif
   
}

#define OPAL_HAVE_ATOMIC_SUB_32 1
static inline int32_t opal_atomic_sub_32(volatile int32_t *addr, int32_t delta)
{
   return InterlockedExchangeAdd( (LONG volatile *) addr,
                                  (int32_t) (-delta));

}

#define OPAL_HAVE_ATOMIC_SUB_64 1
static inline int64_t opal_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
#if 0
   return InterlockedExchangeAdd64 ((int64_t volatile *) addr,
                                    (int64_t) (-delta));
#else
   return 0;
#endif

}
#endif /* ! OMPI_SYS_ARCH_ATOMIC_H */
