/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Inline C implementation of the functions defined in atomic.h */

#include <stdlib.h>

/**********************************************************************
 *
 * Atomic math operations
 *
 * All the architectures provide a compare_and_set atomic operations. If
 * they dont provide atomic additions and/or substractions then we can
 * define these operations using the atomic compare_and_set.
 *
 * Some architectures do not provide support for the 64 bits
 * atomic operations. Until we find a better solution let's just
 * undefine all those functions if there is no 64 bit compare-exchange
 *
 *********************************************************************/
#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32

#define OPAL_ATOMIC_DEFINE_CMPXCG_OP(type, bits, operand, name)  \
    static inline type opal_atomic_ ## name ## _ ## bits (volatile type *addr, type value) \
    {                                                                   \
        type oldval, newval;                                            \
        do {                                                            \
            oldval = *addr;                                             \
            newval = oldval operand value;                              \
        } while (!opal_atomic_compare_exchange_strong_ ## bits (addr, &oldval, newval)); \
                                                                        \
        return newval;                                                  \
    }

#if !defined(OPAL_HAVE_ATOMIC_SWAP_32)
#define OPAL_HAVE_ATOMIC_SWAP_32 1
static inline int32_t opal_atomic_swap_32(volatile int32_t *addr,
                                          int32_t newval)
{
    int32_t old;
    do {
        old = *addr;
    } while (!opal_atomic_bool_cmpset_32(addr, old, newval));

    return old;
}
#endif /* OPAL_HAVE_ATOMIC_SWAP_32 */

#if !defined(OPAL_HAVE_ATOMIC_ADD_32)
#define OPAL_HAVE_ATOMIC_ADD_32 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int32_t, 32, +, add)

#endif  /* OPAL_HAVE_ATOMIC_ADD_32 */

#if !defined(OPAL_HAVE_ATOMIC_AND_32)
#define OPAL_HAVE_ATOMIC_AND_32 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int32_t, 32, &, and)

#endif  /* OPAL_HAVE_ATOMIC_AND_32 */

#if !defined(OPAL_HAVE_ATOMIC_OR_32)
#define OPAL_HAVE_ATOMIC_OR_32 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int32_t, 32, |, or)

#endif  /* OPAL_HAVE_ATOMIC_OR_32 */

#if !defined(OPAL_HAVE_ATOMIC_XOR_32)
#define OPAL_HAVE_ATOMIC_XOR_32 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int32_t, 32, ^, xor)

#endif  /* OPAL_HAVE_ATOMIC_XOR_32 */


#if !defined(OPAL_HAVE_ATOMIC_SUB_32)
#define OPAL_HAVE_ATOMIC_SUB_32 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int32_t, 32, -, sub)

#endif  /* OPAL_HAVE_ATOMIC_SUB_32 */

#endif /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */


#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64

#if !defined(OPAL_HAVE_ATOMIC_SWAP_64)
#define OPAL_HAVE_ATOMIC_SWAP_64 1
static inline int64_t opal_atomic_swap_64(volatile int64_t *addr,
                                          int64_t newval)
{
    int64_t old;
    do {
        old = *addr;
    } while (!opal_atomic_bool_cmpset_64(addr, old, newval));
    return old;
}
#endif /* OPAL_HAVE_ATOMIC_SWAP_32 */

#if !defined(OPAL_HAVE_ATOMIC_ADD_64)
#define OPAL_HAVE_ATOMIC_ADD_64 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int64_t, 64, +, add)

#endif  /* OPAL_HAVE_ATOMIC_ADD_64 */

#if !defined(OPAL_HAVE_ATOMIC_AND_64)
#define OPAL_HAVE_ATOMIC_AND_64 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int64_t, 64, &, and)

#endif  /* OPAL_HAVE_ATOMIC_AND_64 */

#if !defined(OPAL_HAVE_ATOMIC_OR_64)
#define OPAL_HAVE_ATOMIC_OR_64 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int64_t, 64, |, or)

#endif  /* OPAL_HAVE_ATOMIC_OR_64 */

#if !defined(OPAL_HAVE_ATOMIC_XOR_64)
#define OPAL_HAVE_ATOMIC_XOR_64 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int64_t, 64, ^, xor)

#endif  /* OPAL_HAVE_ATOMIC_XOR_64 */

#if !defined(OPAL_HAVE_ATOMIC_SUB_64)
#define OPAL_HAVE_ATOMIC_SUB_64 1

OPAL_ATOMIC_DEFINE_CMPXCG_OP(int64_t, 64, -, sub)

#endif  /* OPAL_HAVE_ATOMIC_SUB_64 */

#else

#if !defined(OPAL_HAVE_ATOMIC_ADD_64)
#define OPAL_HAVE_ATOMIC_ADD_64 0
#endif

#if !defined(OPAL_HAVE_ATOMIC_SUB_64)
#define OPAL_HAVE_ATOMIC_SUB_64 0
#endif

#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64 */

#if (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 || OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64)

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
#define OPAL_ATOMIC_DEFINE_CMPXCG_XX(semantics)                         \
    static inline bool                                                  \
    opal_atomic_compare_exchange_strong ## semantics ## xx (volatile void* addr, void *oldval, \
                                                            int64_t newval, const size_t length) \
    {                                                                   \
        switch (length) {                                               \
        case 4:                                                         \
            return opal_atomic_compare_exchange_strong_32 ((volatile int32_t *) addr, \
                                                           (int32_t *) oldval, (int32_t) newval); \
        case 8:                                                         \
            return opal_atomic_compare_exchange_strong_64 ((volatile int64_t *) addr, \
                                                           (int64_t *) oldval, (int64_t) newval); \
        }                                                               \
        abort();                                                        \
    }
#elif OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
#define OPAL_ATOMIC_DEFINE_CMPXCG_XX(semantics)                         \
    static inline bool                                                  \
    opal_atomic_compare_exchange_strong ## semantics ## xx (volatile void* addr, void *oldval, \
                                                            int64_t newval, const size_t length) \
    {                                                                   \
        switch (length) {                                               \
        case 4:                                                         \
            return opal_atomic_compare_exchange_strong_32 ((volatile int32_t *) addr, \
                                                           (int32_t *) oldval, (int32_t) newval); \
        abort();                                                        \
    }
#else
#error "Platform does not have required atomic compare-and-swap functionality"
#endif

OPAL_ATOMIC_DEFINE_CMPXCG_XX(_)
OPAL_ATOMIC_DEFINE_CMPXCG_XX(_acq_)
OPAL_ATOMIC_DEFINE_CMPXCG_XX(_rel_)

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
#define OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(semantics)                     \
    static inline bool                                                  \
        opal_atomic_compare_exchange_strong ## semantics ## ptr (volatile void* addr, void *oldval, void *newval) \
    {                                                                   \
        return opal_atomic_compare_exchange_strong_32 ((volatile int32_t *) addr, (int32_t *) oldval, (int32_t) newval); \
    }
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
#define OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(semantics)                     \
    static inline bool                                                  \
        opal_atomic_compare_exchange_strong ## semantics ## ptr (volatile void* addr, void *oldval, void *newval) \
    {                                                                   \
        return opal_atomic_compare_exchange_strong_64 ((volatile int64_t *) addr, (int64_t *) oldval, (int64_t) newval); \
    }
#else
#error "Can not define opal_atomic_compare_exchange_strong_ptr with existing atomics"
#endif

OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_)
OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_acq_)
OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_rel_)

#endif /* (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 || OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64) */


/* XXX -- DEPRECATED -- XXX -- Define legacy cmpset functions */
#if (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32)
static inline bool opal_atomic_bool_cmpset_32 (volatile int32_t *addr, int32_t oldval,
                                               int32_t newval)
{
    return opal_atomic_compare_exchange_strong_32 (addr, &oldval, newval);
}

static inline bool opal_atomic_bool_cmpset_acq_32 (volatile int32_t *addr, int32_t oldval,
                                                   int32_t newval)
{
    return opal_atomic_compare_exchange_strong_acq_32 (addr, &oldval, newval);
}

static inline bool opal_atomic_bool_cmpset_rel_32 (volatile int32_t *addr, int32_t oldval,
                                                   int32_t newval)
{
    return opal_atomic_compare_exchange_strong_rel_32 (addr, &oldval, newval);
}
#endif

#if (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64)
static inline bool opal_atomic_bool_cmpset_64 (volatile int64_t *addr, int64_t oldval,
                                               int64_t newval)
{
    return opal_atomic_compare_exchange_strong_64 (addr, &oldval, newval);
}

static inline bool opal_atomic_bool_cmpset_acq_64 (volatile int64_t *addr, int64_t oldval,
                                                   int64_t newval)
{
    return opal_atomic_compare_exchange_strong_acq_64 (addr, &oldval, newval);
}

static inline bool opal_atomic_bool_cmpset_rel_64 (volatile int64_t *addr, int64_t oldval,
                                                   int64_t newval)
{
    return opal_atomic_compare_exchange_strong_rel_64 (addr, &oldval, newval);
}
#endif

#if (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128)
static inline bool opal_atomic_bool_cmpset_128 (volatile opal_int128_t *addr, opal_int128_t oldval,
                                                opal_int128_t newval)
{
    return opal_atomic_compare_exchange_strong_128 (addr, &oldval, newval);
}
#endif

#if (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 || OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64)

static inline bool
opal_atomic_bool_cmpset_xx(volatile void* addr, int64_t oldval,
                           int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
   case 4:
      return opal_atomic_bool_cmpset_32( (volatile int32_t*)addr,
                                    (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
   case 8:
      return opal_atomic_bool_cmpset_64( (volatile int64_t*)addr,
                                    (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64 */
   }
   abort();
   /* This should never happen, so deliberately abort (hopefully
      leaving a corefile for analysis) */
}


static inline bool
opal_atomic_bool_cmpset_acq_xx(volatile void* addr, int64_t oldval,
                               int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
   case 4:
      return opal_atomic_bool_cmpset_acq_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
   case 8:
      return opal_atomic_bool_cmpset_acq_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64 */
   }
   /* This should never happen, so deliberately abort (hopefully
      leaving a corefile for analysis) */
   abort();
}


static inline bool
opal_atomic_bool_cmpset_rel_xx(volatile void* addr, int64_t oldval,
                               int64_t newval, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
   case 4:
      return opal_atomic_bool_cmpset_rel_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */

#if OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
   case 8:
      return opal_atomic_bool_cmpset_rel_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64 */
   }
   /* This should never happen, so deliberately abort (hopefully
      leaving a corefile for analysis) */
   abort();
}


static inline bool
opal_atomic_bool_cmpset_ptr(volatile void* addr,
                            void* oldval,
                            void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
    return opal_atomic_bool_cmpset_32((int32_t*) addr, (unsigned long) oldval,
                                 (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
    return opal_atomic_bool_cmpset_64((int64_t*) addr, (unsigned long) oldval,
                                 (unsigned long) newval);
#else
    abort();
#endif
}


static inline bool
opal_atomic_bool_cmpset_acq_ptr(volatile void* addr,
                                void* oldval,
                                void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
    return opal_atomic_bool_cmpset_acq_32((int32_t*) addr, (unsigned long) oldval,
                                     (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
    return opal_atomic_bool_cmpset_acq_64((int64_t*) addr, (unsigned long) oldval,
                                     (unsigned long) newval);
#else
    abort();
#endif
}


static inline bool opal_atomic_bool_cmpset_rel_ptr(volatile void* addr,
                                                   void* oldval,
                                                   void* newval)
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
    return opal_atomic_bool_cmpset_rel_32((int32_t*) addr, (unsigned long) oldval,
                                     (unsigned long) newval);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
    return opal_atomic_bool_cmpset_rel_64((int64_t*) addr, (unsigned long) oldval,
                                     (unsigned long) newval);
#else
    abort();
#endif
}

#endif /* (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 || OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64) */

#if (OPAL_HAVE_ATOMIC_SWAP_32 || OPAL_HAVE_ATOMIC_SWAP_64)

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_SWAP_32
#define opal_atomic_swap_ptr(addr, value) (void *) opal_atomic_swap_32((int32_t *) addr, (int32_t) value)
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_SWAP_64
#define opal_atomic_swap_ptr(addr, value) (void *) opal_atomic_swap_64((int64_t *) addr, (int64_t) value)
#endif

#endif /* (OPAL_HAVE_ATOMIC_SWAP_32 || OPAL_HAVE_ATOMIC_SWAP_64) */

#if (OPAL_HAVE_ATOMIC_LLSC_32 || OPAL_HAVE_ATOMIC_LLSC_64)

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_LLSC_32

#define opal_atomic_ll_ptr(addr) (void *) opal_atomic_ll_32((int32_t *) addr)
#define opal_atomic_sc_ptr(addr, newval) opal_atomic_sc_32((int32_t *) addr, (int32_t) newval)

#define OPAL_HAVE_ATOMIC_LLSC_PTR 1

#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_LLSC_64

#define opal_atomic_ll_ptr(addr) (void *) opal_atomic_ll_64((int64_t *) addr)
#define opal_atomic_sc_ptr(addr, newval) opal_atomic_sc_64((int64_t *) addr, (int64_t) newval)

#define OPAL_HAVE_ATOMIC_LLSC_PTR 1

#endif

#endif /* (OPAL_HAVE_ATOMIC_LLSC_32 || OPAL_HAVE_ATOMIC_LLSC_64)*/

#if !defined(OPAL_HAVE_ATOMIC_LLSC_PTR)
#define OPAL_HAVE_ATOMIC_LLSC_PTR 0
#endif

#if OPAL_HAVE_ATOMIC_MATH_32 || OPAL_HAVE_ATOMIC_MATH_64


static inline void
opal_atomic_add_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_ADD_32
   case 4:
      opal_atomic_add_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */

#if OPAL_HAVE_ATOMIC_ADD_64
   case 8:
      opal_atomic_add_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_ADD_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a corefile for analysis) */
       abort();
   }
}


static inline void
opal_atomic_sub_xx(volatile void* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_SUB_32
   case 4:
      opal_atomic_sub_32( (volatile int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_SUB_32 */

#if OPAL_HAVE_ATOMIC_SUB_64
   case 8:
      opal_atomic_sub_64( (volatile int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_SUB_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a corefile for analysis) */
       abort();
   }
}

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_ADD_32
static inline int32_t opal_atomic_add_ptr( volatile void* addr,
                                           void* delta )
{
    return opal_atomic_add_32((int32_t*) addr, (unsigned long) delta);
}
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_ADD_64
static inline int64_t opal_atomic_add_ptr( volatile void* addr,
                                           void* delta )
{
    return opal_atomic_add_64((int64_t*) addr, (unsigned long) delta);
}
#else
static inline int32_t opal_atomic_add_ptr( volatile void* addr,
                                           void* delta )
{
    abort();
    return 0;
}
#endif

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_SUB_32
static inline int32_t opal_atomic_sub_ptr( volatile void* addr,
                                           void* delta )
{
    return opal_atomic_sub_32((int32_t*) addr, (unsigned long) delta);
}
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_SUB_32
static inline int64_t opal_atomic_sub_ptr( volatile void* addr,
                                           void* delta )
{
    return opal_atomic_sub_64((int64_t*) addr, (unsigned long) delta);
}
#else
static inline int32_t opal_atomic_sub_ptr( volatile void* addr,
                                           void* delta )
{
    abort();
    return 0;
}
#endif

#endif /* OPAL_HAVE_ATOMIC_MATH_32 || OPAL_HAVE_ATOMIC_MATH_64 */

/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/
#ifdef OPAL_NEED_INLINE_ATOMIC_SPINLOCKS

/*
 * Lock initialization function. It set the lock to UNLOCKED.
 */
static inline void
opal_atomic_lock_init( opal_atomic_lock_t* lock, int32_t value )
{
   lock->u.lock = value;
}


static inline int
opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    bool ret = opal_atomic_bool_cmpset_acq_32( &(lock->u.lock),
                                               OPAL_ATOMIC_LOCK_UNLOCKED, OPAL_ATOMIC_LOCK_LOCKED);
    return (ret == 0) ? 1 : 0;
}


static inline void
opal_atomic_lock(opal_atomic_lock_t *lock)
{
   while( !opal_atomic_bool_cmpset_acq_32( &(lock->u.lock),
                                      OPAL_ATOMIC_LOCK_UNLOCKED, OPAL_ATOMIC_LOCK_LOCKED) ) {
      while (lock->u.lock == OPAL_ATOMIC_LOCK_LOCKED) {
         /* spin */ ;
      }
   }
}


static inline void
opal_atomic_unlock(opal_atomic_lock_t *lock)
{
   opal_atomic_wmb();
   lock->u.lock=OPAL_ATOMIC_LOCK_UNLOCKED;
}

#endif /* OPAL_HAVE_ATOMIC_SPINLOCKS */
