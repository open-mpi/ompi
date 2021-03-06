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
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Inline C implementation of the functions defined in atomic.h */

#include "opal/util/opal_assert.h"

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

#if !defined(OPAL_HAVE_ATOMIC_MIN_32)
static inline int32_t opal_atomic_fetch_min_32 (opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = *addr;
    do {
        if (old <= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_32 (addr, &old, value));

    return old;
}

#define OPAL_HAVE_ATOMIC_MIN_32 1

#endif /* OPAL_HAVE_ATOMIC_MIN_32 */

#if !defined(OPAL_HAVE_ATOMIC_MAX_32)
static inline int32_t opal_atomic_fetch_max_32 (opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = *addr;
    do {
        if (old >= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_32 (addr, &old, value));

    return old;
}

#define OPAL_HAVE_ATOMIC_MAX_32 1
#endif /* OPAL_HAVE_ATOMIC_MAX_32 */

#define OPAL_ATOMIC_DEFINE_CMPXCG_OP(type, bits, operation, name)  \
    static inline type opal_atomic_fetch_ ## name ## _ ## bits (opal_atomic_ ## type *addr, type value) \
    {                                                                   \
        type oldval;                                                    \
        do {                                                            \
            oldval = *addr;                                             \
        } while (!opal_atomic_compare_exchange_strong_ ## bits (addr, &oldval, oldval operation value)); \
                                                                        \
        return oldval;                                                  \
    }

#if !defined(OPAL_HAVE_ATOMIC_SWAP_32)
#define OPAL_HAVE_ATOMIC_SWAP_32 1
static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr,
                                          int32_t newval)
{
    int32_t old = *addr;
    do {
    } while (!opal_atomic_compare_exchange_strong_32 (addr, &old, newval));

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

#if !defined(OPAL_HAVE_ATOMIC_MIN_64)
static inline int64_t opal_atomic_fetch_min_64 (opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = *addr;
    do {
        if (old <= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_64 (addr, &old, value));

    return old;
}

#define OPAL_HAVE_ATOMIC_MIN_64 1

#endif /* OPAL_HAVE_ATOMIC_MIN_64 */

#if !defined(OPAL_HAVE_ATOMIC_MAX_64)
static inline int64_t opal_atomic_fetch_max_64 (opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = *addr;
    do {
        if (old >= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_64 (addr, &old, value));

    return old;
}

#define OPAL_HAVE_ATOMIC_MAX_64 1
#endif /* OPAL_HAVE_ATOMIC_MAX_64 */

#if !defined(OPAL_HAVE_ATOMIC_SWAP_64)
#define OPAL_HAVE_ATOMIC_SWAP_64 1
static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr,
                                          int64_t newval)
{
    int64_t old = *addr;
    do {
    } while (!opal_atomic_compare_exchange_strong_64 (addr, &old, newval));

    return old;
}
#endif /* OPAL_HAVE_ATOMIC_SWAP_64 */

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
    opal_atomic_compare_exchange_strong ## semantics ## xx (opal_atomic_intptr_t* addr, intptr_t *oldval, \
                                                            int64_t newval, const size_t length) \
    {                                                                   \
        switch (length) {                                               \
        case 4:                                                         \
            return opal_atomic_compare_exchange_strong_32 ((opal_atomic_int32_t *) addr, \
                                                           (int32_t *) oldval, (int32_t) newval); \
        case 8:                                                         \
            return opal_atomic_compare_exchange_strong_64 ((opal_atomic_int64_t *) addr, \
                                                           (int64_t *) oldval, (int64_t) newval); \
        }                                                               \
        OPAL_ABORT();                                                        \
    }
#elif OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32
#define OPAL_ATOMIC_DEFINE_CMPXCG_XX(semantics)                         \
    static inline bool                                                  \
    opal_atomic_compare_exchange_strong ## semantics ## xx (opal_atomic_intptr_t* addr, intptr_t *oldval, \
                                                            int64_t newval, const size_t length) \
    {                                                                   \
        switch (length) {                                               \
        case 4:                                                         \
            return opal_atomic_compare_exchange_strong_32 ((opal_atomic_int32_t *) addr, \
                                                           (int32_t *) oldval, (int32_t) newval); \
        }                                                               \
        OPAL_ABORT();                                                        \
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
        opal_atomic_compare_exchange_strong ## semantics ## ptr (opal_atomic_intptr_t* addr, intptr_t *oldval, intptr_t newval) \
    {                                                                   \
        return opal_atomic_compare_exchange_strong_32 ((opal_atomic_int32_t *) addr, (int32_t *) oldval, (int32_t) newval); \
    }
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64
#define OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(semantics)                     \
    static inline bool                                                  \
        opal_atomic_compare_exchange_strong ## semantics ## ptr (opal_atomic_intptr_t* addr, intptr_t *oldval, intptr_t newval) \
    {                                                                   \
        return opal_atomic_compare_exchange_strong_64 ((opal_atomic_int64_t *) addr, (int64_t *) oldval, (int64_t) newval); \
    }
#else
#error "Can not define opal_atomic_compare_exchange_strong_ptr with existing atomics"
#endif

OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_)
OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_acq_)
OPAL_ATOMIC_DEFINE_CMPXCG_PTR_XX(_rel_)

#endif /* (OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 || OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_64) */


#if (OPAL_HAVE_ATOMIC_SWAP_32 || OPAL_HAVE_ATOMIC_SWAP_64)

#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_SWAP_32
#define opal_atomic_swap_ptr(addr, value) (intptr_t) opal_atomic_swap_32((opal_atomic_int32_t *) addr, (int32_t) value)
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_SWAP_64
#define opal_atomic_swap_ptr(addr, value) (intptr_t) opal_atomic_swap_64((opal_atomic_int64_t *) addr, (int64_t) value)
#endif

#endif /* (OPAL_HAVE_ATOMIC_SWAP_32 || OPAL_HAVE_ATOMIC_SWAP_64) */

#if OPAL_HAVE_ATOMIC_MATH_32 || OPAL_HAVE_ATOMIC_MATH_64

static inline void
    opal_atomic_add_xx(opal_atomic_intptr_t* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_ADD_32
   case 4:
       (void) opal_atomic_fetch_add_32( (opal_atomic_int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_32 */

#if OPAL_HAVE_ATOMIC_ADD_64
   case 8:
       (void) opal_atomic_fetch_add_64( (opal_atomic_int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_ADD_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a corefile for analysis) */
       OPAL_ABORT();
   }
}


static inline void
opal_atomic_sub_xx(opal_atomic_intptr_t* addr, int32_t value, size_t length)
{
   switch( length ) {
#if OPAL_HAVE_ATOMIC_SUB_32
   case 4:
       (void) opal_atomic_fetch_sub_32( (opal_atomic_int32_t*)addr, (int32_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_SUB_32 */

#if OPAL_HAVE_ATOMIC_SUB_64
   case 8:
       (void) opal_atomic_fetch_sub_64( (opal_atomic_int64_t*)addr, (int64_t)value );
      break;
#endif  /* OPAL_HAVE_ATOMIC_SUB_64 */
   default:
       /* This should never happen, so deliberately abort (hopefully
          leaving a corefile for analysis) */
       OPAL_ABORT();
   }
}

#define OPAL_ATOMIC_DEFINE_OP_FETCH(op, operation, type, ptr_type, suffix) \
    static inline type opal_atomic_ ## op ## _fetch_ ## suffix (opal_atomic_ ## ptr_type *addr, type value) \
    {                                                                   \
        return opal_atomic_fetch_ ## op ## _ ## suffix (addr, value) operation value; \
    }

OPAL_ATOMIC_DEFINE_OP_FETCH(add, +, int32_t, int32_t, 32)
OPAL_ATOMIC_DEFINE_OP_FETCH(and, &, int32_t, int32_t, 32)
OPAL_ATOMIC_DEFINE_OP_FETCH(or, |, int32_t, int32_t, 32)
OPAL_ATOMIC_DEFINE_OP_FETCH(xor, ^, int32_t, int32_t, 32)
OPAL_ATOMIC_DEFINE_OP_FETCH(sub, -, int32_t, int32_t, 32)

static inline int32_t opal_atomic_min_fetch_32 (opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = opal_atomic_fetch_min_32 (addr, value);
    return old <= value ? old : value;
}

static inline int32_t opal_atomic_max_fetch_32 (opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = opal_atomic_fetch_max_32 (addr, value);
    return old >= value ? old : value;
}

#if OPAL_HAVE_ATOMIC_MATH_64
OPAL_ATOMIC_DEFINE_OP_FETCH(add, +, int64_t, int64_t, 64)
OPAL_ATOMIC_DEFINE_OP_FETCH(and, &, int64_t, int64_t, 64)
OPAL_ATOMIC_DEFINE_OP_FETCH(or, |, int64_t, int64_t, 64)
OPAL_ATOMIC_DEFINE_OP_FETCH(xor, ^, int64_t, int64_t, 64)
OPAL_ATOMIC_DEFINE_OP_FETCH(sub, -, int64_t, int64_t, 64)

static inline int64_t opal_atomic_min_fetch_64 (opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = opal_atomic_fetch_min_64 (addr, value);
    return old <= value ? old : value;
}

static inline int64_t opal_atomic_max_fetch_64 (opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = opal_atomic_fetch_max_64 (addr, value);
    return old >= value ? old : value;
}

#endif

static inline intptr_t opal_atomic_fetch_add_ptr( opal_atomic_intptr_t* addr,
                                           void* delta )
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_ADD_32
    return opal_atomic_fetch_add_32((opal_atomic_int32_t*) addr, (unsigned long) delta);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_ADD_64
    return opal_atomic_fetch_add_64((opal_atomic_int64_t*) addr, (unsigned long) delta);
#else
    OPAL_ABORT();
    return 0;
#endif
}

static inline intptr_t opal_atomic_add_fetch_ptr( opal_atomic_intptr_t* addr,
                                           void* delta )
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_ADD_32
    return opal_atomic_add_fetch_32((opal_atomic_int32_t*) addr, (unsigned long) delta);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_ADD_64
    return opal_atomic_add_fetch_64((opal_atomic_int64_t*) addr, (unsigned long) delta);
#else
    OPAL_ABORT();
    return 0;
#endif
}

static inline intptr_t opal_atomic_fetch_sub_ptr( opal_atomic_intptr_t* addr,
                                           void* delta )
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_SUB_32
    return opal_atomic_fetch_sub_32((opal_atomic_int32_t*) addr, (unsigned long) delta);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_SUB_32
    return opal_atomic_fetch_sub_64((opal_atomic_int64_t*) addr, (unsigned long) delta);
#else
    OPAL_ABORT();
    return 0;
#endif
}

static inline intptr_t opal_atomic_sub_fetch_ptr( opal_atomic_intptr_t* addr,
                                           void* delta )
{
#if SIZEOF_VOID_P == 4 && OPAL_HAVE_ATOMIC_SUB_32
    return opal_atomic_sub_fetch_32((opal_atomic_int32_t*) addr, (unsigned long) delta);
#elif SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_SUB_32
    return opal_atomic_sub_fetch_64((opal_atomic_int64_t*) addr, (unsigned long) delta);
#else
    OPAL_ABORT();
    return 0;
#endif
}

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
    int32_t unlocked = OPAL_ATOMIC_LOCK_UNLOCKED;
    bool ret = opal_atomic_compare_exchange_strong_acq_32 (&lock->u.lock, &unlocked, OPAL_ATOMIC_LOCK_LOCKED);
    return (ret == false) ? 1 : 0;
}


static inline void
opal_atomic_lock(opal_atomic_lock_t *lock)
{
    while (opal_atomic_trylock (lock)) {
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
