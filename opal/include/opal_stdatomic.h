/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_STDATOMIC_H)
#    define OPAL_STDATOMIC_H

#    include "opal_stdint.h"

#if OPAL_USE_C11_ATOMICS == 0

typedef volatile int opal_atomic_int_t;
typedef volatile long opal_atomic_long_t;

typedef volatile int32_t opal_atomic_int32_t;
typedef volatile uint32_t opal_atomic_uint32_t;
typedef volatile int64_t opal_atomic_int64_t;
typedef volatile uint64_t opal_atomic_uint64_t;

typedef volatile size_t opal_atomic_size_t;
typedef volatile ssize_t opal_atomic_ssize_t;
typedef volatile intptr_t opal_atomic_intptr_t;
typedef volatile uintptr_t opal_atomic_uintptr_t;

typedef opal_atomic_int32_t opal_atomic_lock_t;

enum { OPAL_ATOMIC_LOCK_UNLOCKED = 0,
       OPAL_ATOMIC_LOCK_LOCKED = 1 };

#    define OPAL_ATOMIC_LOCK_INIT OPAL_ATOMIC_LOCK_UNLOCKED

#else /* OPAL_USE_C11_ATOMICS == 0 */

#    include <stdatomic.h>

/* Wrap _Atomic types in structs to prevent direct access and enforce 
 * use of accessor functions with memory ordering control */
typedef struct { atomic_int value; } opal_atomic_int_t;
typedef struct { atomic_long value; } opal_atomic_long_t;

typedef struct { _Atomic int32_t value; } opal_atomic_int32_t;
typedef struct { _Atomic uint32_t value; } opal_atomic_uint32_t;
typedef struct { _Atomic int64_t value; } opal_atomic_int64_t;
typedef struct { _Atomic uint64_t value; } opal_atomic_uint64_t;

typedef struct { _Atomic size_t value; } opal_atomic_size_t;
typedef struct { _Atomic ssize_t value; } opal_atomic_ssize_t;
typedef struct { _Atomic intptr_t value; } opal_atomic_intptr_t;
typedef struct { _Atomic uintptr_t value; } opal_atomic_uintptr_t;

typedef atomic_flag opal_atomic_lock_t;

#    define OPAL_ATOMIC_LOCK_UNLOCKED false
#    define OPAL_ATOMIC_LOCK_LOCKED   true

#    define OPAL_ATOMIC_LOCK_INIT ATOMIC_FLAG_INIT

/* Load and store accessor functions with memory ordering control.
 * Default to relaxed ordering to avoid sequential consistency overhead. */

/* Load functions with memory ordering */
static inline int opal_atomic_load_int(const opal_atomic_int_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline long opal_atomic_load_long(const opal_atomic_long_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline int32_t opal_atomic_load_32(const opal_atomic_int32_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline uint32_t opal_atomic_load_uint32(const opal_atomic_uint32_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline int64_t opal_atomic_load_64(const opal_atomic_int64_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline uint64_t opal_atomic_load_uint64(const opal_atomic_uint64_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline size_t opal_atomic_load_size_t(const opal_atomic_size_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline ssize_t opal_atomic_load_ssize_t(const opal_atomic_ssize_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline intptr_t opal_atomic_load_ptr(const opal_atomic_intptr_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

static inline uintptr_t opal_atomic_load_uptr(const opal_atomic_uintptr_t *addr, memory_order order)
{
    return atomic_load_explicit(&addr->value, order);
}

/* Store functions with memory ordering */
static inline void opal_atomic_store_int(opal_atomic_int_t *addr, int value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_long(opal_atomic_long_t *addr, long value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_32(opal_atomic_int32_t *addr, int32_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_uint32(opal_atomic_uint32_t *addr, uint32_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_64(opal_atomic_int64_t *addr, int64_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_uint64(opal_atomic_uint64_t *addr, uint64_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_size_t(opal_atomic_size_t *addr, size_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_ssize_t(opal_atomic_ssize_t *addr, ssize_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_ptr(opal_atomic_intptr_t *addr, intptr_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

static inline void opal_atomic_store_uptr(opal_atomic_uintptr_t *addr, uintptr_t value, memory_order order)
{
    atomic_store_explicit(&addr->value, value, order);
}

/* Convenience functions with default relaxed ordering */
static inline int32_t opal_atomic_load_32_relaxed(const opal_atomic_int32_t *addr)
{
    return opal_atomic_load_32(addr, memory_order_relaxed);
}

static inline int64_t opal_atomic_load_64_relaxed(const opal_atomic_int64_t *addr)
{
    return opal_atomic_load_64(addr, memory_order_relaxed);
}

static inline intptr_t opal_atomic_load_ptr_relaxed(const opal_atomic_intptr_t *addr)
{
    return opal_atomic_load_ptr(addr, memory_order_relaxed);
}

static inline void opal_atomic_store_32_relaxed(opal_atomic_int32_t *addr, int32_t value)
{
    opal_atomic_store_32(addr, value, memory_order_relaxed);
}

static inline void opal_atomic_store_64_relaxed(opal_atomic_int64_t *addr, int64_t value)
{
    opal_atomic_store_64(addr, value, memory_order_relaxed);
}

static inline void opal_atomic_store_ptr_relaxed(opal_atomic_intptr_t *addr, intptr_t value)
{
    opal_atomic_store_ptr(addr, value, memory_order_relaxed);
}

static inline size_t opal_atomic_load_size_t_relaxed(const opal_atomic_size_t *addr)
{
    return opal_atomic_load_size_t(addr, memory_order_relaxed);
}

static inline void opal_atomic_store_size_t_relaxed(opal_atomic_size_t *addr, size_t value)
{
    opal_atomic_store_size_t(addr, value, memory_order_relaxed);
}

#    endif /* OPAL_USE_C11_ATOMICS == 0 */

#    if HAVE_OPAL_INT128_T

#        if OPAL_USE_C11_ATOMICS && OPAL_HAVE_C11_CSWAP_INT128

typedef struct { _Atomic opal_int128_t value; } opal_atomic_int128_t;

#        else

typedef volatile opal_int128_t opal_atomic_int128_t __opal_attribute_aligned__(16);

#        endif

#    endif

#endif /* !defined(OPAL_STDATOMIC_H) */
