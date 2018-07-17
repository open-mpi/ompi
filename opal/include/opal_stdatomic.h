/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_STDATOMIC_H)
#define OPAL_STDATOMIC_H

#include "opal_stdint.h"

#if OPAL_ASSEMBLY_BUILTIN != OPAL_BUILTIN_C11

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

#else /* OPAL_HAVE_C__ATOMIC */

#include <stdatomic.h>

typedef atomic_int opal_atomic_int_t;
typedef atomic_long opal_atomic_long_t;

typedef _Atomic int32_t opal_atomic_int32_t;
typedef _Atomic uint32_t opal_atomic_uint32_t;
typedef _Atomic int64_t opal_atomic_int64_t;
typedef _Atomic uint64_t opal_atomic_uint64_t;

typedef _Atomic size_t opal_atomic_size_t;
typedef _Atomic ssize_t opal_atomic_ssize_t;
typedef _Atomic intptr_t opal_atomic_intptr_t;
typedef _Atomic uintptr_t opal_atomic_uintptr_t;

#endif /* OPAL_HAVE_C__ATOMIC */

#if HAVE_OPAL_INT128_T

/* do not use C11 atomics for __int128 if they are not lock free */
#if OPAL_HAVE_C11_CSWAP_INT128

typedef _Atomic opal_int128_t opal_atomic_int128_t;

#else

typedef volatile opal_int128_t opal_atomic_int128_t;

#endif

#endif

#endif /* !defined(OPAL_STDATOMIC_H) */
