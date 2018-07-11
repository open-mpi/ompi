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

typedef volatile long opal_atomic_long_t;

typedef volatile int32_t opal_atomic_int32_t;
typedef volatile uint32_t opal_atomic_uint32_t;
typedef volatile int64_t opal_atomic_int64_t;
typedef volatile uint64_t opal_atomic_uint64_t;

typedef volatile size_t opal_atomic_size_t;
typedef volatile intptr_t opal_atomic_intptr_t;
typedef volatile uintptr_t opal_atomic_uintptr_t;

#if HAVE_OPAL_INT128_T

typedef volatile opal_int128_t opal_atomic_int128_t;

#endif


#endif /* !defined(OPAL_STDATOMIC_H) */
