/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file
 *
 * Compiler-specific prefetch functions
 *
 * A small set of prefetch / prediction interfaces for using compiler
 * directives to improve memory prefetching and branch prediction
 */

#ifndef PRTE_PREFETCH_H
#define PRTE_PREFETCH_H

#include "prte_config.h"

/* C code */

#if PRTE_C_HAVE_BUILTIN_EXPECT
#    define PMIX_LIKELY(expression)   __builtin_expect(!!(expression), 1)
#    define PMIX_UNLIKELY(expression) __builtin_expect(!!(expression), 0)
#else
#    define PMIX_LIKELY(expression)   (expression)
#    define PMIX_UNLIKELY(expression) (expression)
#endif

#if PRTE_C_HAVE_BUILTIN_PREFETCH
#    define PRTE_PREFETCH(address, rw, locality) __builtin_prefetch(address, rw, locality)
#else
#    define PRTE_PREFETCH(address, rw, locality)
#endif

#endif
