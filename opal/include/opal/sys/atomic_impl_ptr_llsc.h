/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Pointer-sized wrapper for LL/SC calls, wrappers around size-defined
 * calls.  Note that these must be macros, as LL/SC may not work
 * across function calls.
 */


#ifndef ATOMIC_IMPL_PTR_LLSC_H
#define ATOMIC_IMPL_PTR_LLSC_H 1

#if SIZEOF_VOID_P == 4 && defined(OPAL_HAVE_ATOMIC_LLSC_32) && OPAL_HAVE_ATOMIC_LLSC_32

#    define opal_atomic_ll_ptr(addr, ret) opal_atomic_ll_32((opal_atomic_int32_t *) (addr), ret)
#    define opal_atomic_sc_ptr(addr, value, ret) \
        opal_atomic_sc_32((opal_atomic_int32_t *) (addr), (intptr_t)(value), ret)

#    define OPAL_HAVE_ATOMIC_LLSC_PTR 1

#elif SIZEOF_VOID_P == 8 && defined(OPAL_HAVE_ATOMIC_LLSC_64) && OPAL_HAVE_ATOMIC_LLSC_64

#    define opal_atomic_ll_ptr(addr, ret) opal_atomic_ll_64((opal_atomic_int64_t *) (addr), ret)
#    define opal_atomic_sc_ptr(addr, value, ret) \
        opal_atomic_sc_64((opal_atomic_int64_t *) (addr), (intptr_t)(value), ret)

#    define OPAL_HAVE_ATOMIC_LLSC_PTR 1

#endif /* SIZEOF_VOID_P == 8 && OPAL_HAVE_ATOMIC_LLSC_64 */

#endif /* ATOMIC_IMPL_PTR_LLSC_H */
