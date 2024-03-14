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

#ifndef ATOMIC_IMPL_PTR_CSWAP_H
#define ATOMIC_IMPL_PTR_CSWAP_H 1

static inline bool opal_atomic_compare_exchange_strong_ptr(opal_atomic_intptr_t *addr,
                                                           intptr_t *oldval, intptr_t newval)
{
#if SIZEOF_VOID_P == 4
    return opal_atomic_compare_exchange_strong_32((opal_atomic_int32_t*)addr,
                                                  (int32_t*)oldval,
                                                  (int32_t)newval);
#elif SIZEOF_VOID_P == 8
    return opal_atomic_compare_exchange_strong_64((opal_atomic_int64_t*)addr,
                                                  (int64_t*)oldval,
                                                  (int64_t)newval);
#else
#error "No implementation of opal_atomic_compare_exchange_strong_ptr"
#endif
}

static inline bool opal_atomic_compare_exchange_strong_acq_ptr(opal_atomic_intptr_t *addr,
                                                               intptr_t *oldval, intptr_t newval)
{
#if SIZEOF_VOID_P == 4
    return opal_atomic_compare_exchange_strong_acq_32((opal_atomic_int32_t*)addr,
                                                      (int32_t*)oldval,
                                                      (int32_t)newval);
#elif SIZEOF_VOID_P == 8
    return opal_atomic_compare_exchange_strong_acq_64((opal_atomic_int64_t*)addr,
                                                      (int64_t*)oldval,
                                                      (int64_t)newval);
#else
#error "No implementation of opal_atomic_compare_exchange_strong_acq_ptr"
#endif
}

static inline bool opal_atomic_compare_exchange_strong_rel_ptr(opal_atomic_intptr_t *addr,
                                                               intptr_t *oldval, intptr_t newval)
{
#if SIZEOF_VOID_P == 4
    return opal_atomic_compare_exchange_strong_rel_32((opal_atomic_int32_t*)addr,
                                                      (int32_t*)oldval,
                                                      (int32_t)newval);
#elif SIZEOF_VOID_P == 8
    return opal_atomic_compare_exchange_strong_rel_64((opal_atomic_int64_t*)addr,
                                                      (int64_t*)oldval,
                                                      (int64_t)newval);
#else
#error "No implementation of opal_atomic_compare_exchange_strong_rel_ptr"
#endif
}

#endif /* #ifndef ATOMIC_IMPL_PTR_CSWAP_H */
