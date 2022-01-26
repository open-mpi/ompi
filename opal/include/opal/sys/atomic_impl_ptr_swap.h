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

#ifndef ATOMIC_SWAP_PTR_IMPL_H
#define ATOMIC_SWAP_PTR_IMPL_H

static inline intptr_t opal_atomic_swap_ptr(opal_atomic_intptr_t *addr, intptr_t newval)
{
#if SIZEOF_VOID_P == 4
    return (intptr_t)opal_atomic_swap_32((opal_atomic_int32_t *) addr, (int32_t) newval);
#elif SIZEOF_VOID_P == 8
    return (intptr_t)opal_atomic_swap_64((opal_atomic_int64_t *) addr, (int64_t) newval);
#else
#error "No implementation of opal_atomic_swap_ptr"
#endif
}

#endif /* #ifndef ATOMIC_SWAP_PTR_IMPL_H */
