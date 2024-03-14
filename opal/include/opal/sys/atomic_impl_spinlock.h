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
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Compare-and-swap based implementation of the atomic interface
 */

#ifndef ATOMIC_SPINLOCK_IMPL_H
#define ATOMIC_SPINLOCK_IMPL_H

static inline void opal_atomic_lock_init(opal_atomic_lock_t *lock, int32_t value)
{
    *lock = value;
    opal_atomic_wmb();
}

static inline int opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    int32_t unlocked = OPAL_ATOMIC_LOCK_UNLOCKED;
    bool ret = opal_atomic_compare_exchange_strong_acq_32(lock, &unlocked,
                                                          OPAL_ATOMIC_LOCK_LOCKED);
    return (ret == false) ? 1 : 0;
}

static inline void opal_atomic_lock(opal_atomic_lock_t *lock)
{
    while (opal_atomic_trylock(lock)) {
        while (*lock == OPAL_ATOMIC_LOCK_LOCKED) {
            /* spin */;
        }
    }
}

static inline void opal_atomic_unlock(opal_atomic_lock_t *lock)
{
    opal_atomic_wmb();
    *lock = OPAL_ATOMIC_LOCK_UNLOCKED;
}

#endif /* #ifndef ATOMIC_SPINLOCK_IMPL_H */
