/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserverd.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

#include <libkern/OSAtomic.h>


#if OPAL_WANT_SMP_LOCKS
#define MB() OSMemoryBarrier
#else
#define MB()
#endif


/**********************************************************************
 *
 * Define constants for OSX/iOS
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1
#define OPAL_HAVE_ATOMIC_CMPSET_32 1
#define OPAL_HAVE_ATOMIC_CMPSET_64 1
#define OPAL_HAVE_ATOMIC_MATH_32 1
#define OPAL_HAVE_ATOMIC_MATH_64 1
#define OPAL_HAVE_ATOMIC_ADD_32 1
#define OPAL_HAVE_ATOMIC_ADD_64 1
#define OPAL_HAVE_ATOMIC_SUB_32 1
#define OPAL_HAVE_ATOMIC_SUB_64 1
#define OPAL_HAVE_ATOMIC_SPINLOCKS 1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
static inline void opal_atomic_mb(void)
{
    MB();
}


static inline void opal_atomic_rmb(void)
{
    MB();
}


static inline void opal_atomic_wmb(void)
{
    MB();
}

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
static inline int opal_atomic_cmpset_32( volatile int32_t *addr,
                                        int32_t oldval, int32_t newval)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicCompareAndSwap32Barrier(oldval, newval, addr);
#else
    return OSAtomicCompareAndSwap32(oldval, newval, addr);
#endif
}

#define opal_atomic_cmpset_acq_32 opal_atomic_cmpset_32
#define opal_atomic_cmpset_rel_32 opal_atomic_cmpset_32


static inline int opal_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicCompareAndSwap64Barrier(oldval, newval, addr);
#else
    return OSAtomicCompareAndSwap64(oldval, newval, addr);
#endif
}

#define opal_atomic_cmpset_acq_64 opal_atomic_cmpset_64
#define opal_atomic_cmpset_rel_64 opal_atomic_cmpset_64

/**
 * atomic_add - add integer to atomic variable
 * @i: integer value to add
 * @v: pointer of type int
 *
 * Atomically adds @i to @v.
 */
static inline int32_t opal_atomic_add_32(volatile int32_t* v, int i)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicAdd32Barrier (i, v);
#else
    return OSAtomicAdd32 (i, v);
#endif
}

/**
 * atomic_add - add integer to atomic variable
 * @i: integer value to add
 * @v: pointer of type int
 *
 * Atomically adds @i to @v.
 */
static inline int64_t opal_atomic_add_64(volatile int64_t* v, int64_t i)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicAdd64Barrier (i, v);
#else
    return OSAtomicAdd64 (i, v);
#endif
}

/**
 * atomic_sub - subtract the atomic variable
 * @i: integer value to subtract
 * @v: pointer of type int
 *
 * Atomically subtracts @i from @v.
 */
static inline int32_t opal_atomic_sub_32(volatile int32_t* v, int i)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicAdd32Barrier (-i, v);
#else
    return OSAtomicAdd32 (-i, v);
#endif
}

/**
 * atomic_sub - subtract the atomic variable
 * @i: integer value to subtract
 * @v: pointer of type int
 *
 * Atomically subtracts @i from @v.
 */
static inline int64_t opal_atomic_sub_64(volatile int64_t* v, int64_t i)
{
#if OPAL_WANT_SMP_LOCKS
    return OSAtomicAdd64Barrier (-i, v);
#else
    return OSAtomicAdd64 (-i, v);
#endif
}

static inline void opal_atomic_init(opal_atomic_lock_t* lock, int32_t value)
{
    lock->u.lock = OS_SPINLOCK_INIT;
    if (value) {
        OSSpinLockLock (&lock->u.lock);
    }
}

static inline int opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    return !OSSpinLockTry (&lock->u.lock);
}

static inline void opal_atomic_lock(opal_atomic_lock_t *lock)
{
    OSSpinLockLock (&lock->u.lock);
}

static inline void opal_atomic_unlock(opal_atomic_lock_t *lock)
{
    OSSpinLockUnlock (&lock->u.lock);
}

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
