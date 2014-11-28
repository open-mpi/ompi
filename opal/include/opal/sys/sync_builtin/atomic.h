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
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

static inline void opal_atomic_mb(void)
{
    __sync_synchronize();
}

static inline void opal_atomic_rmb(void)
{
    __sync_synchronize();
}

static inline void opal_atomic_wmb(void)
{
    __sync_synchronize();
}

#if OPAL_WANT_SMP_LOCKS
#define MB() opal_atomic_mb()
#else
#define MB()
#endif

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define OPAL_HAVE_ATOMIC_CMPSET_32 1

static inline int32_t opal_atomic_cmpset_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
    return __sync_val_compare_and_swap(addr, oldval, newval);
}

#define opal_atomic_cmpset_acq_32 opal_atomic_cmpset_32
#define opal_atomic_cmpset_rel_32 opal_atomic_cmpset_32

#define OPAL_HAVE_ATOMIC_MATH_32 1

#define OPAL_HAVE_ATOMIC_ADD_32 1
static inline int32_t opal_atomic_add_32(volatile int32_t *addr, int32_t delta)
{
    return __sync_fetch_and_add(addr, delta);
}

#define OPAL_HAVE_ATOMIC_SUB_32 1
static inline int32_t opal_atomic_sub_32(volatile int32_t *addr, int32_t delta)
{
    return __sync_fetch_and_sub(addr, delta);
}

#define OPAL_HAVE_ATOMIC_CMPSET_64 1

static inline int64_t opal_atomic_cmpset_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    return __sync_val_compare_and_swap(addr, oldval, newval);
}

#define opal_atomic_cmpset_acq_64 opal_atomic_cmpset_64
#define opal_atomic_cmpset_rel_64 opal_atomic_cmpset_64

#define OPAL_HAVE_ATOMIC_MATH_64 1
#define OPAL_HAVE_ATOMIC_ADD_64 1
static inline int64_t opal_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
    return __sync_fetch_and_add(addr, delta);
}

#define OPAL_HAVE_ATOMIC_SUB_64 1
static inline int64_t opal_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
    return __sync_fetch_and_sub(addr, delta);
}

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
