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
 * Copyright (c) 2007-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/*
 * On ia32, we use cmpxchg.
 */

#define SMPLOCK "lock; "
#define MB() __asm__ __volatile__("": : :"memory")


/**********************************************************************
 *
 * Define constants for IA32
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 1

#define OPAL_HAVE_ATOMIC_MATH_32 1
#define OPAL_HAVE_ATOMIC_ADD_32 1
#define OPAL_HAVE_ATOMIC_SUB_32 1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

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

static inline void opal_atomic_isync(void)
{
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

static inline int opal_atomic_cmpset_32(volatile int32_t *addr,
                                        int32_t oldval,
                                        int32_t newval)
{
   unsigned char ret;
   __asm__ __volatile__ (
                       SMPLOCK "cmpxchgl %3,%2   \n\t"
                               "sete     %0      \n\t"
                       : "=qm" (ret), "+a" (oldval), "+m" (*addr)
                       : "q"(newval)
                       : "memory", "cc");

   return (int)ret;
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#define opal_atomic_cmpset_acq_32 opal_atomic_cmpset_32
#define opal_atomic_cmpset_rel_32 opal_atomic_cmpset_32

#if OPAL_GCC_INLINE_ASSEMBLY

#define OPAL_HAVE_ATOMIC_SWAP_32 1

static inline int32_t opal_atomic_swap_32( volatile int32_t *addr,
					   int32_t newval)
{
    int32_t oldval;

    __asm__ __volatile__("xchg %1, %0" :
			 "=r" (oldval), "=m" (*addr) :
			 "0" (newval), "m" (*addr) :
			 "memory");
    return oldval;
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */


#if OPAL_GCC_INLINE_ASSEMBLY

/**
 * atomic_add - add integer to atomic variable
 * @i: integer value to add
 * @v: pointer of type int
 *
 * Atomically adds @i to @v.
 */
static inline int32_t opal_atomic_add_32(volatile int32_t* v, int i)
{
    int ret = i;
   __asm__ __volatile__(
                        SMPLOCK "xaddl %1,%0"
                        :"+m" (*v), "+r" (ret)
                        :
                        :"memory", "cc"
                        );
   return (ret+i);
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
    int ret = -i;
   __asm__ __volatile__(
                        SMPLOCK "xaddl %1,%0"
                        :"+m" (*v), "+r" (ret)
                        :
                        :"memory", "cc"
                        );
   return (ret-i);
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
