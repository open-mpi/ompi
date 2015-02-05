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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/*
 * On alpha, everything is load-locked, store-conditional...
 */

#define MB()  __asm__ __volatile__ ("mb");
#define RMB() __asm__ __volatile__ ("mb");
#define WMB() __asm__ __volatile__ ("wmb");

/**********************************************************************
 *
 * Define constants for PowerPC 32
 *
 *********************************************************************/
#define OPAL_HAVE_ATOMIC_MEM_BARRIER 1

#define OPAL_HAVE_ATOMIC_CMPSET_32 1

#define OPAL_HAVE_ATOMIC_CMPSET_64 1


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
    RMB();
}


static inline void opal_atomic_wmb(void)
{
    WMB();
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/
#if OPAL_GCC_INLINE_ASSEMBLY

static inline int32_t opal_atomic_cmpset_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
   int32_t ret;
   
   __asm __volatile__ (
		       "1:  ldl_l %0, %1        \n\t"
		       "cmpeq %0, %2, %0        \n\t"
		       "beq %0, 2f              \n\t"
		       "mov %3, %0              \n\t"
		       "stl_c %0, %1            \n\t"
		       "beq %0, 1b              \n\t"
		       "jmp 3f                  \n"
		       "2:  mov $31, %0         \n"
		       "3:                      \n"
		       : "=&r" (ret), "+m" (*addr)
		       : "r" (oldval), "r" (newval)
		       : "memory");

    return ret;
}


static inline int32_t opal_atomic_cmpset_acq_32(volatile int32_t *addr,
                                                int32_t oldval,
                                                int32_t newval)
{
    int32_t rc;

    rc = opal_atomic_cmpset_32(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}


static inline int32_t opal_atomic_cmpset_rel_32(volatile int32_t *addr,
                                                int32_t oldval,
                                                int32_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_cmpset_32(addr, oldval, newval);
}


static inline int64_t opal_atomic_cmpset_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    int64_t ret;

    __asm__ __volatile__ (
			  "1:  ldq_l %0, %1     \n\t"
			  "cmpeq %0, %2, %0     \n\t"
			  "beq %0, 2f           \n\t"
			  "mov %3, %0           \n\t"
			  "stq_c %0, %1         \n\t"
			  "beq %0, 1b           \n\t"
			  "jmp 3f               \n"
			  "2:  mov $31, %0      \n"
			  "3:                   \n"
			  : "=&r" (ret), "+m" (*addr)
			  : "r" (oldval), "r" (newval)
			  : "memory");

    return ret;
}


static inline int64_t opal_atomic_cmpset_acq_64(volatile int64_t *addr,
                                                int64_t oldval,
                                                int64_t newval)
{
    int64_t rc;

    rc = opal_atomic_cmpset_64(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}


static inline int64_t opal_atomic_cmpset_rel_64(volatile int64_t *addr,
                                                int64_t oldval,
                                                int64_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_cmpset_64(addr, oldval, newval);
}


#define OPAL_HAVE_INLINE_ATOMIC_ADD_32 1
static inline int32_t opal_atomic_add_32(volatile int32_t *addr,
                                         int32_t inc)
{
    int32_t temp, result;

    __asm__ __volatile__(
            "1:     ldl_l %0,%1    \n"
            "       addl %0,%3,%2  \n"
            "       addl %0,%3,%0  \n"
            "       stl_c %0,%1    \n"
            "       beq %0,2f      \n"
            ".subsection 2         \n"
            "2:     br 1b          \n"
            ".previous"
        :"=&r" (temp), "=m" (*addr), "=&r" (result)
        :"Ir" (inc), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return result-inc;
}

#define OPAL_HAVE_INLINE_ATOMIC_SUB_32 1
static inline int64_t opal_atomic_add_64(volatile int64_t *addr,
                                         int64_t inc)
{
    int64_t temp, result;

    __asm__ __volatile__(
            "1:     ldq_l %0,%1\n"
            "       addq %0,%3,%2\n"
            "       addq %0,%3,%0\n"
            "       stq_c %0,%1\n"
            "       beq %0,2f\n"
            ".subsection 2\n"
            "2:     br 1b\n"
            ".previous"
        :"=&r" (temp), "=m" (*addr), "=&r" (result)
        :"Ir" (inc), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return result-inc;
}

#define OPAL_HAVE_INLINE_ATOMIC_ADD_64 1
static inline int32_t opal_atomic_sub_32(volatile int32_t *addr,
                                         int32_t dec)
{
    int32_t temp, result;

    __asm__ __volatile__(
            "1:     ldl_l %0,%1    \n"
            "       subl %0,%3,%2  \n"
            "       subl %0,%3,%0  \n"
            "       stl_c %0,%1    \n"
            "       beq %0,2f      \n"
            ".subsection 2         \n"
            "2:     br 1b          \n"
            ".previous"
        :"=&r" (temp), "=m" (*addr), "=&r" (result)
        :"Ir" (dec), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return result+dec;
}

#define OPAL_HAVE_INLINE_ATOMIC_SUB_64 1
static inline int64_t opal_atomic_sub_64(volatile int64_t *addr,
                                         int64_t dec)
{
    int64_t temp, result;

    __asm__ __volatile__(
            "1:     ldq_l %0,%1    \n"
            "       subq %0,%3,%2  \n"
            "       subq %0,%3,%0  \n"
            "       stq_c %0,%1    \n"
            "       beq %0,2f      \n"
            ".subsection 2         \n"
            "2:     br 1b          \n"
            ".previous"
        :"=&r" (temp), "=m" (*addr), "=&r" (result)
        :"Ir" (dec), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return result+dec;
}

#define OPAL_HAVE_ATOMIC_SWAP_32 1
static inline int32_t opal_atomic_swap_32( volatile int32_t *addr,
                                           int32_t newval)
{
    int32_t dummy;

    __asm__ __volatile__(
        "1:     ldl_l %0,%4    \n"
        "       bis $31,%3,%1  \n"
        "       stl_c %1,%2    \n"
        "       beq %1,2f      \n"
        ".subsection 2         \n"
        "2:     br 1b          \n"
        ".previous"
        : "=&r" (newval), "=&r" (dummy), "=m" (*addr)
        : "rI" (newval), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return newval;
}

#define OPAL_HAVE_ATOMIC_SWAP_64 1
static inline int64_t opal_atomic_swap_64( volatile int64_t *addr,
                                           int64_t newval)
{
    int32_t dummy;

    __asm__ __volatile__(
        "1:     ldq_l %0,%4    \n"
        "       bis $31,%3,%1  \n"
        "       stq_c %1,%2    \n"
        "       beq %1,2f      \n"
        ".subsection 2         \n"
        "2:     br 1b          \n"
        ".previous"
        : "=&r" (newval), "=&r" (dummy), "=m" (*addr)
        : "rI" (newval), "m" (*addr)
        : "memory");
    opal_atomic_mb();

    return newval;
}

#endif /* OPAL_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
