/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef CYGWIN_I686_ATOMIC_H_INCLUDED
#define CYGWIN_I686_ATOMIC_H_INCLUDED

/*
 * Lock structure
 */

enum { LOCK_UNLOCKED = 1 };

typedef struct {
    union {
        volatile int lockData_m;
        char padding[4];
    } data;
} ompi_lock_data_t;


/*
 * 64 bit integer
 */
typedef struct {
    ompi_lock_data_t lock;
    volatile unsigned long long data;
} bigAtomicUnsignedInt;

/*
#ifdef __INTEL_COMPILER

#if defined(c_plusplus) || defined(__cplusplus)
extern "C"
{
#endif

    void spinlock(ompi_lock_data_t *lockData);
    int spintrylock(ompi_lock_data_t *lockData);
    int fetchNadd(volatile int *addr, int inc);
    int fetchNset(volatile int *addr, int setValue);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#else
*/

/*
 *  Spin until I can get the lock
 */
static inline void spinlock(ompi_lock_data_t *lockData)
{
    __asm__ __volatile__(
        "cmp $1, %0\n"
        "jl 2f\n"
        "\n1: "
        "lock ; decl %0\n"
        "jz 3f\n"
        "2:\n"
        "cmp $1, %0\n"
        "jl 2b\n"
        "jmp 1b\n"
        "3:\n"
        : "=m" (lockData->data.lockData_m) :  : "memory");
}

/*
 * This routine tries once to obtain the lock
 */
static inline int spintrylock(ompi_lock_data_t *lockData)
{
    int gotLock;

    __asm__ __volatile__(
        "mov %2, %1\n"
        "cmp $1, %0\n"
        "jl 1f\n"
        "lock ; decl %0\n"
        "js 1f\n"
        "mov $1, %1\n"
        "jmp 2f\n"
        "1:\n"
        "mov $0, %1\n"
        "2:"
        : "=m" (lockData->data.lockData_m),
#ifdef __INTEL_COMPILER
        "=&r" (gotLock) : "r" (0) : "memory");
#else
        "=r" (gotLock) : "r" (0) : "memory");
#endif

    return gotLock;
}


/*
 *  atomically add a constant to the input integer returning the
 *  previous value
 */
static inline int fetchNadd(volatile int *addr, int inc)
{
    int inputValue;
    __asm__ __volatile__(
	"       mov %2, %1\n" \
	"lock ; xadd %1, %0\n"
#ifdef __INTEL_COMPILER
	: "=m" (*addr), "=&r" (inputValue) : "r" (inc) : "memory");
#else
    : "=m" (*addr), "=r" (inputValue) : "r" (inc) : "memory");
#endif

    return (inputValue);
}


static inline int fetchNset(volatile int *addr, int setValue)
{
    int inputValue;

    __asm__ __volatile__(
	"       mov %2, %1\n" \
	"lock ; xchg %1, %0\n"
#ifdef __INTEL_COMPILER
	: "=m" (*addr), "=&r" (inputValue) : "r" (setValue) : "memory");
#else
    : "=m" (*addr), "=r" (inputValue) : "r" (setValue) : "memory");
#endif

    return (inputValue);
}


/*
 * Clear the lock
 */
static inline void spinunlock(ompi_lock_data_t *lockData)
{
    lockData->data.lockData_m = 1;
}


static inline unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr,
                                               int inc)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    (addr->data) += inc;
    spinunlock(&(addr->lock));

    return returnValue;
}


static inline unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    addr->data = val;
    spinunlock(&(addr->lock));

    return returnValue;
}


static inline unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                                     int inc)
{
    unsigned long long returnValue;

    returnValue = addr->data;
    addr->data += inc;

    return returnValue;
}

static inline void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                           unsigned long long value)
{
    addr->data = value;
    addr->lock.data.lockData_m = LOCK_UNLOCKED;
}

#endif /* CYGWIN_I686_ATOMIC_H_INCLUDED */
