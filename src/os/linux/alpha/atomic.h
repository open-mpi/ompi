/*
 * $HEADER$
 */

#ifndef LINUX_ALPHA_ATOMIC_H_INCLUDED
#define LINUX_ALPHA_ATOMIC_H_INCLUDED


/*
 * 64 bit integer
 */
typedef volatile unsigned long long bigAtomicUnsignedInt;

#include "internal/linkage.h"

CDECL_BEGIN

/*
 * Lock structure
 */
    enum { LOCK_UNLOCKED = 0 };

    typedef struct {
        union {
            volatile int lockData_m;
            char padding[4];
        } data;
    } ompi_lock_data_t;

#define mb() \
__asm__ __volatile__("mb": : :"memory")

#define rmb() \
__asm__ __volatile__("mb": : :"memory")

#define wmb() \
__asm__ __volatile__("wmb": : :"memory")


/*
 *  This is routine spins until the lock is obtained
 *    A value of 0 indicates that the lock is available
 *               1 or more the lock is held by someone
 */
    inline void spinlock(ompi_lock_data_t *lock)
    {
        /*** sungeun *** ref: alpha-linux spinlock sources ***/
        int tmp = 0;

        /* Use sub-sections to put the actual loop at the end
           of this object file's text section so as to perfect
           branch prediction.  */
        __asm__ __volatile__(
            "1:	ldl	%0,%1\n"
            "	blbs	%0,2f\n"
            "	ldl_l	%0,%1\n"
            "	blbs	%0,2f\n"
            "	or	%0,1,%0\n"
            "	stl_c	%0,%1\n"
            "	beq	%0,2f\n"
            "	mb\n"
            ".subsection 2\n"
            "2:	ldl	%0,%1\n"
            "	blbs	%0,2b\n"
            "	br	1b\n"
            ".previous"
            : "=&r" (tmp), "=m" (lock->data.lockData_m)
            : "m"(lock->data.lockData_m) : "memory");
    }


/*
 * This routine tries once to obtain the lock
 */
    inline int spintrylock(ompi_lock_data_t *lock)
    {
        int got_lock = 0;
        int tmp = 0;

        __asm__ __volatile__(
            "	ldl	%0,%2\n"
            "	blbs	%0,1f\n"
            "	ldl_l	%0,%2\n"
            "	blbs	%0,1f\n"
            "	or	%0,1,%0\n"
            "	stl_c	%0,%2\n"
            "	beq	%0,1f\n"
            "	mov	1,%1\n"
            "1:	mb\n"
            : "=&r" (tmp), "=&r" (got_lock), "=m" (lock->data.lockData_m)
            : "m"(lock->data.lockData_m) : "memory");

        return got_lock;
    }


/*
 * Clear the lock - alpha specific - need memory barrier
 */
    inline void spinunlock(ompi_lock_data_t *lock)
    {
        mb();
        lock->data.lockData_m = 0;
    }

    inline int fetchNadd(volatile int *addr, int inc)
    {
        int oldval = 0;
        int tmp = 0;

        __asm__ __volatile__(
            "1:	ldl_l	%1, %0\n"
            "	addl	%1, %2, %3\n"
            "	stl_c	%3, %0\n"
            "	beq	%3, 2f\n"
            "	br 3f\n"
            "2:\n"
            "	br 1b\n"
            "3:\n"
            "	mb\n"
            : "=m" (*addr), "=r" (oldval)
            : "r" (inc), "r" (tmp)
            : "memory");

        return oldval;
    }



    inline int fetchNset(volatile int *addr, int val)
    {
        int oldval = 0;
        int tmp = 0;

        __asm__ __volatile__(
            "1:	ldl_l	%1, %0\n"
            "	mov	%2, %3\n"
            "	stl_c	%3, %0\n"
            "	beq	%3, 2f\n"
            "	br 3f\n"
            "2:\n"
            "	br 1b\n"
            "3:\n"
            "	mb\n"
            : "=m" (*addr), "=r" (oldval)
            : "r" (val), "r" (tmp)
            : "memory");

        return oldval;
    }


    inline unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr, int inc)
    {
        unsigned long long oldval = 0;
        unsigned long long tmp = 0;

        __asm__ __volatile__(
            /* load the contents of addr */
            "1:	ldq_l %1, %0\n"
            /* increment count */
            "	addq %1, %2, %3\n"
            /* conditional store */
            "	stq_c %3, %0\n"
            /* store conditional failed - loop again */
            "	beq %3, 1b\n"
            /* store conditional passed - go to memory barrier */
            "	br 3f\n"
            /* loop again */
            "2:	br 1b\n"
            /* memory barrier and exit */
            "3:	mb\n"
            : "=m" (*addr), "=r" (oldval)
            : "r" (inc), "r" (tmp)
            : "memory");

        return oldval;
    }


    inline unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                                         int inc)
    {
        unsigned long long val;

        val = *addr;
        *addr += inc;

        return val;
    }

    inline unsigned long long fetchNsetLong(volatile unsigned long long *addr,
                                                   unsigned long long val)
    {
        unsigned long long oldval = 0;
        unsigned long long tmp = 0;

        __asm__ __volatile__(
            "1:	ldq_l	%1, %0\n"
            "	mov	%2, %3\n"
            "	stq_c	%3, %0\n"
            "	beq	%3, 2f\n"
            "	br 3f\n"
            "2:\n"
            "	br 1b\n"
            "3:\n"
            "	mb\n"
            : "=m" (*addr), "=r" (oldval)
            : "r" (val), "r" (tmp)
            : "memory");

        return oldval;
    }


    inline void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
    {
        *addr = val;
    }

CDECL_END

#endif /* LINUX_ALPHA_ATOMIC_H_INCLUDED */
