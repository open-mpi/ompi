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

#ifndef TRU64_ATOMIC_H_INCLUDED
#define TRU64_ATOMIC_H_INCLUDED

#include <c_asm.h>
#include <regdef.h>

#define mb()  asm("mb\n");

#define rmb() asm("mb\n");

#define wmb() asm("wmb\n");

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

/*
 * 64 bit integer
 */
typedef volatile unsigned long long bigAtomicUnsignedInt;


static inline void spinlock(ompi_lock_data_t *lock)
{
    asm("loop:\n"
	"    ldl %t1, (%a0)\n"
	"    blbs %t1, already_set\n"
	"    ldl_l %t1, (%a0)\n"
	"    blbs %t1, already_set\n"
	"    or %t1, 1, %t2\n"
	"    stl_c %t2, (%a0)\n"
	"    beq %t2, stl_c_failed\n"
	"    br lock_set\n"
	"already_set:\n"
	"stl_c_failed:\n"
	"    br loop\n"
	"lock_set:\n"
	"    mb\n",
	&(lock->data.lockData_m));
}

/*
 * locked load - store conditional pair can fail for
 * any number of implementation dependent reasons, so
 * we try up to 4 times before declaring failure to
 * obtain the lock...
 */

static inline int spintrylock(ompi_lock_data_t *lock)
{
    int result = asm("mov %zero, %t3\n"
           "loop:\n"
	       "    ldl %t1, (%a0)\n"
	       "    blbs %t1, already_set\n"
	       "    ldl_l %t1, (%a0)\n"
           "    blbs %t1, already_set\n"
	       "    or %t1, 1, %t2\n"
	       "    stl_c %t2, (%a0)\n"
	       "    beq %t2, stl_c_failed\n"
	       "    mov 1, %v0\n"
	       "    br lock_set\n"
	       "stl_c_failed:\n"
           "    addl %t3, 1, %t3\n"
           "    mov %t3, %t4\n"
           "    subl %t4, 3, %t4\n"
           "    ble %t4, loop\n"
	       "already_set:\n"
	       "    mov %zero, %v0\n"
	       "lock_set:\n"
	       "    mb\n",
	       &(lock->data.lockData_m));
    return result;
}

/* alpha specific unlock function - need the memory barrier */
static inline void spinunlock(ompi_lock_data_t *lock)
{
    asm("mb");
    lock->data.lockData_m = LOCK_UNLOCKED;
}

static inline int fetchNadd(volatile int *addr, int inc)
{
    int oldval;

    oldval = asm("try_again:\n"
                 "    ldl_l %v0, (%a0)\n"
                 "    addl %v0, %a1, %t1\n"
                 "    stl_c %t1, (%a0)\n"
                 "    beq %t1, no_store\n"
                 "    br store\n"
                 "no_store:\n"
                 "    br try_again\n"
                 "store:\n"
                 "    mb\n",
                 addr, inc);

    return oldval;
}


static inline unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr,
                                               int inc)
{
    unsigned long long oldval;

    oldval = asm("try_again:\n"
                 "    ldq_l %v0, (%a0)\n"
                 "    addq %v0, %a1, %t1\n"
                 "    stq_c %t1, (%a0)\n"
                 "    beq %t1, no_store\n"
                 "    br store\n"
                 "no_store:\n"
                 "    br try_again\n"
                 "store:\n"
                 "    mb\n",
                 addr, inc);

    return oldval;
}


static inline unsigned long long
fetchNaddLongNoLock(bigAtomicUnsignedInt *addr, int inc)
{
    unsigned long long oldval;

    oldval = *addr;
    *addr += inc;

    return oldval;
}


static inline int fetchNset(volatile int *addr, int val)
{
    int oldval;

    oldval = asm("try_again:\n"
                 "    ldl_l %v0, (%a0)\n"
                 "    mov %a1, %t1\n"
                 "    stl_c %t1, (%a0)\n"
                 "    beq %t1, no_store\n"
                 "    br store\n"
                 "no_store:\n"
                 "    br try_again\n"
                 "store:\n"
                 "    mb\n",
                 addr, val);

    return oldval;
}


static inline unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
{
    unsigned long long oldval;

    oldval = asm("try_again:\n"
                 "    ldq_l %v0, (%a0)\n"
                 "    mov %a1, %t1\n"
                 "    stq_c %t1, (%a0)\n"
                 "    beq %t1, no_store\n"
                 "    br store\n"
                 "no_store:\n"
                 "    br try_again\n"
                 "store:\n"
                 "    mb\n",
                 addr, val);

    return oldval;
}


static inline void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                           unsigned long long val)
{
    *addr = val;
}

#endif /* TRU64_ATOMIC_H_INCLUDED */
