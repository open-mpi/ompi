/*
 * Copyright 2002-2003. The Regents of the University of
 * California. This material was produced under U.S. Government
 * contract W-7405-ENG-36 for Los Alamos National Laboratory, which is
 * operated by the University of California for the U.S. Department of
 * Energy. The Government is granted for itself and others acting on
 * its behalf a paid-up, nonexclusive, irrevocable worldwide license
 * in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years
 * after October 10,2002 subject to additional five-year worldwide
 * renewals, the Government is granted for itself and others acting on
 * its behalf a paid-up, nonexclusive, irrevocable worldwide license
 * in this material to reproduce, prepare derivative works, distribute
 * copies to the public, perform publicly and display publicly, and to
 * permit others to do so. NEITHER THE UNITED STATES NOR THE UNITED
 * STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF CALIFORNIA, NOR
 * ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR
 * ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT,
 * OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
 * PRIVATELY OWNED RIGHTS.

 * Additionally, this program is free software; you can distribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or any later version.  Accordingly, this
 * program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

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
} lam_lock_data_t;

/*
 * 64 bit integer
 */
typedef volatile unsigned long long bigAtomicUnsignedInt;


static inline void spinlock(lam_lock_data_t *lock)
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

static inline int spintrylock(lam_lock_data_t *lock)
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
static inline void spinunlock(lam_lock_data_t *lock)
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
