/*
 * Copyright 2002-2003. The Regents of the University of California. This material 
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos 
 * National Laboratory, which is operated by the University of California for 
 * the U.S. Department of Energy. The Government is granted for itself and 
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide 
 * license in this material to reproduce, prepare derivative works, and 
 * perform publicly and display publicly. Beginning five (5) years after 
 * October 10,2002 subject to additional five-year worldwide renewals, the 
 * Government is granted for itself and others acting on its behalf a paid-up, 
 * nonexclusive, irrevocable worldwide license in this material to reproduce, 
 * prepare derivative works, distribute copies to the public, perform publicly 
 * and display publicly, and to permit others to do so. NEITHER THE UNITED 
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF 
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR 
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY 
 * OWNED RIGHTS.

 * Additionally, this program is free software; you can distribute it and/or 
 * modify it under the terms of the GNU Lesser General Public License as 
 * published by the Free Software Foundation; either version 2 of the License, 
 * or any later version.  Accordingly, this program is distributed in the hope 
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/



#ifndef IRIX_ATOMIC_H_INCLUDED
#define IRIX_ATOMIC_H_INCLUDED

#include "internal/linkage.h"

/*
 * 64 bit integer
 */
typedef volatile unsigned long long bigAtomicUnsignedInt;

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

CDECL_BEGIN

static inline void spinunlock(lam_lock_data_t *ctlData_m)
{
    ctlData_m->data.lockData_m = LOCK_UNLOCKED;
}

void spinlock(lam_lock_data_t *);
int spintrylock(lam_lock_data_t *);
int fetchNadd(volatile int *addr, int inc);
int fetchNset(volatile int *addr, int val);
unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr, int inc);
unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                 unsigned long long val);
void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                             unsigned long long val);
unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                       int inc);
CDECL_END

#endif /* IRIX_ATOMIC_H_INCLUDED */
