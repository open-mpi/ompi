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



#ifndef LINUX_IA64_ATOMIC_H_INCLUDED
#define LINUX_IA64_ATOMIC_H_INCLUDED

/*
 * Lock structure
 */

enum { LOCK_UNLOCKED = 1 };

typedef struct {
  union {
    volatile int lockData_m;
    char padding[4];
  } data;
} lam_lock_data_t;


/*
 * 64 bit integer
 */
typedef struct {
  lam_lock_data_t lock;
  volatile unsigned long long data;
} bigAtomicUnsignedInt;


#ifdef __INTEL_COMPILER

#ifdef __cplusplus
extern "C"
{
#endif

  void spinlock(lam_lock_data_t *lockData);
  int spintrylock(lam_lock_data_t *lockData);
  int fetchNadd(volatile int *addr, int inc);
  int fetchNset(volatile int *addr, int setValue);

#ifdef __cplusplus
}
#endif


#else

/*
 *  Spin until I can get the lock
 */
INLINE void spinlock(lam_lock_data_t *lockData)
{

  __asm__ __volatile__ (								
	"mov r30=1\n"								
	"mov ar.ccv=r0\n"							
	";;\n"									
	"cmpxchg4.acq r30=[%0],r30,ar.ccv\n"					
	";;\n"									
	"cmp.ne p15,p0=r30,r0\n"						
	"(p15) br.call.spnt.few b7=ia64_spinlock_contention\n"			
	";;\n"									
	"1:\n"				/* force a new bundle */		
	:: "r"(*lockData)								
	: "ar.ccv", "ar.pfs", "b7", "p15", "r28", "r29", "r30", "memory");	
}

/*
 * This routine tries once to obtain the lock
 */
INLINE int spintrylock(lam_lock_data_t *lockData)
{

  int gotLock;

    __asm__ __volatile__ (								
	  "mov ar.ccv=r0\n"							
	  ";;\n"									
	  "cmpxchg4.acq %0=[%2],%1,ar.ccv\n"					
	  : "=r"(gotLock) : "r"(1), "r"(&(lockData)->data.lockData_m) : "ar.ccv", "memory");		

    return gotLock;

}


/*
 *  atomically add a constant to the input integer returning the
 *  previous value
 */

INLINE int fetchNadd(volatile int *addr, int inc)
{
  int inputValue;

  __asm__ __volatile__ (
	"fetchadd4.rel %0=[%1],%2" \
	: "=r"(*addr) : "r"(inputValue), "i"(inc) : "memory");

  return (inputValue);
}


INLINE int fetchNset(volatile int *addr, int setValue)
{
    int inputValue;

    __asm__ __volatile__ (
	"xchg4 %0=[%1],%2" \
	: "=r"(*addr) : "r"(inputValue), "i"(setValue) : "memory");

    return (inputValue);
}

#endif		/* __INTEL_COMPILER */

/*
 * Clear the lock
 */
INLINE void spinunlock(lam_lock_data_t *lockData)
{
    lockData->data.lockData_m = LOCK_UNLOCKED;
}

INLINE unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr,
                                               int inc)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    (addr->data) += inc;
    spinunlock(&(addr->lock));

    return returnValue;
}


INLINE unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    addr->data = val;
    spinunlock(&(addr->lock));

    return returnValue;
}


INLINE unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                                     int inc)
{
    unsigned long long returnValue;

    returnValue = addr->data;
    addr->data += inc;

    return returnValue;
}

INLINE void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                           unsigned long long value)
{
    addr->data = value;
    addr->lock.data.lockData_m = LOCK_UNLOCKED;
}

#endif /* LINUX_IA64_ATOMIC_H_INCLUDED */
