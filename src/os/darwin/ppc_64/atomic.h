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

#ifndef DARWIN_PPC_64_ATOMIC_H_INCLUDED
#define DARWIN_PPC_64_ATOMIC_H_INCLUDED

/*
 * The following atomic operations were adapted from the examples
 * provided in the PowerPC programming manual available at
 * http://www-3.ibm.com/chips/techlib/techlib.nsf/techdocs/852569B20050FF778525699600719DF2
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define mb()    __asm__ __volatile__("sync")
#define rmb()    __asm__ __volatile__("sync")
#define wmb()    __asm__ __volatile__("sync")
    
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


/*
 *  Spin until I can get the lock
 */
static inline void spinlock(ompi_lock_data_t *lockData)
{
	volatile int		*lockptr = &(lockData->data.lockData_m);
	
    __asm__ __volatile__(
		"mr		r6, %0\n"		/* save the address of the lock. */
		"li    	 r4,1\n"
		"1:\n"
		"lwarx   r5,0,r6\n"		/* Get current lock value. */
		"cmpwi   r5,0x0\n"		/* Is it unlocked. if not, keep checking. */
		"bne-    1b\n"
		"stwcx.  r4,0,r6\n"		/* Try to atomically set the lock */
		"bne-    1b\n"		
		"isync\n" 
        : : "r" (lockptr)
		: "memory", "r4", "r5", "r6");
}

/*
 * This routine tries once to obtain the lock
 */
static inline int spintrylock(ompi_lock_data_t *lockData)
{
	volatile int		*lockptr = &(lockData->data.lockData_m);
    int 	gotLock = 0;

    __asm__ __volatile__(
		"mr		r6, %1\n"		/* save the address of the lock. */
		"li     r4,0x1\n"	
	"1:\n"
		"lwarx	r5,0,r6\n"		
		"cmpwi  r5,0x0\n"		/* Is it locked? */
		"bne-   2f\n"			/* Yes, return 0 */
		"stwcx. r4,0,r6\n"		/* Try to atomically set the lock */
		"bne-   1b\n"
		"addi	%0,0,1\n"
		"isync\n" 
		"b		3f\n"
	"2:	addi	%0,0,0x0\n"
	"3:"
        : "=&r" (gotLock) : "r" (lockptr)
		: "memory", "r4", "r5", "r6" );
				
	return gotLock;
}

/*
 * Clear the lock
 */
static inline void spinunlock(ompi_lock_data_t *lockData)
{
    lockData->data.lockData_m = LOCK_UNLOCKED;
}


/*
 *  atomically add a constant to the input integer returning the
 *  previous value
 */
static inline int fetchNadd(volatile int *addr, int inc)
{
    int inputValue;

    __asm__ __volatile__(
	"mr	r5,%2\n"				/* Save the increment */
"1:\n"
	"lwarx	%0, 0, %1\n"			/* Grab the value */
	"add	r6, %0, r5\n"			/* Add the value */
	"stwcx.	r6, 0, %1\n"			/* Try to save the new value */
	"bne-	1b\n"			/* Didn't get it, try again... */
	"isync\n" 
	: "=&r" (inputValue) : "r" (addr), "r" (inc) : 
	"memory", "r5", "r6");
	
	return inputValue;
}


static inline int fetchNset(volatile int *addr, int setValue)
{
    int inputValue;

    __asm__ __volatile__(
	"mr	r5,%2\n"				/* Save the value to store */
"1:\n"
	"lwarx	%0, 0, %1\n"			/* Grab the area value */
	"stwcx.	r5, 0, %1\n"			/* Try to save the new value */
	"bne-	1b\n"			/* Didn't get it, try again... */
	"isync\n" 
	: "=&r" (inputValue) : "r" (addr), "r" (setValue) : 
	"memory", "r5");
	
	return inputValue;
}


static inline unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr,
                                               int inc)
{
    unsigned long long returnValue;

    __asm__ __volatile__(
	"mr	r5,%2\n"				/* Save the increment */
"1:\n"
	"ldarx	%0, 0, %1\n"			/* Grab the value */
	"add	r6, %0, r5\n"			/* Add the value */
	"stdcx.	r6, 0, %1\n"			/* Try to save the new value */
	"bne-	1b\n"			/* Didn't get it, try again... */
	"isync\n" 
	: "=&r" (returnValue) : "r" (addr), "r" (inc) : 
	"memory", "r5", "r6");

    return returnValue;
}


static inline unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
{
    unsigned long long returnValue;

    __asm__ __volatile__(
	"mr	r5,%2\n"				/* Save the value to store */
"1:\n"
	"ldarx	%0, 0, %1\n"			/* Grab the area value */
	"stdcx.	r5, 0, %1\n"			/* Try to save the new value */
	"bne-	1b\n"			/* Didn't get it, try again... */
	"isync\n" 
	: "=&r" (returnValue) : "r" (addr), "r" (val) : 
	"memory", "r5");

    return returnValue;
}


static inline unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                                     int inc)
{
    unsigned long long returnValue;

    returnValue = *addr;
    *addr += inc;

    return returnValue;
}

static inline void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                           unsigned long long value)
{
    *addr = value;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* DARWIN_POWERPC_ATOMIC_H_INCLUDED */
