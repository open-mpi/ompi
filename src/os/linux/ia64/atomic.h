/*
 * $HEADER$
 */

#ifndef LINUX_IA64_ATOMIC_H_INCLUDED
#define LINUX_IA64_ATOMIC_H_INCLUDED

/*
 * Lock structure
 */

enum { OMPI_LOCK_OMPI_UNLOCKED = 1 };

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


#ifdef __INTEL_COMPILER

#ifdef __cplusplus
extern "C"
{
#endif

  void spinlock(ompi_lock_data_t *lockData);
  int spintrylock(ompi_lock_data_t *lockData);
  int fetchNadd(volatile int *addr, int inc);
  int fetchNset(volatile int *addr, int setValue);

#ifdef __cplusplus
}
#endif


#else

/*
 *  Spin until I can get the lock
 */
inline void spinlock(ompi_lock_data_t *lockData)
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
inline int spintrylock(ompi_lock_data_t *lockData)
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

inline int fetchNadd(volatile int *addr, int inc)
{
  int inputValue;

  __asm__ __volatile__ (
	"fetchadd4.rel %0=[%1],%2" \
	: "=r"(*addr) : "r"(inputValue), "i"(inc) : "memory");

  return (inputValue);
}


inline int fetchNset(volatile int *addr, int setValue)
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
inline void spinunlock(ompi_lock_data_t *lockData)
{
    lockData->data.lockData_m = OMPI_LOCK_OMPI_UNLOCKED;
}

inline unsigned long long fetchNaddLong(bigAtomicUnsignedInt *addr,
                                               int inc)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    (addr->data) += inc;
    spinunlock(&(addr->lock));

    return returnValue;
}


inline unsigned long long fetchNsetLong(bigAtomicUnsignedInt *addr,
                                               unsigned long long val)
{
    unsigned long long returnValue;

    spinlock(&(addr->lock));
    returnValue = addr->data;
    addr->data = val;
    spinunlock(&(addr->lock));

    return returnValue;
}


inline unsigned long long fetchNaddLongNoLock(bigAtomicUnsignedInt *addr,
                                                     int inc)
{
    unsigned long long returnValue;

    returnValue = addr->data;
    addr->data += inc;

    return returnValue;
}

inline void setBigAtomicUnsignedInt(bigAtomicUnsignedInt *addr,
                                           unsigned long long value)
{
    addr->data = value;
    addr->lock.data.lockData_m = OMPI_LOCK_OMPI_UNLOCKED;
}

#endif /* LINUX_IA64_ATOMIC_H_INCLUDED */
