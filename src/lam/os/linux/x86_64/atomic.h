/*
 * $HEADER$
 */

#ifndef LINUX_X86_64_ATOMIC_H_INCLUDED
#define LINUX_X86_64_ATOMIC_H_INCLUDED

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

/*
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
*/

/*
 *  Spin until I can get the lock
 */
inline void spinlock(lam_lock_data_t *lockData)
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
inline int spintrylock(lam_lock_data_t *lockData)
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
inline int fetchNadd(volatile int *addr, int inc)
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


inline int fetchNset(volatile int *addr, int setValue)
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

//#endif		/* __INTEL_COMPILER */


/*
 * Clear the lock
 */
inline void spinunlock(lam_lock_data_t *lockData)
{
    lockData->data.lockData_m = 1;
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
    addr->lock.data.lockData_m = LOCK_UNLOCKED;
}

#endif /* LINUX_X86_64_ATOMIC_H_INCLUDED */
