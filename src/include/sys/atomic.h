/*
 * $HEADER$
 */

/** @file
 *
 * Atomic operations.
 *
 * This API is patterned after the FreeBSD kernel atomic interface
 * (which is influenced by Intel's ia64 architecture).  The
 * FreeBSD interface is documented at
 *
 * http://www.freebsd.org/cgi/man.cgi?query=atomic&sektion=9
 *
 * Only the necessary subset of functions are implemented here.
 */

#ifndef OMPI_SYS_ATOMIC_H
#define OMPI_SYS_ATOMIC_H 1

#include "ompi_config.h"

#if defined(__GNUC__) || defined (WIN32)
#define STATIC_INLINE static inline
#else
#define STATIC_INLINE
#endif

/**
 * Volatile lock object (with optional padding).
 */
struct ompi_lock_t {
    union {
        volatile int lock;         /**< The lock address (an integer) */
        char padding[sizeof(int)]; /**< Array for optional padding */
    } u;
};

typedef struct ompi_lock_t ompi_lock_t;


/**
 * Memory barrier
 */
STATIC_INLINE void ompi_atomic_mb(void);


/**
 * Read memory barrier
 */
STATIC_INLINE void ompi_atomic_rmb(void);


/**
 * Write memory barrier.
 */
STATIC_INLINE void ompi_atomic_wmb(void);


/**
 * Atomic compare and set of unsigned 32-bit integer.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * Pseudo-code:
 *
 * @code
 *   int ompi_atomic_cmpset_32(addr, oldval, newval)
 *   {
 *       if (*addr == oldval) {
 *           *addr = newval;
 *           return 1;  // success, set value
 *       } else {
 *           return 0;  // failure, do not set value
 *       }
 *   }
 * @endcode
 */
STATIC_INLINE int ompi_atomic_cmpset_32(volatile uint32_t *addr,
                                        uint32_t oldval,
                                        uint32_t newval);


/**
 * Atomic compare and set of unsigned 32-bit integer with acquire
 * semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                            uint32_t oldval,
                                            uint32_t newval);


/**
 * Atomic compare and set of unsigned 32-bit integer with release
 * semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                            uint32_t oldval,
                                            uint32_t newval);


/**
 * Atomic compare and set of unsigned 64-bit integer.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_64(volatile uint64_t *addr,
                                        uint64_t oldval,
                                        uint64_t newval);


/**
 * Atomic compare and set of unsigned 64-bit integer with acquire
 * semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                            uint64_t oldval,
                                            uint64_t newval);


/**
 * Atomic compare and set of unsigned 64-bit integer with release
 * semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                            uint64_t oldval,
                                            uint64_t newval);


/**
 * Atomic compare and set of integer.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_int(volatile int *addr,
                                         int oldval,
                                         int newval);


/**
 * Atomic compare and set of integer with acquire semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_acq_int(volatile int *addr,
                                             int oldval,
                                             int newval);


/**
 * Atomic compare and set of integer with release semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_rel_int(volatile int *addr,
                                             int oldval,
                                             int newval);


/**
 * Atomic compare and set of pointer.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_ptr(volatile void *addr,
                                         void *oldval,
                                         void *newval);


/**
 * Atomic compare and set of pointer with acquire semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_acq_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval);


/**
 * Atomic compare and set of pointer with release semantics.
 *
 * @param addr          Address of integer.
 * @param oldval        Comparison value.
 * @param newval        New value to set if comparision is true.
 *
 * See ompi_atomic_cmpset_32 for pseudo-code.
 */
STATIC_INLINE int ompi_atomic_cmpset_rel_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval);

/**
 * Atomically add to a 32-bit integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline uint32_t ompi_atomic_add_32(volatile uint32_t *addr, int delta);


/**
 * Atomically add to a 64-bit integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline uint64_t ompi_atomic_add_64(volatile uint64_t *addr, int delta);


/**
 * Atomically add to an integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline int ompi_atomic_add_int(volatile int *addr, int delta);
 

/**
 * Atomically add to an integer.
 *
 * @param addr          Address of integer.
 * @param newval        Value to set.
 * @return              Old value of integer.
 */
static inline int ompi_atomic_fetch_and_set_int(volatile int *addr, int newval);
 

/**
 * Try to acquire a lock.
 *
 * @param lock          Address of the lock.
 * @return              0 if the lock was acquired, 1 otherwise.
 */
static inline int ompi_atomic_trylock(ompi_lock_t *lock);


/**
 * Acquire a lock by spinning.
 *
 * @param lock          Address of the lock.
 */
static inline void ompi_atomic_lock(ompi_lock_t *lock);


/**
 * Release a lock.
 *
 * @param lock          Address of the lock.
 */
static inline void ompi_atomic_unlock(ompi_lock_t *lock);


/*
 * Include system specific inline asm definitions. Otherwise
 * the definitions are in system specific .s files in src/util.
 */

/* Include win32/atomic.h if we are in windows platform. Else, we 
   can go through other compilers and options. */
#ifdef WIN32
#define OMPI_HAVE_ATOMIC_WIN32 1
#include "include/sys/win32/atomic.h"
#else /* only now go through this stuff */

#if   defined(__alpha__)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "alpha/atomic.h"
# endif
#elif defined(__amd64__)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "amd64/atomic.h"
# endif
#elif defined(__i386__)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "ia32/atomic.h"
# endif
#elif defined(__ia64__)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "ia64/atomic.h"
# endif
#elif defined(__POWERPC__)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "powerpc/atomic.h"
# endif
#elif defined(__sparc__) || defined(__sparc)
# define OMPI_HAVE_ATOMIC 1
# ifdef __GNUC__
#  include "sparc64/atomic.h"
# endif
#endif

#ifndef OMPI_HAVE_ATOMIC
#define OMPI_HAVE_ATOMIC 0
#endif

#if OMPI_HAVE_ATOMIC

/*
 * derived operations
 */

#if SIZEOF_INT == 4

static inline int ompi_atomic_cmpset_int(volatile int *addr,
                                         int oldval,
                                         int newval)
{
    return ompi_atomic_cmpset_32((volatile uint32_t *) addr,
                                 (uint32_t) oldval,
                                 (uint32_t) newval);
}

static inline int ompi_atomic_cmpset_acq_int(volatile int *addr,
                                             int oldval,
                                             int newval)
{
    return ompi_atomic_cmpset_acq_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval,
                                     (uint32_t) newval);
}

static inline int ompi_atomic_cmpset_rel_int(volatile int *addr,
                                             int oldval,
                                             int newval)
{
    return ompi_atomic_cmpset_rel_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval,
                                     (uint32_t) newval);
}

#elif SIZEOF_INT == 8

static inline int ompi_atomic_cmpset_int(volatile int *addr,
                                         int oldval,
                                         int newval)
{
    return ompi_atomic_cmpset_64((volatile uint64_t *) addr,
                                 (uint64_t) oldval,
                                 (uint64_t) newval);
}

static inline int ompi_atomic_cmpset_acq_int(volatile int *addr,
                                             int oldval,
                                             int newval)
{
    return ompi_atomic_cmpset_acq_64((volatile uint64_t *) addr,
                                     (uint64_t) oldval,
                                     (uint64_t) newval);
}

static inline int ompi_atomic_cmpset_rel_int(volatile int *addr,
                                             int oldval,
                                             int newval)
{
    return ompi_atomic_cmpset_rel_64((volatile uint64_t *) addr,
                                     (uint64_t) oldval,
                                     (uint64_t) newval);
}

#else

#error

#endif


#if SIZEOF_VOID_P == 4

static inline int ompi_atomic_cmpset_ptr(volatile void *addr,
                                         void *oldval,
                                         void *newval)
{
    return ompi_atomic_cmpset_32((volatile uint32_t *) addr,
                                 (uint32_t) oldval, (uint32_t) newval);
}

static inline int ompi_atomic_cmpset_acq_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval)
{
    return ompi_atomic_cmpset_acq_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval, (uint32_t) newval);
}

static inline int ompi_atomic_cmpset_rel_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval)
{
    return ompi_atomic_cmpset_rel_32((volatile uint32_t *) addr,
                                     (uint32_t) oldval, (uint32_t) newval);
}

#elif SIZEOF_VOID_P == 8

static inline int ompi_atomic_cmpset_ptr(volatile void *addr,
                                         void *oldval,
                                         void *newval)
{
    return ompi_atomic_cmpset_64((volatile uint64_t *) addr,
                                 (uint64_t) oldval,
                                 (uint64_t) newval);
}

static inline int ompi_atomic_cmpset_acq_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval)
{
    return ompi_atomic_cmpset_acq_64((volatile uint64_t *) addr,
                                     (uint64_t) oldval,
                                     (uint64_t) newval);
}

static inline int ompi_atomic_cmpset_rel_ptr(volatile void *addr,
                                             void *oldval,
                                             void *newval)
{
    return ompi_atomic_cmpset_rel_64((volatile uint64_t *) addr,
                                     (uint64_t) oldval,
                                     (uint64_t) newval);
}

#else

#error

#endif


static inline uint32_t ompi_atomic_add_32(volatile uint32_t *addr, int delta)
{
    uint32_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval + delta));
    return (oldval + delta);
}


static inline uint64_t ompi_atomic_add_64(volatile uint64_t *addr, int delta)
{
    uint64_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval + delta));
    return (oldval + delta);
}


static inline int ompi_atomic_add_int(volatile int *addr, int delta)
{
    int oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_int(addr, oldval, oldval + delta));
    return (oldval + delta);
}


static inline int ompi_atomic_fetch_and_set_int(volatile int *addr, int newval)
{
    int oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_int(addr, oldval, newval));
    return (oldval);
}

#endif /* OMPI_HAVE_ATOMIC */

#endif /* ifdef WIN32 */

#if OMPI_HAVE_ATOMIC || OMPI_HAVE_ATOMIC_WIN32

/*
 * Atomic locks
 */

/**
 * Enumeration of lock states
 */
enum {
    OMPI_ATOMIC_UNLOCKED = 0,
    OMPI_ATOMIC_LOCKED = 1
};


static inline int ompi_atomic_trylock(ompi_lock_t *lock)
{
    return ompi_atomic_cmpset_acq_int((volatile int *) lock,
                                      OMPI_ATOMIC_UNLOCKED,
                                      OMPI_ATOMIC_LOCKED);
}

static inline void ompi_atomic_lock(ompi_lock_t *lock)
{
    while (!ompi_atomic_cmpset_acq_int((volatile int *) lock,
                                       OMPI_ATOMIC_UNLOCKED,
                                       OMPI_ATOMIC_LOCKED)) {
        while (lock->u.lock == OMPI_ATOMIC_LOCKED) {
            /* spin */ ;
        }
    }
}

static inline void ompi_atomic_unlock(ompi_lock_t *lock)
{
    if (0) {
        ompi_atomic_cmpset_rel_int((volatile int *) lock,
                                   OMPI_ATOMIC_LOCKED,
                                   OMPI_ATOMIC_UNLOCKED);
    } else {
        ompi_atomic_wmb();
        lock->u.lock = OMPI_ATOMIC_UNLOCKED;
    }
}
#endif /* OMPI_HAVE_ATOMIC || OMPI_HAVE_ATOMIC_WIN32 */

#endif /* OMPI_SYS_ATOMIC_H */
