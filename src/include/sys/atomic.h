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
static inline void ompi_atomic_mb(void);


/**
 * Read memory barrier
 */
static inline void ompi_atomic_rmb(void);


/**
 * Write memory barrier.
 */
static inline void ompi_atomic_wmb(void);

#if 0
/**
 * Atomically add to an integer.
 *
 * @param addr          Address of integer.
 * @param newval        Value to set.
 * @return              Old value of integer.
 */
static inline int ompi_atomic_fetch_and_set_int(volatile void *addr, int newval);
#endif

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
#else
#error No atomic operations defined yet
#endif
#endif  /* ifdef WIN32*/

#ifndef OMPI_HAVE_ATOMIC
#define OMPI_HAVE_ATOMIC 0
#endif

/* All the architectures provide a compare_and_set atomic operations. If
 * they dont provide atomic additions and/or substractions then we can
 * define these operations using the atomic compare_and_set.
 */

#if !defined(OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_32)
static inline int32_t ompi_atomic_add_32(volatile int32_t *addr, int delta)
{
   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_32 */

#if !defined(OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_32)
static inline int32_t ompi_atomic_sub_32(volatile int32_t *addr, int delta)

   int32_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval - delta));
   return (oldval - delta);
}
#endif  /* OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_32 */

#if !defined(OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_64)
static inline int64_t ompi_atomic_add_64(volatile int64_t *addr, int delta)
{
   int64_t oldval;
   
   do {
      oldval = *addr;
   } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval + delta));
   return (oldval + delta);
}
#endif  /* OMPI_ARCHITECTURE_DEFINE_ATOMIC_ADD_64 */

#if !defined(OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_64)
static inline int64_t ompi_atomic_sub_64(volatile int64_t *addr, int delta)
{
    int64_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval - delta));
    return (oldval - delta);
}
#endif  /* OMPI_ARCHITECTURE_DEFINE_ATOMIC_SUB_64 */

static inline int ompi_atomic_cmpset_xx( volatile void* addr, int64_t oldval,
                                         int64_t newval, size_t length )
{
   switch( length ) {
   case 4:
      return ompi_atomic_cmpset_32( (volatile int32_t*)addr,
                                    (int32_t)oldval, (int32_t)newval );
   case 8:
      return ompi_atomic_cmpset_64( (volatile int64_t*)addr,
                                    (int64_t)oldval, (int64_t)newval );
   default:
      *(int*)(NULL) = 0;
   }
   return 0;  /* always fail */
}

/**
 * Atomic compare and set of pointer with relaxed semantics. This
 * macro detect at compile time the type of the first argument 
 * and choose the correct function to be called.
 *
 * @param addr          Address of <TYPE>.
 * @param oldval        Comparison value <TYPE>.
 * @param newval        New value to set if comparision is true <TYPE>.
 *
 * See ompi_atomic_cmpset_* for pseudo-code.
 */
#define ompi_atomic_cmpset( ADDR, OLDVAL, NEWVAL )                  \
   ompi_atomic_cmpset_xx( (volatile void*)(ADDR), (int64_t)(OLDVAL), (int64_t)(NEWVAL), sizeof(*(ADDR)) )

static inline int ompi_atomic_cmpset_acq_xx( volatile void* addr, int64_t oldval,
                                             int64_t newval, size_t length )
{
   switch( length ) {
   case 4:
      return ompi_atomic_cmpset_acq_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
   case 8:
      return ompi_atomic_cmpset_acq_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
   default:
      *(int*)(NULL) = 0;
   }
   return 0;  /* always fail */
}

/**
 * Atomic compare and set of pointer with acquire semantics. This
 * macro detect at compile time the type of the first argument 
 * and choose the correct function to be called.
 *
 * @param addr          Address of <TYPE>.
 * @param oldval        Comparison value <TYPE>.
 * @param newval        New value to set if comparision is true <TYPE>.
 *
 * See ompi_atomic_cmpset_acq_* for pseudo-code.
 */
#define ompi_atomic_cmpset_acq( ADDR, OLDVAL, NEWVAL )           \
   ompi_atomic_cmpset_acq_xx( (volatile void*)(ADDR), (int64_t)(OLDVAL), (int64_t)(NEWVAL), sizeof(*(ADDR)) )

static inline int ompi_atomic_cmpset_rel_xx( volatile void* addr, int64_t oldval,
                                             int64_t newval, size_t length )
{
   switch( length ) {
   case 4:
      return ompi_atomic_cmpset_rel_32( (volatile int32_t*)addr,
                                        (int32_t)oldval, (int32_t)newval );
   case 8:
      return ompi_atomic_cmpset_rel_64( (volatile int64_t*)addr,
                                        (int64_t)oldval, (int64_t)newval );
   default:
      *(int*)(NULL) = 0;
   }
   return 0;  /* always fail */
}

/**
 * Atomic compare and set of pointer with release semantics. This
 * macro detect at compile time the type of the first argument 
 * and choose the correct function to b
 *
 * @param addr          Address of <TYPE>.
 * @param oldval        Comparison value <TYPE>.
 * @param newval        New value to set if comparision is true <TYPE>.
 *
 * See ompi_atomic_cmpsetrel_* for pseudo-code.
 */
#define ompi_atomic_cmpset_rel( ADDR, OLDVAL, NEWVAL )           \
   ompi_atomic_cmpset_rel_xx( (volatile void*)(ADDR), (int64_t)(OLDVAL), (int64_t)(NEWVAL), sizeof(*(ADDR)) )

static inline void ompi_atomic_add_xx( volatile void* addr, int32_t value, size_t length )
{
   switch( length ) {
   case 4:
      ompi_atomic_add_32( (volatile int32_t*)addr, (int32_t)value );
      break;
   case 8:
      ompi_atomic_add_64( (volatile int64_t*)addr, (int64_t)value );
      break;
   default:
      *(int*)(NULL) = 0;
   }
}

/**
 * Atomically increment the content depending on the type. This
 * macro detect at compile time the type of the first argument 
 * and choose the correct function to be called.
 *
 * @param addr          Address of <TYPE>
 * @param delta         Value to add (converted to <TYPE>).
 */
#define ompi_atomic_add( ADDR, VALUE )                                  \
   ompi_atomic_add_xx( (volatile void*)(ADDR), (int32_t)(VALUE), sizeof(*(ADDR)) )

static inline void ompi_atomic_sub_xx( volatile void* addr, int32_t value, size_t length )
{
   switch( length ) {
   case 4:
      ompi_atomic_sub_32( (volatile int32_t*)addr, (int32_t)value );
      break;
   case 8:
      ompi_atomic_sub_64( (volatile int64_t*)addr, (int64_t)value );
      break;
   default:
      *(int*)(NULL) = 0;
   }
}

/**
 * Atomically decrement the content depending on the type. This
 * macro detect at compile time the type of the first argument 
 * and choose the correct function to be called.
 *
 * @param addr          Address of <TYPE>
 * @param delta         Value to substract (converted to <TYPE>).
 */
#define ompi_atomic_sub( ADDR, VALUE )                                  \
   ompi_atomic_sub_xx( (volatile void*)(ADDR), (int32_t)(VALUE), sizeof(*(ADDR)) )

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
   ompi_atomic_cmpset_acq((volatile int*) lock,
                          OMPI_ATOMIC_UNLOCKED,
                          OMPI_ATOMIC_LOCKED);
   return lock->u.lock;
}

static inline void ompi_atomic_lock(ompi_lock_t *lock)
{
   while( !ompi_atomic_cmpset_acq((volatile int *) lock,
                                  OMPI_ATOMIC_UNLOCKED,
                                  OMPI_ATOMIC_LOCKED) ) {
      while (lock->u.lock == OMPI_ATOMIC_LOCKED) {
         /* spin */ ;
      }
   }
}

static inline void ompi_atomic_unlock(ompi_lock_t *lock)
{
   ompi_atomic_cmpset_rel((volatile int *) lock,
                          OMPI_ATOMIC_LOCKED,
                          OMPI_ATOMIC_UNLOCKED);
}
#endif /* OMPI_HAVE_ATOMIC || OMPI_HAVE_ATOMIC_WIN32 */

#endif /* OMPI_SYS_ATOMIC_H */
