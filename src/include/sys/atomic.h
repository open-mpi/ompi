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

#ifndef OMPI_ATOMIC_H
#define OMPI_ATOMIC_H 1

#include "ompi_config.h"

#ifdef __GNUC__
#define STATIC_INLINE static inline
#else
#define STATIC_INLINE
#endif

/*
 * prototypes
 */

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
STATIC_INLINE uint32_t ompi_atomic_add_32(uint32_t *addr, int delta);


/**
 * Atomically add to a 64-bit integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
STATIC_INLINE uint64_t ompi_atomic_add_64(uint64_t *addr, int delta);


/**
 * Atomically add to an integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
STATIC_INLINE int ompi_atomic_add_int(int *addr, int delta);
 

#ifdef __GNUC__

/*
 * Include system specific inline asm definitions. Otherwise
 * the definitions are in system specific .s files in src/util.
 */

#if   defined(__alpha__)
# include "sys/alpha/atomic.h"
#elif defined(__amd64__)
# include "sys/amd64/atomic.h"
#elif defined(__i386__)
# include "sys/ia32/atomic.h"
#elif defined(__ia64__)
# include "sys/ia64/atomic.h"
#elif defined(__POWERPC__)
# include "sys/powerpc/atomic.h"
#elif defined(__sparc__) || defined(__sparc)
# include "sys/sparc64/atomic.h"
#endif

#endif


/*
 * implementation (derived)
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


static inline uint32_t ompi_atomic_add_32(uint32_t *addr, int delta)
{
    uint32_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_32(addr, oldval, oldval + delta));
    return (oldval + delta);
}


static inline uint64_t ompi_atomic_add_64(uint64_t *addr, int delta)
{
    uint64_t oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_64(addr, oldval, oldval + delta));
    return (oldval + delta);
}


static inline int ompi_atomic_add_int(int *addr, int delta)
{
    int oldval;

    do {
        oldval = *addr;
    } while (0 == ompi_atomic_cmpset_int(addr, oldval, oldval + delta));
    return (oldval + delta);
}

#endif /* OMPI_ATOMIC_H */
