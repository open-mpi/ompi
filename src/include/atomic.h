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

#ifndef LAM_ATOMIC_H
#define LAM_ATOMIC_H 1

#include "lam_config.h"

#if 0

/*
 * prototypes
 */

/**
 * Atomic compare and set of unsigned 32-bit integer.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * Pseudo-code:
 *
 * @code
 *   int lam_atomic_cmpset_acq_32(addr, cmp, new)
 *   {
 *       if (*addr == cmp) {
 *           *addr = new;
 *           return 1;
 *       } else {
 *           return 0;
 *       }
 *   }
 * @endcode
 */
static inline int lam_atomic_cmpset_32(volatile uint32_t *addr,
                                       uint32_t cmp,
                                       uint32_t new);


/**
 * Atomic compare and set of unsigned 32-bit integer with acquire
 * semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_32(volatile uint32_t *addr,
                                           uint32_t cmp,
                                           uint32_t new);


/**
 * Atomic compare and set of unsigned 32-bit integer with release
 * semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_rel_32(volatile uint32_t *addr,
                                           uint32_t cmp,
                                           uint32_t new);


/**
 * Atomic compare and set of unsigned 64-bit integer.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t cmp,
                                           uint64_t new);


/**
 * Atomic compare and set of unsigned 64-bit integer with acquire
 * semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_64(volatile uint64_t *addr,
                                           uint64_t cmp,
                                           uint64_t new);


/**
 * Atomic compare and set of unsigned 64-bit integer with release
 * semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_rel_64(volatile uint64_t *addr,
                                           uint64_t cmp,
                                           uint64_t new);


/**
 * Atomic compare and set of integer.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_int(volatile int *addr,
                                            int cmp,
                                            int new);


/**
 * Atomic compare and set of integer with acquire semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_int(volatile int *addr,
                                            int cmp,
                                            int new);


/**
 * Atomic compare and set of integer with release semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_rel_int(volatile int *addr,
                                            int cmp,
                                            int new);


/**
 * Atomic compare and set of pointer.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_ptr(volatile void *addr,
                                        void *cmp,
                                        void *new);


/**
 * Atomic compare and set of pointer with acquire semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_acq_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new);


/**
 * Atomic compare and set of pointer with release semantics.
 *
 * @param addr          Address of integer.
 * @param cmp           Comparison value.
 * @param new           New value to set if comparision is true.
 *
 * See lam_atomic_cmpset_32 for pseudo-code.
 */
static inline int lam_atomic_cmpset_rel_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new);

/**
 * Atomically add to a 32-bit integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline uint32_t lam_atomic_add_32(uint32_t *addr, int delta);


/**
 * Atomically add to a 64-bit integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline uint64_t lam_atomic_add_64(uint64_t *addr, int delta);


/**
 * Atomically add to an integer.
 *
 * @param addr          Address of integer.
 * @param delta         Value to add.
 * @return              New value of integer.
 */
static inline int lam_atomic_add_int(int *addr, int delta);
 

/*
 * implementation (system specific)
 */

#if defined (__GNUC__)

#if   defined(__alpha__)
# include "sys/alpha/atomic.h"
# define LAM_ATOMIC_OPS 1
#elif defined(__amd64__)
# include "sys/amd64/atomic.h"
# define LAM_ATOMIC_OPS 1
#elif defined(__i386__)
# define LAM_ATOMIC_OPS 1
# include "sys/ia32/atomic.h"
# define LAM_ATOMIC_OPS 1
#elif defined(__ia64__)
# include "sys/ia64/atomic.h"
# define LAM_ATOMIC_OPS 1
#elif defined(__powerpc__)
# include "sys/powerpc/atomic.h"
# define LAM_ATOMIC_OPS 1
#elif defined(__sparc64__)
# include "sys/sparc/atomic.h"
# define LAM_ATOMIC_OPS 1
#endif

#endif


/*
 * implementation (derived)
 */

#if SIZEOF_INT == 4

static inline int lam_atomic_cmpset_int(volatile int *addr,
                                        int cmp,
                                        int new)
{
    return lam_atomic_cmpset_32((volatile uint32_t *) addr,
                                (uint32_t) cmp,
                                (uint32_t) new);
}

static inline int lam_atomic_cmpset_acq_int(volatile int *addr,
                                            int cmp,
                                            int new)
{
    return lam_atomic_cmpset_acq_32((volatile uint32_t *) addr,
                                    (uint32_t) cmp,
                                    (uint32_t) new);
}

static inline int lam_atomic_cmpset_rel_int(volatile int *addr,
                                            int cmp,
                                            int new)
{
    return lam_atomic_cmpset_rel_32((volatile uint32_t *) addr,
                                    (uint32_t) cmp,
                                    (uint32_t) new);
}

#elif SIZEOF_INT == 8

static inline int lam_atomic_cmpset_int(volatile int *addr,
                                        int cmp,
                                        int new)
{
    return lam_atomic_cmpset_64((volatile uint64_t *) addr,
                                (uint64_t) cmp,
                                (uint64_t) new);
}

static inline int lam_atomic_cmpset_acq_int(volatile int *addr,
                                            int cmp,
                                            int new)
{
    return lam_atomic_cmpset_acq_64((volatile uint64_t *) addr,
                                    (uint64_t) cmp,
                                    (uint64_t) new);
}

static inline int lam_atomic_cmpset_rel_int(volatile int *addr,
                                            int cmp,
                                            int new)
{
    return lam_atomic_cmpset_rel_64((volatile uint64_t *) addr,
                                    (uint64_t) cmp,
                                    (uint64_t) new);
}

#else 

#error

#endif


#if SIZEOF_VOID_P == 4

static inline int lam_atomic_cmpset_ptr(volatile void *addr,
                                        void *cmp,
                                        void *new)
{
    return lam_atomic_cmpset_32((volatile uint32_t *) addr,
                                (uint32_t) cmp, (uint32_t) new);
}

static inline int lam_atomic_cmpset_acq_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new)
{
    return lam_atomic_cmpset_acq_32((volatile uint32_t *) addr,
                                    (uint32_t) cmp, (uint32_t) new);
}

static inline int lam_atomic_cmpset_rel_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new)
{
    return lam_atomic_cmpset_rel_32((volatile uint32_t *) addr,
                                    (uint32_t) cmp, (uint32_t) new);
}

#elif SIZEOF_VOID_P == 8

static inline int lam_atomic_cmpset_ptr(volatile void *addr,
                                        void *cmp,
                                        void *new)
{
    return lam_atomic_cmpset_64((volatile uint64_t *) addr,
                                (uint64_t) cmp,
                                (uint64_t) new);
}

static inline int lam_atomic_cmpset_acq_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new)
{
    return lam_atomic_cmpset_acq_64((volatile uint64_t *) addr,
                                    (uint64_t) cmp,
                                    (uint64_t) new);
}

static inline int lam_atomic_cmpset_rel_ptr(volatile void *addr,
                                            void *cmp,
                                            void *new)
{
    return lam_atomic_cmpset_rel_64((volatile uint64_t *) addr,
                                    (uint64_t) cmp,
                                    (uint64_t) new);
}

#else

#error

#endif


static inline uint32_t lam_atomic_add_32(uint32_t *addr, int delta)
{
    uint32_t old;

    do {
        old = *addr;
    } while (0 == lam_atomic_cmpset_32(addr, old, old + delta));
    return (old + delta);
}


static inline uint64_t lam_atomic_add_64(uint64_t *addr, int delta)
{
    uint64_t old;

    do {
        old = *addr;
    } while (0 == lam_atomic_cmpset_64(addr, old, old + delta));
    return (old + delta);
}


static inline int lam_atomic_add_int(int *addr, int delta)
{
    int old;

    do {
        old = *addr;
    } while (0 == lam_atomic_cmpset_int(addr, old, old + delta));
    return (old + delta);
}

#endif

#endif /* LAM_ATOMIC_H */
