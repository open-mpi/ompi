/*
 * $HEADER$
 */

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
} ompi_lock_data_t;

CDECL_BEGIN

static inline void spinunlock(ompi_lock_data_t *ctlData_m)
{
    ctlData_m->data.lockData_m = LOCK_UNLOCKED;
}

void spinlock(ompi_lock_data_t *);
int spintrylock(ompi_lock_data_t *);
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
