/*
 * $HEADER$
 */

#ifndef _MUTEX_SPINLOCK_
#define _MUTEX_SPINLOCK_

#include "lam/os/atomic.h"
typedef lam_lock_data_t lam_mutex_t;

#define lam_mutex_init(m)    spinunlock(m)
#define lam_mutex_lock(m)    spinlock(m)
#define lam_mutex_trylock(m) spintrylock(m)
#define lam_mutex_unlock(m)  spinunlock(m)

#endif

