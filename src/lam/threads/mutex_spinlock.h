/*
 * $HEADER$
 */

#ifndef _MUTEX_SPINLOCK_
#define _MUTEX_SPINLOCK_

#include "lam/os/atomic.h"
typedef lam_lock_data_t lam_mutex_t;

#define lam_mtx_init(m)    spinunlock(m)
#define lam_mtx_lock(m)    spinlock(m)
#define lam_mtx_trylock(m) spintrylock(m)
#define lam_mtx_unlock(m)  spinunlock(m)

#endif

