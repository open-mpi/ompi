/*
 * $HEADER$
 */

#ifndef _MUTEX_H_
#define _MUTEX_H_

#include "lam_config.h"

#if defined(USE_SPINWAIT)
#include "lam/threads/mutex_spinwait.h"
#else
#include "lam/threads/mutex_spinlock.h"
#endif

extern bool lam_uses_threads;

static inline bool lam_use_threads(void) { return lam_uses_threads; }

/*
 * Lock macros
 */
#define THREAD_LOCK(a)   if(lam_use_threads()) \
                              lam_mtx_lock((a));

#define LOCK(a)          lam_mtx_lock((a))

/*
 * unlock macros
 */
#define THREAD_UNLOCK(a) if(lam_use_threads()) \
                              lam_mtx_unlock((a));

#define UNLOCK(a)        lam_mtx_unlock((a));

#endif

