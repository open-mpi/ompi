/*
 * $HEADER$
 */
#ifndef LAM_CONDITION_H
#define LAM_CONDITION_H

#include "lam_config.h"
#include "lam/threads/mutex.h"

#if defined(LAM_USE_SPINLOCK)
#include "condition_spinlock.h"
#elif defined(LAM_USE_SPINWAIT)
#include "condition_spinwait.h"
#elif defined(LAM_USE_PTHREADS)
#include "condition_pthread.h"
#else
#error "concurrency model not configured"
#endif

#endif
