/*
 * $HEADER$
 */

#ifndef _MUTEX_H_
#define _MUTEX_H_

#if defined(USE_SPINWAIT)
#include "lam/threads/mutex_spinwait.h"
#else
#include "lam/threads/mutex_spinlock.h"
#endif
#endif

