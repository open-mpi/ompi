/*
 * $HEADER$
 */
#ifndef LAM_CONDITION_H
#define LAM_CONDITION_H

#include "lam_config.h"

#if LAM_HAVE_POSIX_THREADS
#include "condition_pthread.h"
#else
#include "condition_spinlock.h"
#endif

#endif
