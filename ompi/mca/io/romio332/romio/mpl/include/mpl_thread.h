/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_THREAD_H_INCLUDED
#define MPL_THREAD_H_INCLUDED

#include "mplconfig.h"

/* _INVALID exists to avoid accidental macro evaluations to 0 */
#define MPL_THREAD_PACKAGE_INVALID 0
#define MPL_THREAD_PACKAGE_NONE    1
#define MPL_THREAD_PACKAGE_POSIX   2
#define MPL_THREAD_PACKAGE_SOLARIS 3
#define MPL_THREAD_PACKAGE_WIN     4
#define MPL_THREAD_PACKAGE_UTI     5
#define MPL_THREAD_PACKAGE_ARGOBOTS 6

#if defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_POSIX || MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_UTI)
#include "mpl_thread_posix.h"
#elif defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_SOLARIS)
#include "mpl_thread_solaris.h"
#elif defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_WIN)
#include "mpl_thread_win.h"
#elif defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_ARGOBOTS)
#include "mpl_thread_argobots.h"
#elif defined(MPL_THREAD_PACKAGE_NAME) && (MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_NONE)
typedef int MPL_thread_mutex_t;
typedef int MPL_thread_cond_t;
typedef int MPL_thread_id_t;
typedef int MPL_thread_tls_t;
typedef void (*MPL_thread_func_t) (void *data);
#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = 0;}
#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = 0;}
#else
#error "thread package (MPL_THREAD_PACKAGE_NAME) not defined or unknown"
#endif

/* Error values */
#define MPL_THREAD_SUCCESS 0
#define MPL_THREAD_ERROR   1
/* FIXME: Define other error codes.  For now, any non-zero value is an error. */

#include "mpl_thread_priv.h"

#endif /* MPL_THREAD_H_INCLUDED */
