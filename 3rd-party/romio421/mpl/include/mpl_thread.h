/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
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

#define MPL_THREAD_PRIO_HIGH 0
#define MPL_THREAD_PRIO_LOW 1

#if !defined MPL_THREAD_PACKAGE_NAME
#error "thread package (MPL_THREAD_PACKAGE_NAME) not defined or unknown"
#elif MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_POSIX || MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_UTI
#include "mpl_thread_posix.h"
#elif MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_SOLARIS
#include "mpl_thread_solaris.h"
#elif MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_WIN
#include "mpl_thread_win.h"
#elif MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_ARGOBOTS
#include "mpl_thread_argobots.h"
#elif MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_NONE
typedef int MPL_thread_mutex_t;
typedef int MPL_thread_cond_t;
typedef int MPL_thread_id_t;
typedef int MPL_thread_tls_key_t;
typedef void (*MPL_thread_func_t) (void *data);
#define MPL_thread_mutex_create(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = 0;}
#define MPL_thread_mutex_destroy(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = 0;}
#define MPL_thread_init(err_ptr_)      do { *((int*)err_ptr_) = 0;} while (0)
#define MPL_thread_finalize(err_ptr_)  do { *((int*)err_ptr_) = 0;} while (0)
#define MPL_thread_mutex_lock(mutex_ptr_, err_ptr_, prio_)   do { } while (0)
#define MPL_thread_mutex_unlock(mutex_ptr_, err_ptr_)        do { } while (0)
#define MPL_thread_yield()             do { } while (0)
#else
#error "thread package (MPL_THREAD_PACKAGE_NAME) not defined or unknown"
#endif

/* If the semantics of TLS is optional, i.e. collision or even corruption is ok,
 * MPL_TLS is always defined and can be used directly.
 * When the TLS semantics is critical, always check MPL_HAS_TLS and supply work-
 * around.
 */
#undef MPL_HAS_TLS

#if MPL_THREAD_PACKAGE_NAME == MPL_THREAD_PACKAGE_NONE
#define MPL_TLS /* empty */
#define MPL_HAS_TLS 1

#elif defined(MPL_COMPILER_TLS)
#define MPL_TLS MPL_COMPILER_TLS
/* Thread package such as argobots may define MPL_NO_COMPILER_TLS to indicate that
 * compiler native tls (e.g. __thread) should not be used.
 */
#if !defined(MPL_NO_COMPILER_TLS)
#define MPL_HAS_TLS 1
#endif

#else
/* MPL_TLS is still defined so it can be used where TLS semantics is optional.
 * When TLS semantics is critical, guard MPL_TLS usage with #ifdef MPL_HAS_TLS */
#define MPL_TLS /* empty */

#endif

#include "mpl_thread_priv.h"

#endif /* MPL_THREAD_H_INCLUDED */
