/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_PROC_MUTEX_H_INCLUDED
#define MPL_PROC_MUTEX_H_INCLUDED

#include "mplconfig.h"

/* Define interprocess mutex interface.
 *
 * The user should always call MPL_proc_mutex_enabled(void) to check if
 * the functionality of interprocess mutex is enabled before use.
 * It returns 1 if the functionality is enabled, otherwise returns 0.
 */

/* _INVALID exists to avoid accidental macro evaluations to 0 */
#define MPL_PROC_MUTEX_PACKAGE_INVALID 0
#define MPL_PROC_MUTEX_PACKAGE_NONE    1
#define MPL_PROC_MUTEX_PACKAGE_POSIX   2

#if defined(MPL_PROC_MUTEX_PACKAGE_NAME) && (MPL_PROC_MUTEX_PACKAGE_NAME == MPL_PROC_MUTEX_PACKAGE_POSIX)
#include "mpl_proc_mutex_posix.h"
#elif defined(MPL_PROC_MUTEX_PACKAGE_NAME) && (MPL_PROC_MUTEX_PACKAGE_NAME == MPL_PROC_MUTEX_PACKAGE_NONE)
typedef int MPL_proc_mutex_t;
static inline int MPL_proc_mutex_enabled(void)
{
    return 0;   /* always disabled */
}

#define MPL_proc_mutex_create(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = MPL_ERR_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_destroy(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = MPL_ERR_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_lock(mutex_ptr_, err_ptr_)  { *((int*)err_ptr_) = MPL_ERR_PROC_MUTEX_EINVAL;}
#define MPL_proc_mutex_unlock(mutex_ptr_, err_ptr_) { *((int*)err_ptr_) = MPL_ERR_PROC_MUTEX_EINVAL;}
#else
#error "proc package (MPL_PROC_MUTEX_PACKAGE_NAME) not defined or unknown"
#endif

#endif /* MPL_PROC_MUTEX_H_INCLUDED */
