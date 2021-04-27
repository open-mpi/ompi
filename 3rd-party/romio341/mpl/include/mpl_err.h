/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ERR_H_INCLUDED
#define MPL_ERR_H_INCLUDED

/* this file defines all the return code all the MPL components */

enum {
    MPL_SUCCESS = 0,
    MPL_ERR_THREAD,
    MPL_ERR_PROC_MUTEX_EINTR,
    MPL_ERR_PROC_MUTEX_EINVAL,
    MPL_ERR_STR_FAIL,
    MPL_ERR_STR_NOMEM,
    MPL_ERR_STR_TRUNCATED,
    MPL_ERR_DBG_INTERN,
    MPL_ERR_DBG_OTHER,
    MPL_ERR_SHM_INTERN,
    MPL_ERR_SHM_INVAL,
    MPL_ERR_SHM_NOMEM,
    /* The timer code is allowed to return "NOT_INITIALIZED" before it is
     * initialized.  Once it is initialized, it must always return
     * SUCCESS, so the upper layers do not need to check for the return
     * code.  */
    MPL_ERR_TIMER_NOT_INITIALIZED,
    MPL_ERR_GPU_INTERNAL,
    MPL_ERR_GPU_NOMEM,
    MPL_ERR_NOMEM
};

#endif /* MPL_ERR_H_INCLUDED */
