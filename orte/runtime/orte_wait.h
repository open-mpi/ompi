/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for waitpid / async notification of child death with the
 * libevent runtime system.
 */
#ifndef ORTE_WAIT_H
#define ORTE_WAIT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/** typedef for callback function used in \c ompi_rte_wait_cb */
typedef void (*orte_wait_fn_t)(pid_t wpid, int status, void *data);


/**
 * Wait for process terminiation
 *
 * Similar to \c waitpid, \c orte_waitpid utilizes the run-time
 * event library for process terminiation notification.  The \c
 * WUNTRACED option is not supported, but the \c WNOHANG option is
 * supported.
 *
 * \note A \c wpid value of \c -1 is not currently supported and will
 * return an error.
 */
ORTE_DECLSPEC pid_t orte_waitpid(pid_t wpid, int *status, int options);


/**
 * Register a callback for process termination
 *
 * Register a callback for notification when \c wpid causes a SIGCHLD.
 * \c waitpid() will have already been called on the process at this
 * time.  
 *
 * If a thread is already blocked in \c ompi_rte_waitpid for \c wpid,
 * this function will return \c ORTE_ERR_EXISTS.  It is illegal for
 * multiple callbacks to be registered for a single \c wpid
 * (OMPI_EXISTS will be returned in this case).
 *
 * \warning It is not legal for \c wpid to be -1 when registering a
 * callback.
 */
ORTE_DECLSPEC int orte_wait_cb(pid_t wpid, orte_wait_fn_t callback, void *data);

ORTE_DECLSPEC int orte_wait_cb_cancel(pid_t wpid);

ORTE_DECLSPEC int orte_wait_cb_disable(void);

ORTE_DECLSPEC int orte_wait_cb_enable(void);

/**
 * \internal
 *
 * Initialize the wait system (allocate mutexes, etc.)
 */
ORTE_DECLSPEC int orte_wait_init(void);

/**
 * Kill all processes we are waiting on.
 */
ORTE_DECLSPEC int orte_wait_kill(int sig);

/**
 * \internal
 *
 * Finalize the wait system (deallocate mutexes, etc.)
 */
ORTE_DECLSPEC int orte_wait_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* #ifndef ORTE_WAIT_H */
