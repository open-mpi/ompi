/*
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for waitpid / async notification of child death with the
 * libevent runtime system.
 */
#ifndef OMPI_RTE_WAIT_H
#define OMPI_RTE_WAIT_H

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

/** typedef for callback function used in \c ompi_rte_wait_cb */
typedef void (*ompi_rte_wait_fn_t)(pid_t wpid, int status, void *data);


/**
 * Wait for process terminiation
 *
 * Similar to \c waitpid, \c ompi_rte_waitpid utilizes the run-time
 * event library for process terminiation notification.  The \c
 * WUNTRACED option is not supported, but the \c WNOHANG option is
 * supported.
 *
 * \note A \c wpid value of \c -1 is not currently supported and will
 * return an error.
 */
pid_t ompi_rte_waitpid(pid_t wpid, int *status, int options);


/**
 * Register a callback for process termination
 *
 * Register a callback for notification when \c wpid causes a SIGCHLD.
 * \c waitpid() will have already been called on the process at this
 * time.  
 *
 * If a thread is already blocked in \c ompi_rte_waitpid for \c wpid,
 * this function will return \c OMPI_ERR_EXISTS.  It is legal for
 * multiple callbacks to be registered for a single \c wpid.
 *
 * \warning It is not legal for \c wpid to be -1 when registering a
 * callback.
 */
int ompi_rte_wait_cb(pid_t wpid, ompi_rte_wait_fn_t callback, void *data);

int ompi_rte_wait_cb_cancel(pid_t wpid);

int ompi_rte_wait_cb_disable(void);

int ompi_rte_wait_cb_enable(void);

/**
 * \internal
 *
 * Initialize the wait system (allocate mutexes, etc.)
 */
int ompi_rte_wait_init(void);


/**
 * \internal
 *
 * Finalize the wait system (deallocate mutexes, etc.)
 */
int ompi_rte_wait_finalize(void);


#endif
