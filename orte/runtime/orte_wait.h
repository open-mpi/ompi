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
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/util/output.h"
#include "opal/event/event.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

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
 * Setup to wait for an event
 *
 * This function is used to setup a pipe that can be used elsewhere
 * in the code base where we want to wait for some event to
 * happen. For example, orterun uses this function to setup an event
 * that is used to notify orterun of abnormal and normal termination
 * so it can wakeup and exit cleanly.
 *
 * The event will be defined so that a write to the provided trigger
 * pipe will cause the event to trigger and callback to the provided
 * function
 */
ORTE_DECLSPEC int orte_wait_event(opal_event_t **event, int *trig,
                                  void (*cbfunc)(int, short, void*));

/**
 * Trigger a defined event
 *
 * This function will trigger a previously-defined event - as setup
 * by orte_wait_event - by sending a message to the provided pipe
 */
ORTE_DECLSPEC void orte_trigger_event(int trig);

/**
 * Delayed triggering of a defined event
 *
 * Sometimes, we need to trigger an event, but not until we return
 * from a current function. For example, if we are in an OOB recv
 * callback, we don't really want to trigger an event that will
 * do an OOB send as this can get us into a loopback situation.
 * This function will setup a separate timed event such that
 * the specified event can be triggered as soon as the recv
 * callback is completed
 */
ORTE_DECLSPEC void orte_delayed_trigger_event(int trig);

/**
 * In a number of places within the code, we want to setup a timer
 * to detect when some procedure failed to complete. For example,
 * when we launch the daemons, we frequently have no way to directly
 * detect that a daemon failed to launch. Setting a timer allows us
 * to automatically fail out of the launch if we don't hear from a
 * daemon in some specified time window.
 *
 * Computing the amount of time to wait takes a few lines of code, but
 * this macro encapsulates those lines along with the timer event
 * definition just as a convenience. It also centralizes the
 * necessary checks to ensure that the microsecond field is always
 * less than 1M since some systems care about that, and to ensure
 * that the computed wait time doesn't exceed the desired max
 * wait
 */
#define ORTE_DETECT_TIMEOUT(event, n, deltat, maxwait, cbfunc)      \
    do {                                                            \
        struct timeval now;                                         \
        opal_event_t *tmp;                                          \
        tmp = (opal_event_t*)malloc(sizeof(opal_event_t));          \
        opal_evtimer_set(tmp, (cbfunc), NULL);                      \
        now.tv_sec = 0;                                             \
        now.tv_usec = (float)(deltat) * (float)(n);                 \
        if (now.tv_usec > (maxwait)) {                              \
            now.tv_usec = (maxwait);                                \
        }                                                           \
        if (now.tv_usec > 1000000.0) {                              \
            now.tv_sec = (float)((int)(now.tv_usec/1000000.0));     \
            now.tv_usec = now.tv_usec - 1000000.0*now.tv_sec;       \
        }                                                           \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,                  \
                             "defining timeout: %ld sec %ld usec",  \
                            (long)now.tv_sec, (long)now.tv_usec));  \
        opal_evtimer_add(tmp, &now);                                \
        *(event) = tmp;                                             \
    }while(0);                                                      \

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

END_C_DECLS

#endif /* #ifndef ORTE_WAIT_H */
