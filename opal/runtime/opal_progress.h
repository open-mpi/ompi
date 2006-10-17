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
 * Progress engine for Open MPI
 */

#ifndef _OMPI_PROGRESS_H_
#define _OMPI_PROGRESS_H_
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
#include "opal/threads/mutex.h"

/**
 * Initialize the progress engine
 *
 * Initialize the progress engine, including constructing the 
 * proper locks and allocating space for the progress registration
 * functions.  At this point, any function in the progress engine
 * interface may be called.
 */
OPAL_DECLSPEC int opal_progress_init(void);

/**
 * Configure the progress engine for executing MPI applications
 *
 * Register to receive any needed information from the GPR and 
 * intialize any data structures required for MPI applications.
 *
 * \note opal_progress_init() must be called before calling
 * this function.  Failure to do so is an error.
 */
OPAL_DECLSPEC int opal_progress_mpi_init(void);

/** 
 * Turn on optimizations for MPI progress
 *
 * Turn on optimizations for MPI applications.  This includes lowering
 * the rate at which the event library is ticked if it is not under
 * active use and possibly disabling the sched_yield call when the
 * progress engine is idle 
 */
OPAL_DECLSPEC int opal_progress_mpi_enable(void);

/**
 * Turn off all optimizations enabled by opal_progress_mpi_enable().
 *
 * Completely reverses all optimizations enabled by
 * opal_progress_mpi_enable().  The event library resumes constant
 * ticking and the progress engine yields the CPU when idle.
 */
OPAL_DECLSPEC int opal_progress_mpi_disable(void);

/** 
 * Shut down the progress engine
 *
 * Shut down the progress engine.  This includes deregistering all
 * registered callbacks and freeing all resources.  After finalize
 * returns, no calls into the progress interface are allowed.
 */
OPAL_DECLSPEC int opal_progress_finalize(void);

/**
 * Control how the event library is called
 */
OPAL_DECLSPEC void opal_progress_events(int);

/**
 * Progress all pending events
 */
OPAL_DECLSPEC void opal_progress(void);

typedef int (*opal_progress_callback_t)(void);

/**
 * Register an event to be progressed
 */
OPAL_DECLSPEC int opal_progress_register(opal_progress_callback_t cb);


/**
 * Unregister previously registered event
 */
OPAL_DECLSPEC int opal_progress_unregister(opal_progress_callback_t cb);


/**
 * Increase count of MPI users of the event library
 */   
OPAL_DECLSPEC int opal_progress_event_increment(void);

/**
 * Decrease count of MPI users of the event library
 */   
OPAL_DECLSPEC int opal_progress_event_decrement(void);


/**
 * Progress until flag is true or poll iterations completed
 */

OPAL_DECLSPEC extern volatile int32_t opal_progress_thread_count;
OPAL_DECLSPEC extern int opal_progress_spin_count;

static inline bool opal_progress_threads(void) 
{ 
    return (opal_progress_thread_count > 0); 
}


static inline bool opal_progress_spin(volatile bool* complete)
{
    int32_t c;
    OPAL_THREAD_ADD32(&opal_progress_thread_count,1);
    for (c = 0; c < opal_progress_spin_count; c++) {
        if (true == *complete) {
             OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
             return true;
        }
        opal_progress();
    }
    OPAL_THREAD_ADD32(&opal_progress_thread_count,-1);
    return false;
}


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

