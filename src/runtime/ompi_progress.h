/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef _OMPI_PROGRESS_H_
#define _OMPI_PROGRESS_H_
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Initialize the progress engine
 *
 * Initialize the progress engine, including constructing the 
 * proper locks and allocating space for the progress registration
 * functions.  At this point, any function in the progress engine
 * interface may be called.
 */
OMPI_DECLSPEC extern int ompi_progress_init(void);

/**
 * Configure the progress engine for executing MPI applications
 *
 * Register to receive any needed information from the GPR and 
 * intialize any data structures required for MPI applications.
 */
OMPI_DECLSPEC extern int ompi_progress_mpi_init(void);

/** 
 * Turn on optimizations for MPI progress
 */
OMPI_DECLSPEC extern int ompi_progress_mpi_enable(void);

/**
 * Turn off all optimizations enabled by ompi_progress_mpi_enable().
 */
OMPI_DECLSPEC extern int ompi_progress_mpi_disable(void);

/** 
 * Shut down the progress engine
 *
 * Shut down the progress engine.  This includes deregistering all
 * registered callbacks and freeing all resources.  After finalize
 * returns, no calls into the progress interface are allowed.
 */
OMPI_DECLSPEC extern int ompi_progress_finalize(void);

/**
 * Control how the event library is called
 */
OMPI_DECLSPEC extern void ompi_progress_events(int);

/**
 * Progress all pending events
 */
OMPI_DECLSPEC extern void ompi_progress(void);

typedef int (*ompi_progress_callback_t)(void);


/**
 * Register an event to be progressed
 */
OMPI_DECLSPEC int ompi_progress_register(ompi_progress_callback_t cb);


/**
 * Unregister previously registered event
 */
OMPI_DECLSPEC int ompi_progress_unregister(ompi_progress_callback_t cb);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

