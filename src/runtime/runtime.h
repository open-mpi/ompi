/*
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the Open MPI Run Time Environment
 */

#ifndef OMPI_RUNTIME_H
#define OMPI_RUNTIME_H

#include "ompi_config.h"


/*
 * Global variables and symbols for the MPI layer
 */

extern bool ompi_mpi_initialized;
extern bool ompi_mpi_finalized;
extern bool ompi_mpi_param_check;

extern bool ompi_mpi_thread_multiple;
extern int ompi_mpi_thread_requested;
extern int ompi_mpi_thread_provided;


#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Initialize the Open MPI support code
   *
   * This function initializes the Open MPI support code, including
   * malloc debugging and threads.  It should be called exactly once
   * by every application that utilizes any of the Open MPI support
   * libraries (including MPI applications, mpirun, and mpicc).
   *
   * This function should be called before \code ompi_rte_init, if
   * \code ompi_rte_init is to be called.
   */
  int ompi_init(int argc, char* argv[]);

  /**
   * Finalize the Open MPI support code
   *
   * Finalize the Open MPI support code.  Any function calling \code
   * ompi_init should call \code ompi_finalize.  This function should
   * be called after \code ompi_rte_finalize, if \code
   * ompi_rte_finalize is called.
   */
  int ompi_finalize(void);

  /**
   * Abort the current application with a pretty-print error message
   *
   * Aborts currently running application with \code abort(), pretty
   * printing an error message if possible.  Error message should be
   * specified using the standard \code printf() format.
   */
  int ompi_abort(int status, char *fmt, ...);


  /**
   * Initialize the Open MPI run time environment
   *
   * Initlize the Open MPI run time environment, including process
   * control and out of band messaging.  This function should be
   * called exactly once, after \code ompi_init.  This function should
   * be called by every application using the RTE interface, including
   * MPI applications and mpirun.
   */
  int ompi_rte_init(bool *allow_multi_user_threads, bool *have_hidden_threads);

  /**
   * Finalize the Open MPI run time environment
   *
   */
  int ompi_rte_finalize(void);


  /**
   * Initialize the Open MPI MPI environment
   *
   * Intialize all support code needed for MPI applications.  This
   * function should only be called by MPI applications (including
   * singletons).  It should only be called after both \code
   * ompi_init() and \code ompi_rte_init() have been called.
   */
  int ompi_mpi_init(int argc, char **argv, int requested, int *provided);

  /**
   * Finalize the Open MPI MPI environment
   *
   * Should be called after all MPI functionality is complete (usually
   * during MPI_FINALIZE) and before \c ompi_rte_finalize().
   */
  int ompi_mpi_finalize(void);

#ifdef __cplusplus
}
#endif

#endif /* OMPI_RUNTIME_H */
