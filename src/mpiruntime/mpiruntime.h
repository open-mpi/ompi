/*
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the MPI portion of the Open MPI Run Time Environment
 */

#ifndef OMPI_MPIRUNTIME_H
#define OMPI_MPIRUNTIME_H

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

#endif /* OMPI_MPIRUNTIME_H */
