/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the MPI portion of the Open MPI Run Time Environment
 */

#ifndef OMPI_MPI_MPIRUNTIME_H
#define OMPI_MPI_MPIRUNTIME_H

#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /** forward type declaration */
    struct ompi_communicator_t;
    /** forward type declaration */
    struct ompi_thread_t;

    /* Global variables and symbols for the MPI layer */

    /** Is mpi initialized? */
    OMPI_DECLSPEC extern bool ompi_mpi_initialized;
    /** Has mpi been finalized? */
    OMPI_DECLSPEC extern bool ompi_mpi_finalized;

    /** Do we have multiple threads? */
    OMPI_DECLSPEC extern bool ompi_mpi_thread_multiple;
    /** Thread level requested to \c MPI_Init_thread() */
    OMPI_DECLSPEC extern int ompi_mpi_thread_requested;
    /** Thread level provided by Open MPI */
    OMPI_DECLSPEC extern int ompi_mpi_thread_provided;
    /** Identifier of the main thread */
    OMPI_DECLSPEC extern struct ompi_thread_t *ompi_mpi_main_thread;


  /**
   * Initialize the Open MPI MPI environment
   *
   * @param argc argc, typically from main() (IN)
   * @param argv argv, typically from main() (IN)
   * @param requested Thread support that is requested (IN)
   * @param provided Thread support that is provided (OUT)
   *
   * @returns MPI_SUCCESS if successful
   * @returns Error code if unsuccessful
   *
   * Intialize all support code needed for MPI applications.  This
   * function should only be called by MPI applications (including
   * singletons).  If this function is called, ompi_init() and
   * ompi_rte_init() should *not* be called.
   *
   * It is permissable to pass in (0, NULL) for (argc, argv).
   */
  int ompi_mpi_init(int argc, char **argv, int requested, int *provided);

  /**
   * Finalize the Open MPI MPI environment
   *
   * @returns MPI_SUCCESS if successful
   * @returns Error code if unsuccessful
   *
   * Should be called after all MPI functionality is complete (usually
   * during MPI_FINALIZE).
   */
  int ompi_mpi_finalize(void);

    /**
     * Abort the processes of comm
     */
    int ompi_mpi_abort(struct ompi_communicator_t* comm,
                       int errcode, bool kill_remote_of_intercomm);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MPI_MPIRUNTIME_H */
