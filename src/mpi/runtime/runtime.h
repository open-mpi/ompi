/*
 * $HEADER$
 */

#ifndef LAM_MPI_INIT_H
#define LAM_MPI_INIT_H

/*
 * Run-time functions for the MPI layer
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int lam_mpi_init(int argc, char **argv, int requested, int *provided);
  int lam_mpi_finalize(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Global variables and symbols for the MPI layer
 */

extern bool lam_mpi_initialized;
extern bool lam_mpi_finalized;

extern bool lam_mpi_thread_multiple;
extern int lam_mpi_thread_requested;
extern int lam_mpi_thread_provided;

#endif /* LAM_MPI_INIT_H */
