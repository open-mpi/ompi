/*
 * $HEADER$
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

  int ompi_abort(int status, char *fmt, ...);
  int ompi_init(int argc, char* argv[]);
  int ompi_finalize(void);
  int ompi_rte_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
  int ompi_rte_finalize(void);

  int ompi_mpi_init(int argc, char **argv, int requested, int *provided);
  int ompi_mpi_finalize(void);

#ifdef __cplusplus
}
#endif

#endif /* OMPI_RUNTIME_H */
