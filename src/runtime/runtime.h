/*
 * $HEADER$
 */

#ifndef LAM_RUNTIME_H
#define LAM_RUNTIME_H

#include "lam_config.h"


/*
 * Global variables and symbols for the MPI layer
 */

#define LAM_MPI_INVALID_STATE (!lam_mpi_initialized || lam_mpi_finalized)

extern bool lam_mpi_initialized;
extern bool lam_mpi_finalized;
extern bool lam_mpi_param_check;

extern bool lam_mpi_thread_multiple;
extern int lam_mpi_thread_requested;
extern int lam_mpi_thread_provided;


#ifdef __cplusplus
extern "C" {
#endif

  int lam_abort(int status, char *fmt, ...);
  int lam_init(int argc, char* argv[]);
  int lam_finalize(void);
  int lam_rte_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
  int lam_rte_finalize(void);

  int lam_mpi_init(int argc, char **argv, int requested, int *provided);
  int lam_mpi_finalize(void);

#ifdef __cplusplus
}
#endif

#endif /* LAM_RUNTIME_H */
