/*
 * $HEADER$
 */

#ifndef MCA_MPI_BASE_H
#define MCA_MPI_BASE_H

#include "lam_config.h"

#include "mpi.h"
#include "mca/mca.h"


/*
 * Types for each function
 */

typedef int (*mca_mpi_init_cb_t)(void);
typedef int (*mca_mpi_alloc_mem_fn_t)(MPI_Aint size, MPI_Info info, 
                                      void **base);
typedef int (*mca_mpi_free_mem_fn_t)(void *base);


/*
 * Global functions for MPI MCA modules
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_mpi_alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);
  int mca_mpi_free_mem(void *baseptr);

  int mca_mpi_init_select_modules(int requested, 
                                  bool allow_multi_user_threads,
                                  bool have_hidden_threads, int *provided);

#if 0
  /* JMS Not implemented yet */
  int mca_mpi_init_callback(mca_mpi_init_cb_t func);
  int mca_mpi_init_callbacks_invoke(void);
  int mca_mpi_module_select(int requested);

  int mca_mpi_param_associate(int index, int keyval);
  int mca_mpi_param_lookup_int(int index, MPI_Comm comm);
  char *mca_mpi_param_lookup_string(int index, MPI_Comm comm);
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
