/*
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_H
#define MCA_COLL_BASE_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_list.h"
#include "mca/coll/coll.h"


/*
 * Global functions for MCA overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_coll_base_open(void);
  int mca_coll_base_select(lam_list_t *selected, 
                           bool *allow_multi_user_threads, 
                           bool *have_hidden_threads);
  const mca_coll_1_0_0_t *
    mca_coll_basic_query(MPI_Comm comm, int *priority);
  int mca_coll_base_close(void);

  int mca_coll_base_init_comm(MPI_Comm comm);
  int mca_coll_base_get_param(MPI_Comm comm, int keyval);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */
extern int mca_coll_base_output;
extern int mca_coll_base_crossover;
extern int mca_coll_base_associative;
extern int mca_coll_base_reduce_crossover;
extern lam_list_t mca_coll_base_modules_available;

#endif /* MCA_BASE_COLL_H */
