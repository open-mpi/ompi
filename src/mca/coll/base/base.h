/*
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_H
#define MCA_COLL_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "class/ompi_list.h"
#include "mca/coll/coll.h"


/*
 * Global functions for MCA overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_coll_base_open(void);
    int mca_coll_base_select(ompi_list_t *selected, 
			     bool *allow_multi_user_threads, 
			     bool *have_hidden_threads);
    int mca_coll_base_query(void);
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
extern ompi_list_t mca_coll_base_modules_opened;
extern int mca_coll_base_bcast_collmaxlin;
extern int mca_coll_base_bcast_collmaxdim;

#endif /* MCA_BASE_COLL_H */
