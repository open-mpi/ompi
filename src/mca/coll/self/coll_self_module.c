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

#include "ompi_config.h"
#include "coll_self.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/base/mca_base_param.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_self.h"


/*
 * Module
 */
static const mca_coll_base_module_1_0_0_t module = {

  /* Initialization / finalization functions */

  mca_coll_self_module_init,
  mca_coll_self_module_finalize,

  /* Collective function pointers */

  mca_coll_self_allgather_intra,
  mca_coll_self_allgatherv_intra,
  mca_coll_self_allreduce_intra,
  mca_coll_self_alltoall_intra,
  mca_coll_self_alltoallv_intra,
  mca_coll_self_alltoallw_intra,
  mca_coll_self_barrier_intra,
  mca_coll_self_bcast_intra,
  mca_coll_self_exscan_intra,
  mca_coll_self_gather_intra,
  mca_coll_self_gatherv_intra,
  mca_coll_self_reduce_intra,
  mca_coll_self_reduce_scatter_intra,
  mca_coll_self_scan_intra,
  mca_coll_self_scatter_intra,
  mca_coll_self_scatterv_intra
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_self_init_query(bool *allow_multi_user_threads,
                             bool *have_hidden_user_threads)
{
    *allow_multi_user_threads = true;
    *have_hidden_user_threads = false;

    /* All done */
  
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_self_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    /* We only work on intracommunicators of size 1 */

    if (!OMPI_COMM_IS_INTER(comm) && 1 == ompi_comm_size(comm)) {
        if (OMPI_SUCCESS != 
            mca_base_param_lookup_int(mca_coll_self_priority_param,
                                      priority)) {
            return NULL;
        }

        return &module;
    }

    return NULL;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_self_module_init(struct ompi_communicator_t *comm)
{
    /* Don't really need to do anything */

    return &module;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_self_module_finalize(struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}
