/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_demo.h"

#include <stdio.h>

#include "mpi.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "coll_demo.h"


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t intra = {

    /* Initialization / finalization functions */

    mca_coll_demo_module_init,
    mca_coll_demo_module_finalize,

    /* Collective function pointers */

    mca_coll_demo_allgather_intra,
    mca_coll_demo_allgatherv_intra,
    mca_coll_demo_allreduce_intra,
    mca_coll_demo_alltoall_intra,
    mca_coll_demo_alltoallv_intra,
    mca_coll_demo_alltoallw_intra,
    mca_coll_demo_barrier_intra,
    mca_coll_demo_bcast_intra,
    NULL, /* Leave exscan blank just to force basic to be used */
    mca_coll_demo_gather_intra,
    mca_coll_demo_gatherv_intra,
    mca_coll_demo_reduce_intra,
    mca_coll_demo_reduce_scatter_intra,
    mca_coll_demo_scan_intra,
    mca_coll_demo_scatter_intra,
    mca_coll_demo_scatterv_intra
};


/*
 * Linear set of collective algorithms for intercommunicators
 */
static const mca_coll_base_module_1_0_0_t inter = {

    /* Initialization / finalization functions */

    mca_coll_demo_module_init,
    mca_coll_demo_module_finalize,

    /* Collective function pointers */

    mca_coll_demo_allgather_inter,
    mca_coll_demo_allgatherv_inter,
    mca_coll_demo_allreduce_inter,
    mca_coll_demo_alltoall_inter,
    mca_coll_demo_alltoallv_inter,
    mca_coll_demo_alltoallw_inter,
    mca_coll_demo_barrier_inter,
    mca_coll_demo_bcast_inter,
    mca_coll_demo_exscan_inter,
    mca_coll_demo_gather_inter,
    mca_coll_demo_gatherv_inter,
    mca_coll_demo_reduce_inter,
    mca_coll_demo_reduce_scatter_inter,
    NULL,
    mca_coll_demo_scatter_inter,
    mca_coll_demo_scatterv_inter
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_demo_init_query(bool *allow_demo_user_threads,
                             bool *have_hidden_user_threads)
{
    *allow_demo_user_threads = true;
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
mca_coll_demo_comm_query(struct ompi_communicator_t *comm, int *priority,
                         struct mca_coll_base_comm_t **data)
{
    if (OMPI_SUCCESS != 
        mca_base_param_lookup_int(mca_coll_demo_priority_param, priority)) {
        return NULL;
    }

    return OMPI_COMM_IS_INTER(comm) ? &inter : &intra;
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_demo_module_init(struct ompi_communicator_t *comm)
{
    mca_base_param_lookup_int(mca_coll_demo_verbose_param,
                              &mca_coll_demo_verbose);
    if (mca_coll_demo_verbose > 0) {
        printf("Hello!  This is the \"demo\" coll component.  I'll be your coll component\ntoday.  Please tip your waitresses well.\n");
    }

    return OMPI_COMM_IS_INTER(comm) ? &inter : &intra;
}


/*
 * Finalize module on the communicator
 */
int mca_coll_demo_module_finalize(struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}
