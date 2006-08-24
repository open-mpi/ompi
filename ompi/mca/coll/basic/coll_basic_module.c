/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_basic.h"


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t intra_linear = {

    /* Initialization / finalization functions */

    mca_coll_basic_module_init,
    mca_coll_basic_module_finalize,

    /* Collective function pointers */

    mca_coll_basic_allgather_intra,
    mca_coll_basic_allgatherv_intra,
    mca_coll_basic_allreduce_intra,
    mca_coll_basic_alltoall_intra,
    mca_coll_basic_alltoallv_intra,
    mca_coll_basic_alltoallw_intra,
    mca_coll_basic_barrier_intra_lin,
    mca_coll_basic_bcast_lin_intra,
    mca_coll_basic_exscan_intra,
    mca_coll_basic_gather_intra,
    mca_coll_basic_gatherv_intra,
    mca_coll_basic_reduce_lin_intra,
    mca_coll_basic_reduce_scatter_intra,
    mca_coll_basic_scan_intra,
    mca_coll_basic_scatter_intra,
    mca_coll_basic_scatterv_intra
};


/*
 * Lograthmic set of collective algorithms.  Note that not all
 * collectives have lograthmic algorithms.  For example, scan will use
 * the same algorithm as in the linear set.
 */
static const mca_coll_base_module_1_0_0_t intra_log = {

    /* Initialization / finalization functions */

    mca_coll_basic_module_init,
    mca_coll_basic_module_finalize,

    /* Collective function pointers */

    mca_coll_basic_allgather_intra,
    mca_coll_basic_allgatherv_intra,
    mca_coll_basic_allreduce_intra,
    mca_coll_basic_alltoall_intra,
    mca_coll_basic_alltoallv_intra,
    mca_coll_basic_alltoallw_intra,
    mca_coll_basic_barrier_intra_log,
    mca_coll_basic_bcast_log_intra,
    mca_coll_basic_exscan_intra,
    mca_coll_basic_gather_intra,
    mca_coll_basic_gatherv_intra,
    mca_coll_basic_reduce_log_intra,
    mca_coll_basic_reduce_scatter_intra,
    mca_coll_basic_scan_intra,
    mca_coll_basic_scatter_intra,
    mca_coll_basic_scatterv_intra
};


/*
 * Linear set of collective algorithms for intercommunicators
 */
static const mca_coll_base_module_1_0_0_t inter_linear = {

    /* Initialization / finalization functions */

    mca_coll_basic_module_init,
    mca_coll_basic_module_finalize,

    /* Collective function pointers */

    mca_coll_basic_allgather_inter,
    mca_coll_basic_allgatherv_inter,
    mca_coll_basic_allreduce_inter,
    mca_coll_basic_alltoall_inter,
    mca_coll_basic_alltoallv_inter,
    mca_coll_basic_alltoallw_inter,
    mca_coll_basic_barrier_inter_lin,
    mca_coll_basic_bcast_lin_inter,
    NULL,
    mca_coll_basic_gather_inter,
    mca_coll_basic_gatherv_inter,
    mca_coll_basic_reduce_lin_inter,
    mca_coll_basic_reduce_scatter_inter,
    NULL,
    mca_coll_basic_scatter_inter,
    mca_coll_basic_scatterv_inter
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int
mca_coll_basic_init_query(bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    /* Nothing to do */

    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_basic_comm_query(struct ompi_communicator_t *comm, int *priority,
                          struct mca_coll_base_comm_t **data)
{
    *priority = mca_coll_basic_priority;

    /* Choose whether to use [intra|inter], and [linear|log]-based
     * algorithms. */

    if (OMPI_COMM_IS_INTER(comm)) {
        return &inter_linear;
    } else {
        if (ompi_comm_size(comm) <= mca_coll_basic_crossover) {
            return &intra_linear;
        } else {
            return &intra_log;
        }
    }

    /* Never reach here */
}


/*
 * Init module on the communicator
 */
const struct mca_coll_base_module_1_0_0_t *
mca_coll_basic_module_init(struct ompi_communicator_t *comm)
{
    int size;
    struct mca_coll_base_comm_t *data;

    /* Allocate the data that hangs off the communicator */

    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }
    data = (struct mca_coll_base_comm_t*)malloc(sizeof(struct mca_coll_base_comm_t) +
                                                (sizeof(ompi_request_t *) * size * 2));

    if (NULL == data) {
        return NULL;
    }
    data->mccb_reqs = (ompi_request_t **) (data + 1);
    data->mccb_num_reqs = size * 2;

    /* All done */

    comm->c_coll_basic_data = data;
    return comm->c_coll_basic_module;
}


/*
 * Finalize module on the communicator
 */
int
mca_coll_basic_module_finalize(struct ompi_communicator_t *comm)
{
    if (NULL == comm->c_coll_basic_module) {
        return OMPI_SUCCESS;
    }

#if OMPI_ENABLE_DEBUG
    /* Reset the reqs to NULL/0 -- they'll be freed as part of freeing
     * the generel c_coll_basic_data */

    comm->c_coll_basic_data->mccb_reqs = NULL;
    comm->c_coll_basic_data->mccb_num_reqs = 0;
#endif

    /* All done */

    free(comm->c_coll_basic_data);
    comm->c_coll_basic_data = NULL;
    return OMPI_SUCCESS;
}
