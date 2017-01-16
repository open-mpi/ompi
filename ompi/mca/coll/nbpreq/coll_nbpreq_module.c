/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_nbpreq.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int
mca_coll_nbpreq_init_query(bool enable_progress_threads,
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
mca_coll_base_module_t *
mca_coll_nbpreq_comm_query(struct ompi_communicator_t *comm,
                          int *priority)
{
    mca_coll_nbpreq_module_t *nbpreq_module;
    mca_coll_base_module_t *base_module;

    nbpreq_module = OBJ_NEW(mca_coll_nbpreq_module_t);
    if (NULL == nbpreq_module) {
        return NULL;
    }
    base_module = &nbpreq_module->super;

    *priority = mca_coll_nbpreq_component.priority;

    base_module->coll_module_enable             = mca_coll_nbpreq_module_enable;
    base_module->coll_allgather_init            = mca_coll_nbpreq_allgather_init;
    base_module->coll_allgatherv_init           = mca_coll_nbpreq_allgatherv_init;
    base_module->coll_allreduce_init            = mca_coll_nbpreq_allreduce_init;
    base_module->coll_alltoall_init             = mca_coll_nbpreq_alltoall_init;
    base_module->coll_alltoallv_init            = mca_coll_nbpreq_alltoallv_init;
    base_module->coll_alltoallw_init            = mca_coll_nbpreq_alltoallw_init;
    base_module->coll_barrier_init              = mca_coll_nbpreq_barrier_init;
    base_module->coll_bcast_init                = mca_coll_nbpreq_bcast_init;
    base_module->coll_exscan_init               = mca_coll_nbpreq_exscan_init;
    base_module->coll_gather_init               = mca_coll_nbpreq_gather_init;
    base_module->coll_gatherv_init              = mca_coll_nbpreq_gatherv_init;
    base_module->coll_reduce_init               = mca_coll_nbpreq_reduce_init;
    base_module->coll_reduce_scatter_init       = mca_coll_nbpreq_reduce_scatter_init;
    base_module->coll_reduce_scatter_block_init = mca_coll_nbpreq_reduce_scatter_block_init;
    base_module->coll_scan_init                 = mca_coll_nbpreq_scan_init;
    base_module->coll_scatter_init              = mca_coll_nbpreq_scatter_init;
    base_module->coll_scatterv_init             = mca_coll_nbpreq_scatterv_init;
    base_module->coll_neighbor_allgather_init   = mca_coll_nbpreq_neighbor_allgather_init;
    base_module->coll_neighbor_allgatherv_init  = mca_coll_nbpreq_neighbor_allgatherv_init;
    base_module->coll_neighbor_alltoall_init    = mca_coll_nbpreq_neighbor_alltoall_init;
    base_module->coll_neighbor_alltoallv_init   = mca_coll_nbpreq_neighbor_alltoallv_init;
    base_module->coll_neighbor_alltoallw_init   = mca_coll_nbpreq_neighbor_alltoallw_init;
    base_module->ft_event                       = mca_coll_nbpreq_ft_event;

    return base_module;
}

/*
 * Init module on the communicator
 */
int
mca_coll_nbpreq_module_enable(mca_coll_base_module_t *module,
                              struct ompi_communicator_t *comm)
{
    /* prepare the placeholder for the array of request* */
    module->base_data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == module->base_data) {
        return OMPI_ERROR;
    }

    /* All done */
    return OMPI_SUCCESS;
}

int
mca_coll_nbpreq_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
