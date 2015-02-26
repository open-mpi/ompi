/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "coll_basic.h"


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
mca_coll_base_module_t *
mca_coll_basic_comm_query(struct ompi_communicator_t *comm, 
                          int *priority)
{
    int size;
    mca_coll_basic_module_t *basic_module;

    basic_module = OBJ_NEW(mca_coll_basic_module_t);
    if (NULL == basic_module) return NULL;

    *priority = mca_coll_basic_priority;

    /* Allocate the data that hangs off the communicator */

    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }
    size *= 2;
    if (OMPI_COMM_IS_CART(comm)) {
        int cart_size, ndims;
        assert (NULL != comm->c_topo);
        comm->c_topo->topo.cart.cartdim_get(comm, &ndims);
        cart_size = ndims * 4;
        if (cart_size > size) {
            size = cart_size;
        }
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        int rank, degree;
        assert (NULL != comm->c_topo);
        rank = ompi_comm_rank (comm);
        comm->c_topo->topo.graph.graph_neighbors_count (comm, rank, &degree);
        degree *= 2;
        if (degree > size) {
            size = degree;
        }
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        int dist_graph_size, inneighbors, outneighbors, weighted;
        assert (NULL != comm->c_topo);
        comm->c_topo->topo.dist_graph.dist_graph_neighbors_count(comm, &inneighbors, &outneighbors, &weighted);
        dist_graph_size = inneighbors + outneighbors;
        if (dist_graph_size > size) {
            size = dist_graph_size;
        }
    }
    basic_module->mccb_num_reqs = size;
    basic_module->mccb_reqs = (ompi_request_t**) 
        malloc(sizeof(ompi_request_t *) * basic_module->mccb_num_reqs);

    /* Choose whether to use [intra|inter], and [linear|log]-based
     * algorithms. */
    basic_module->super.coll_module_enable = mca_coll_basic_module_enable;
    basic_module->super.ft_event = mca_coll_basic_ft_event;

    if (OMPI_COMM_IS_INTER(comm)) {
        basic_module->super.coll_allgather  = mca_coll_basic_allgather_inter;
        basic_module->super.coll_allgatherv = mca_coll_basic_allgatherv_inter;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_inter;
        basic_module->super.coll_alltoall   = mca_coll_basic_alltoall_inter;
        basic_module->super.coll_alltoallv  = mca_coll_basic_alltoallv_inter;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_inter;
        basic_module->super.coll_barrier    = mca_coll_basic_barrier_inter_lin;
        basic_module->super.coll_bcast      = mca_coll_basic_bcast_lin_inter;
        basic_module->super.coll_exscan     = NULL;
        basic_module->super.coll_gather     = mca_coll_basic_gather_inter;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_inter;
        basic_module->super.coll_reduce     = mca_coll_basic_reduce_lin_inter;
        basic_module->super.coll_reduce_scatter_block = mca_coll_basic_reduce_scatter_block_inter;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_inter;
        basic_module->super.coll_scan       = NULL;
        basic_module->super.coll_scatter    = mca_coll_basic_scatter_inter;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_inter;
    } else if (ompi_comm_size(comm) <= mca_coll_basic_crossover) {
        basic_module->super.coll_allgather  = ompi_coll_base_allgather_intra_basic_linear;
        basic_module->super.coll_allgatherv = ompi_coll_base_allgatherv_intra_basic_default;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_intra;
        basic_module->super.coll_alltoall   = ompi_coll_base_alltoall_intra_basic_linear;
        basic_module->super.coll_alltoallv  = ompi_coll_base_alltoallv_intra_basic_linear;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_intra;
        basic_module->super.coll_barrier    = ompi_coll_base_barrier_intra_basic_linear;
        basic_module->super.coll_bcast      = ompi_coll_base_bcast_intra_basic_linear;
        basic_module->super.coll_exscan     = mca_coll_basic_exscan_intra;
        basic_module->super.coll_gather     = ompi_coll_base_gather_intra_basic_linear;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_intra;
        basic_module->super.coll_reduce     = ompi_coll_base_reduce_intra_basic_linear;
        basic_module->super.coll_reduce_scatter_block = mca_coll_basic_reduce_scatter_block_intra;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_intra;
        basic_module->super.coll_scan       = mca_coll_basic_scan_intra;
        basic_module->super.coll_scatter    = ompi_coll_base_scatter_intra_basic_linear;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_intra;
    } else {
        basic_module->super.coll_allgather  = ompi_coll_base_allgather_intra_basic_linear;
        basic_module->super.coll_allgatherv = ompi_coll_base_allgatherv_intra_basic_default;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_intra;
        basic_module->super.coll_alltoall   = ompi_coll_base_alltoall_intra_basic_linear;
        basic_module->super.coll_alltoallv  = ompi_coll_base_alltoallv_intra_basic_linear;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_intra;
        basic_module->super.coll_barrier    = mca_coll_basic_barrier_intra_log;
        basic_module->super.coll_bcast      = mca_coll_basic_bcast_log_intra;
        basic_module->super.coll_exscan     = mca_coll_basic_exscan_intra;
        basic_module->super.coll_gather     = ompi_coll_base_gather_intra_basic_linear;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_intra;
        basic_module->super.coll_reduce     = mca_coll_basic_reduce_log_intra;
        basic_module->super.coll_reduce_scatter_block = mca_coll_basic_reduce_scatter_block_intra;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_intra;
        basic_module->super.coll_scan       = mca_coll_basic_scan_intra;
        basic_module->super.coll_scatter    = ompi_coll_base_scatter_intra_basic_linear;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_intra;
    }

    /* These functions will return an error code if comm does not have a virtual topology */
    basic_module->super.coll_neighbor_allgather = mca_coll_basic_neighbor_allgather;
    basic_module->super.coll_neighbor_allgatherv = mca_coll_basic_neighbor_allgatherv;
    basic_module->super.coll_neighbor_alltoall = mca_coll_basic_neighbor_alltoall;
    basic_module->super.coll_neighbor_alltoallv = mca_coll_basic_neighbor_alltoallv;
    basic_module->super.coll_neighbor_alltoallw = mca_coll_basic_neighbor_alltoallw;

    return &(basic_module->super);
}


/*
 * Init module on the communicator
 */
int
mca_coll_basic_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


int
mca_coll_basic_ft_event(int state) {
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
