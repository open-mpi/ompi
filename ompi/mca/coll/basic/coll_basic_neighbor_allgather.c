/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdlib.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"
#include "ompi/mca/topo/base/base.h"

static int
mca_coll_basic_neighbor_allgather_cart(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype, void *rbuf,
                                       int rcount, struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_cart_2_1_0_t *cart = comm->c_topo->mtc.cart;
    const int rank = ompi_comm_rank (comm);
    ompi_request_t **reqs;
    ptrdiff_t lb, extent;
    int rc = MPI_SUCCESS, dim, nreqs;

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, cart->ndims * 4);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    ompi_datatype_get_extent(rdtype, &lb, &extent);

    /* The ordering is defined as -1 then +1 in each dimension in
     * order of dimension. */
    for (dim = 0, reqs = basic_module->mccb_reqs, nreqs = 0 ; dim < cart->ndims ; ++dim) {
        int srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            srank = drank = rank;
        }

        if (MPI_PROC_NULL != srank) {
            rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, srank,
                                    MCA_COLL_BASE_TAG_ALLGATHER,
                                    comm, reqs++));
            if (OMPI_SUCCESS != rc) break;

            /* remove cast from const when the pml layer is updated to take
             * a const for the send buffer. */
            rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, srank,
                                    MCA_COLL_BASE_TAG_ALLGATHER,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    comm, reqs++));
            if (OMPI_SUCCESS != rc) break;

            nreqs += 2;
        }

        rbuf = (char *) rbuf + extent * rcount;

        if (MPI_PROC_NULL != drank) {
            rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, drank,
                                    MCA_COLL_BASE_TAG_ALLGATHER,
                                    comm, reqs++));
            if (OMPI_SUCCESS != rc) break;


            rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, drank,
                                    MCA_COLL_BASE_TAG_ALLGATHER,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    comm, reqs++));
            if (OMPI_SUCCESS != rc) break;

            nreqs += 2;
        }

        rbuf = (char *) rbuf + extent * rcount;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    return ompi_request_wait_all (nreqs, basic_module->mccb_reqs, MPI_STATUSES_IGNORE);
}

static int
mca_coll_basic_neighbor_allgather_graph(const void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype, void *rbuf,
                                        int rcount, struct ompi_datatype_t *rdtype,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_graph_2_1_0_t *graph = comm->c_topo->mtc.graph;
    const int rank = ompi_comm_rank (comm);
    const int *edges;
    int degree;
    ompi_request_t **reqs;
    ptrdiff_t lb, extent;
    int rc = MPI_SUCCESS, neighbor;

    mca_topo_base_graph_neighbors_count (comm, rank, &degree);

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, degree * 2);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    edges = graph->edges;
    if (rank > 0) {
        edges += graph->index[rank - 1];
    }

    ompi_datatype_get_extent(rdtype, &lb, &extent);

    for (neighbor = 0, reqs = basic_module->mccb_reqs ; neighbor < degree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, edges[neighbor], MCA_COLL_BASE_TAG_ALLGATHER,
                                comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
        rbuf = (char *) rbuf + extent * rcount;

        /* remove cast from const when the pml layer is updated to take
         * a const for the send buffer. */
        rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, edges[neighbor],
                                MCA_COLL_BASE_TAG_ALLGATHER, MCA_PML_BASE_SEND_STANDARD,
                                comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    return ompi_request_wait_all (degree * 2, basic_module->mccb_reqs, MPI_STATUSES_IGNORE);
}

static int
mca_coll_basic_neighbor_allgather_dist_graph(const void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype, void *rbuf,
                                             int rcount, struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_dist_graph_2_1_0_t *dist_graph = comm->c_topo->mtc.dist_graph;
    const int *inedges, *outedges;
    int indegree, outdegree;
    ompi_request_t **reqs;
    ptrdiff_t lb, extent;
    int rc = MPI_SUCCESS, neighbor;

    indegree = dist_graph->indegree;
    outdegree = dist_graph->outdegree;

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, indegree + outdegree);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    inedges = dist_graph->in;
    outedges = dist_graph->out;

    ompi_datatype_get_extent(rdtype, &lb, &extent);

    for (neighbor = 0, reqs = basic_module->mccb_reqs ; neighbor < indegree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, inedges[neighbor],
                                MCA_COLL_BASE_TAG_ALLGATHER,
                                comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
        rbuf = (char *) rbuf + extent * rcount;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    for (neighbor = 0 ; neighbor < outdegree ; ++neighbor) {
        /* remove cast from const when the pml layer is updated to take
         * a const for the send buffer. */
        rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, outedges[neighbor],
                                MCA_COLL_BASE_TAG_ALLGATHER,
                                MCA_PML_BASE_SEND_STANDARD,
                                comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    return ompi_request_wait_all (indegree + outdegree, basic_module->mccb_reqs, MPI_STATUSES_IGNORE);
}

int mca_coll_basic_neighbor_allgather(void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype, void *rbuf,
                                      int rcount, struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    if (OMPI_COMM_IS_INTER(comm)) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (OMPI_COMM_IS_CART(comm)) {
        return mca_coll_basic_neighbor_allgather_cart (sbuf, scount, sdtype, rbuf,
                                                       rcount, rdtype, comm, module);
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        return mca_coll_basic_neighbor_allgather_graph (sbuf, scount, sdtype, rbuf,
                                                        rcount, rdtype, comm, module);
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        return mca_coll_basic_neighbor_allgather_dist_graph (sbuf, scount, sdtype, rbuf,
                                                             rcount, rdtype, comm, module);
    }

    return OMPI_ERR_NOT_SUPPORTED;
}
