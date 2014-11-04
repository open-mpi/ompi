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
mca_coll_basic_neighbor_alltoallw_cart(const void *sbuf, const int scounts[], const MPI_Aint sdisps[],
                                       struct ompi_datatype_t * const sdtypes[], void *rbuf, const int rcounts[],
                                       const MPI_Aint rdisps[], struct ompi_datatype_t * const rdtypes[],
                                       struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_cart_2_1_0_t *cart = comm->c_topo->mtc.cart;
    const int rank = ompi_comm_rank (comm);
    int rc = MPI_SUCCESS, dim, i, nreqs;
    ompi_request_t **reqs;

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, cart->ndims * 4);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* post receives first */
    for (dim = 0, i = 0, nreqs = 0, reqs = basic_module->mccb_reqs ; dim < cart->ndims ; ++dim, i += 2) {
        int srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            srank = drank = rank;
        }

        if (MPI_PROC_NULL != srank) {
            rc = MCA_PML_CALL(irecv((char *) rbuf + rdisps[i], rcounts[i], rdtypes[i], srank,
                                    MCA_COLL_BASE_TAG_ALLTOALL, comm, reqs++));
            if (OMPI_SUCCESS != rc) break;
            nreqs++;
        }

        if (MPI_PROC_NULL != drank) {
            rc = MCA_PML_CALL(irecv((char *) rbuf + rdisps[i+1], rcounts[i+1], rdtypes[i+1], drank,
                                    MCA_COLL_BASE_TAG_ALLTOALL, comm, reqs++));
            if (OMPI_SUCCESS != rc) break;
            nreqs++;
        }
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    for (dim = 0, i = 0 ; dim < cart->ndims ; ++dim, i += 2) {
        int srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            srank = drank = rank;
        }

        if (MPI_PROC_NULL != srank) {
            /* remove cast from const when the pml layer is updated to take a const for the send buffer */
            rc = MCA_PML_CALL(isend((char *) sbuf + sdisps[i], scounts[i], sdtypes[i], srank,
                                    MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD, comm, reqs++));
            if (OMPI_SUCCESS != rc) break;
            nreqs++;
        }

        if (MPI_PROC_NULL != drank) {
            rc = MCA_PML_CALL(isend((char *) sbuf + sdisps[i+1], scounts[i+1], sdtypes[i+1], drank,
                                    MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD, comm, reqs++));
            if (OMPI_SUCCESS != rc) break;
            nreqs++;
        }
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    return ompi_request_wait_all (nreqs, basic_module->mccb_reqs, MPI_STATUSES_IGNORE);
}

static int
mca_coll_basic_neighbor_alltoallw_graph(const void *sbuf, const int scounts[], const MPI_Aint sdisps[],
                                        struct ompi_datatype_t * const sdtypes[], void *rbuf, const int rcounts[],
                                        const MPI_Aint rdisps[], struct ompi_datatype_t * const rdtypes[],
                                        struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_graph_2_1_0_t *graph = comm->c_topo->mtc.graph;
    int rc = MPI_SUCCESS, neighbor, degree;
    const int rank = ompi_comm_rank (comm);
    ompi_request_t **reqs;
    const int *edges;

    mca_topo_base_graph_neighbors_count (comm, rank, &degree);

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, 2 * degree);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    edges = graph->edges;
    if (rank > 0) {
        edges += graph->index[rank - 1];
    }

    /* post all receives first */
    for (neighbor = 0, reqs = basic_module->mccb_reqs ; neighbor < degree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv((char *) rbuf + rdisps[neighbor], rcounts[neighbor], rdtypes[neighbor],
                                edges[neighbor], MCA_COLL_BASE_TAG_ALLTOALL, comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    for (neighbor = 0 ; neighbor < degree ; ++neighbor) {
        /* remove cast from const when the pml layer is updated to take a const for the send buffer */
        rc = MCA_PML_CALL(isend((char *) sbuf + sdisps[neighbor], scounts[neighbor], sdtypes[neighbor],
                                edges[neighbor], MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
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
mca_coll_basic_neighbor_alltoallw_dist_graph(const void *sbuf, const int scounts[], const MPI_Aint sdisps[],
                                             struct ompi_datatype_t * const sdtypes[], void *rbuf, const int rcounts[],
                                             const MPI_Aint rdisps[], struct ompi_datatype_t * const rdtypes[],
                                             struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t *) module;
    const mca_topo_base_comm_dist_graph_2_1_0_t *dist_graph = comm->c_topo->mtc.dist_graph;
    int rc = MPI_SUCCESS, neighbor;
    const int *inedges, *outedges;
    int indegree, outdegree;
    ompi_request_t **reqs;

    indegree = dist_graph->indegree;
    outdegree = dist_graph->outdegree;

    /* ensure we have enough storage for requests */
    rc = mca_coll_basic_check_for_requests (basic_module, indegree + outdegree);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    inedges = dist_graph->in;
    outedges = dist_graph->out;

    /* post all receives first */
    for (neighbor = 0, reqs = basic_module->mccb_reqs ; neighbor < indegree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv((char *) rbuf + rdisps[neighbor], rcounts[neighbor], rdtypes[neighbor],
                                inedges[neighbor], MCA_COLL_BASE_TAG_ALLTOALL, comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    for (neighbor = 0 ; neighbor < outdegree ; ++neighbor) {
        /* remove cast from const when the pml layer is updated to take a const for the send buffer */
        rc = MCA_PML_CALL(isend((char *) sbuf + sdisps[neighbor], scounts[neighbor], sdtypes[neighbor],
                                outedges[neighbor], MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                                comm, reqs++));
        if (OMPI_SUCCESS != rc) break;
    }

    if (OMPI_SUCCESS != rc) {
        /* should probably try to clean up here */
        return rc;
    }

    return ompi_request_wait_all (indegree + outdegree, basic_module->mccb_reqs, MPI_STATUSES_IGNORE);
}

int mca_coll_basic_neighbor_alltoallw(void *sbuf, int scounts[], MPI_Aint sdisps[],
                                      struct ompi_datatype_t *sdtypes[], void *rbuf, int rcounts[],
                                      MPI_Aint rdisps[], struct ompi_datatype_t *rdtypes[],
                                      struct ompi_communicator_t *comm, mca_coll_base_module_t *module)
{
    if (OMPI_COMM_IS_INTER(comm)) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (OMPI_COMM_IS_CART(comm)) {
        return mca_coll_basic_neighbor_alltoallw_cart (sbuf, scounts, sdisps, sdtypes, rbuf,
                                                       rcounts, rdisps, rdtypes, comm, module);
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        return mca_coll_basic_neighbor_alltoallw_graph (sbuf, scounts, sdisps, sdtypes, rbuf,
                                                        rcounts, rdisps, rdtypes, comm, module);
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        return mca_coll_basic_neighbor_alltoallw_dist_graph (sbuf, scounts, sdisps, sdtypes, rbuf,
                                                             rcounts, rdisps, rdtypes, comm, module);
    }

    return OMPI_ERR_NOT_SUPPORTED;
}
