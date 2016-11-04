/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
mca_coll_basic_neighbor_alltoall_cart(const void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf,
                                      int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    const mca_topo_base_comm_cart_2_2_0_t *cart = comm->c_topo->mtc.cart;
    const int rank = ompi_comm_rank (comm);
    ompi_request_t **reqs, **preqs;
    ptrdiff_t lb, rdextent, sdextent;
    int rc = MPI_SUCCESS, dim, nreqs;

    ompi_datatype_get_extent(rdtype, &lb, &rdextent);
    ompi_datatype_get_extent(sdtype, &lb, &sdextent);
    reqs = preqs = coll_base_comm_get_reqs( module->base_data, 4 * cart->ndims);
    if( NULL == reqs ) { return OMPI_ERR_OUT_OF_RESOURCE; }

    /* post receives first */
    for (dim = 0, nreqs = 0; dim < cart->ndims ; ++dim) {
        int srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            srank = drank = rank;
        }

        if (MPI_PROC_NULL != srank) {
            nreqs++;
            rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, srank,
                                    MCA_COLL_BASE_TAG_ALLTOALL,
                                    comm, preqs++));
            if (OMPI_SUCCESS != rc) break;
        }

        rbuf = (char *) rbuf + rdextent * rcount;

        if (MPI_PROC_NULL != drank) {
            nreqs++;
            rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, drank,
                                    MCA_COLL_BASE_TAG_ALLTOALL,
                                    comm, preqs++));
            if (OMPI_SUCCESS != rc) break;
        }

        rbuf = (char *) rbuf + rdextent * rcount;
    }

    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs( reqs, nreqs);
        return rc;
    }

    for (dim = 0 ; dim < cart->ndims ; ++dim) {
        int srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            srank = drank = rank;
        }

        if (MPI_PROC_NULL != srank) {
            /* remove cast from const when the pml layer is updated to take
             * a const for the send buffer. */
            nreqs++;
            rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, srank,
                                    MCA_COLL_BASE_TAG_ALLTOALL,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    comm, preqs++));
            if (OMPI_SUCCESS != rc) break;
        }

        sbuf = (const char *) sbuf + sdextent * scount;

        if (MPI_PROC_NULL != drank) {
            nreqs++;
            rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, drank,
                                    MCA_COLL_BASE_TAG_ALLTOALL,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    comm, preqs++));
            if (OMPI_SUCCESS != rc) break;
        }

        sbuf = (const char *) sbuf + sdextent * scount;
    }

    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs( reqs, nreqs);
        return rc;
    }

    rc = ompi_request_wait_all (nreqs, reqs, MPI_STATUSES_IGNORE);
    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs( reqs, nreqs);
    }
    return rc;
}

static int
mca_coll_basic_neighbor_alltoall_graph(const void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf,
                                       int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    const mca_topo_base_comm_graph_2_2_0_t *graph = comm->c_topo->mtc.graph;
    const int rank = ompi_comm_rank (comm);
    int rc = MPI_SUCCESS, neighbor, degree;
    ptrdiff_t lb, rdextent, sdextent;
    ompi_request_t **reqs, **preqs;
    const int *edges;

    mca_topo_base_graph_neighbors_count (comm, rank, &degree);

    edges = graph->edges;
    if (rank > 0) {
        edges += graph->index[rank - 1];
    }

    ompi_datatype_get_extent(rdtype, &lb, &rdextent);
    ompi_datatype_get_extent(sdtype, &lb, &sdextent);
    reqs = preqs = coll_base_comm_get_reqs( module->base_data, 2 * degree);
    if( NULL == reqs ) { return OMPI_ERR_OUT_OF_RESOURCE; }

    /* post receives first */
    for (neighbor = 0; neighbor < degree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, edges[neighbor], MCA_COLL_BASE_TAG_ALLTOALL,
                                comm, preqs++));
        if (OMPI_SUCCESS != rc) break;
        rbuf = (char *) rbuf + rdextent * rcount;
    }
    if( MPI_SUCCESS != rc ) {
        ompi_coll_base_free_reqs( reqs, neighbor + 1 );
        return rc;
    }

    for (neighbor = 0 ; neighbor < degree ; ++neighbor) {
        /* remove cast from const when the pml layer is updated to take
         * a const for the send buffer. */
        rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, edges[neighbor],
                                MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                                comm, preqs++));
        if (OMPI_SUCCESS != rc) break;
        sbuf = (const char *) sbuf + sdextent * scount;
    }

    if( MPI_SUCCESS != rc ) {
        ompi_coll_base_free_reqs( reqs, degree + neighbor + 1 );
        return rc;
    }

    rc = ompi_request_wait_all (degree * 2, reqs, MPI_STATUSES_IGNORE);
    if( MPI_SUCCESS != rc ) {
        ompi_coll_base_free_reqs( reqs, 2 * degree );
    }
    return rc;
}

static int
mca_coll_basic_neighbor_alltoall_dist_graph(const void *sbuf, int scount,struct ompi_datatype_t *sdtype, void *rbuf,
                                            int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    const mca_topo_base_comm_dist_graph_2_2_0_t *dist_graph = comm->c_topo->mtc.dist_graph;
    ptrdiff_t lb, rdextent, sdextent;
    int rc = MPI_SUCCESS, neighbor;
    const int *inedges, *outedges;
    int indegree, outdegree;
    ompi_request_t **reqs, **preqs;

    indegree = dist_graph->indegree;
    outdegree = dist_graph->outdegree;

    inedges = dist_graph->in;
    outedges = dist_graph->out;

    ompi_datatype_get_extent(rdtype, &lb, &rdextent);
    ompi_datatype_get_extent(sdtype, &lb, &sdextent);
    reqs = preqs = coll_base_comm_get_reqs( module->base_data, indegree + outdegree);
    if( NULL == reqs ) { return OMPI_ERR_OUT_OF_RESOURCE; }

    /* post receives first */
    for (neighbor = 0; neighbor < indegree ; ++neighbor) {
        rc = MCA_PML_CALL(irecv(rbuf, rcount, rdtype, inedges[neighbor],
                                MCA_COLL_BASE_TAG_ALLTOALL,
                                comm, preqs++));
        if (OMPI_SUCCESS != rc) break;
        rbuf = (char *) rbuf + rdextent * rcount;
    }

    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs(reqs, neighbor + 1);
        return rc;
    }

    for (neighbor = 0 ; neighbor < outdegree ; ++neighbor) {
        /* remove cast from const when the pml layer is updated to take a const for the send buffer */
        rc = MCA_PML_CALL(isend((void *) sbuf, scount, sdtype, outedges[neighbor],
                                MCA_COLL_BASE_TAG_ALLTOALL, MCA_PML_BASE_SEND_STANDARD,
                                comm, preqs++));
        if (OMPI_SUCCESS != rc) break;
        sbuf = (char *) sbuf + sdextent * scount;
    }

    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs(reqs, indegree + neighbor + 1);
        return rc;
    }

    rc = ompi_request_wait_all (indegree + outdegree, reqs, MPI_STATUSES_IGNORE);
    if (OMPI_SUCCESS != rc) {
        ompi_coll_base_free_reqs(reqs, indegree + outdegree);
    }
    return rc;
}

int mca_coll_basic_neighbor_alltoall(const void *sbuf, int scount, struct ompi_datatype_t *sdtype, void *rbuf,
                                     int rcount, struct ompi_datatype_t *rdtype, struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module)
{
    if (OMPI_COMM_IS_INTER(comm)) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    if (OMPI_COMM_IS_CART(comm)) {
        return mca_coll_basic_neighbor_alltoall_cart (sbuf, scount, sdtype, rbuf,
                                                      rcount, rdtype, comm, module);
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        return mca_coll_basic_neighbor_alltoall_graph (sbuf, scount, sdtype, rbuf,
                                                       rcount, rdtype, comm, module);
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        return mca_coll_basic_neighbor_alltoall_dist_graph (sbuf, scount, sdtype, rbuf,
                                                            rcount, rdtype, comm, module);
    }

    return OMPI_ERR_NOT_SUPPORTED;
}
