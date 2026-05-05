/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mpi.h"
#include "ompi/mpi/fortran/base/fortran_base_topo_neighbors.h"

int ompi_fortran_neighbor_count(MPI_Comm comm, int *indegree, int *outdegree)
{
    int ret;
    int topo_type, ndims, nneighbors, weighted, my_rank;

    if ((NULL == indegree) || (NULL == outdegree)) {
        ret = MPI_ERR_ARG;
        goto fn_exit;
    }
        
    ret = PMPI_Topo_test(comm, &topo_type);
    if (MPI_SUCCESS != ret) {
        goto fn_exit;
    }

    switch (topo_type) {
        case MPI_CART:
            ret = PMPI_Cartdim_get(comm, &ndims);
            if (MPI_SUCCESS != ret) {
                goto fn_exit;
            }
            *outdegree = *indegree = 2 * ndims; 
            break;
        case MPI_GRAPH:
            ret = PMPI_Comm_rank(comm, &my_rank);
            if (MPI_SUCCESS != ret) {
                goto fn_exit;
            }
            ret = PMPI_Graph_neighbors_count(comm, my_rank, &nneighbors);
            if (MPI_SUCCESS != ret) {
                goto fn_exit;
            }
            *outdegree = *indegree = nneighbors;
            break;
        case MPI_DIST_GRAPH:
            ret = PMPI_Dist_graph_neighbors_count(comm, indegree, outdegree, &weighted);
            if (MPI_SUCCESS != ret) {
                goto fn_exit;
            }
            break;
        case MPI_UNDEFINED:
        default:
            break;
    }

fn_exit:
    return ret;
}
