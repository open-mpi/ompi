/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */

#include "comm_helpers.h"

int ompi_comm_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted) {
    int res;

    if (OMPI_COMM_IS_CART(comm)) { 
        int ndims;
        res = MPI_Cartdim_get(comm, &ndims)  ;
        if (MPI_SUCCESS != res) {
            return res;
        }
        /* outdegree is always 2*ndims because we need to iterate over empty buffers for MPI_PROC_NULL */
        *outdegree = *indegree = 2*ndims;
        *weighted = 0;
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        int rank, nneighbors;
        MPI_Comm_rank(comm, &rank);
        res = MPI_Graph_neighbors_count(comm, rank, &nneighbors);
        if (MPI_SUCCESS != res) {
            return res;
        }
        *outdegree = *indegree = nneighbors;
        *weighted = 0;
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        res = MPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted);
    } else {
        return MPI_ERR_ARG;
    }

    return MPI_SUCCESS;
}

int ompi_comm_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[], int maxoutdegree, int destinations[], int destweights[]) {
    int res;
    int index = 0;

    int indeg, outdeg, wgtd;
    res = ompi_comm_neighbors_count(comm, &indeg, &outdeg, &wgtd);
    if (MPI_SUCCESS != res) {
        return res;
    }
    if(indeg > maxindegree && outdeg > maxoutdegree) return MPI_ERR_TRUNCATE; /* we want to return *all* neighbors */

    if (OMPI_COMM_IS_CART(comm)) { 
        int ndims, i, rpeer, speer;
        res = MPI_Cartdim_get(comm, &ndims);
        if (MPI_SUCCESS != res) {
            return res;
        }

        for(i = 0; i<ndims; i++) {
          res = MPI_Cart_shift(comm, i, 1, &rpeer, &speer);
          if (MPI_SUCCESS != res) {
              return res;
          }
          sources[index] = destinations[index] = rpeer; index++;
          sources[index] = destinations[index] = speer; index++;
        }
    } else if (OMPI_COMM_IS_GRAPH(comm)) {
        int rank;
        MPI_Comm_rank(comm, &rank);
        res = MPI_Graph_neighbors(comm, rank, maxindegree, sources);
        if (MPI_SUCCESS != res) {
            return res;
        }
        for(int i=0; i<maxindegree; i++) destinations[i] = sources[i];
    } else if (OMPI_COMM_IS_DIST_GRAPH(comm)) {
        res = MPI_Dist_graph_neighbors(comm, maxindegree, sources, sourceweights, maxoutdegree, destinations, destweights);
        if (MPI_SUCCESS != res) {
            return res;
        }
    } else {
        return MPI_ERR_ARG;
    }

    return MPI_SUCCESS;
}
