/*
 * $HEADER$
 */ 
#include "mca/topo/unity/src/topo_unity.h"

/*
 * function - mca_topo_unity_graph_map
 *
 *  @param comm input communicator (handle)
 *  @param nnodes number of graph nodes (integer)
 *  @param index integer array specifying the graph structure
 *  @param edges integer array specifying the graph structure
 *  @param newrank reordered rank of the calling process; 'MPI_UNDEFINED'
 *                  if the calling process does not belong to 
 *                  graph (integer)
 *
 *  @retval MPI_SUCCESS
 *  @retval MPI_UNDEFINED
 */ 

int mca_topo_unity_graph_map (MPI_Comm comm,
                              int nnodes,
                              int *index,
                              int *edges,
                              int *newrank){

    int errcode;
    int myrank;

#if 0
    errcode = lam_comm_rank (comm, &myrank);
#endif
    if (LAM_SUCCESS != errcode) {
        printf ("failed to get a comm rank\n");
        return LAM_ERROR;
    }

    *newrank = 
        ((0 > myrank) || (myrank >= nnodes)) ? MPI_UNDEFINED : myrank;
    
    return LAM_SUCCESS;
}
