/*
 * $HEADER$
 */ 

#include "ompi_config.h"
#include "mca/topo/unity/src/topo_unity.h"

#include "communicator/communicator.h"

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
                              int *newrank)
{
    int myrank;

    myrank = ompi_comm_rank(comm);
    *newrank = 
        ((0 > myrank) || (myrank >= nnodes)) ? MPI_UNDEFINED : myrank;
    
    return OMPI_SUCCESS;
}
