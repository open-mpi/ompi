/*
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * File         : Graphcomm.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.5 $
 * Updated      : $Date: 2001/10/22 21:07:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Communicator with graph structure.
 */
public final class GraphComm extends Intracomm
{
static
{
    init();
}

private static native void init();

protected GraphComm(long handle) throws MPIException
{
    super(handle);
}

/**
 * Duplicate this communicator.
 * <p>Java binding of the MPI operation {@code MPI_COMM_DUP}.
 * <p>The new communicator is "congruent" to the old one,
 *    but has a different context.
 * @return copy of this communicator
 */
@Override public GraphComm clone()
{
    try
    {
        MPI.check();
        return new GraphComm(dup());
    }
    catch(MPIException e)
    {
        throw new RuntimeException(e.getMessage());
    }
}

/**
 * Returns graph topology information.
 * <p>Java binding of the MPI operations {@code MPI_GRAPHDIMS_GET}
 * and {@code MPI_GRAPH_GET}.
 * <p>The number of nodes and number of edges can be extracted
 * from the sizes of the {@code index} and {@code edges} fields
 * of the returned object.
 * @return object defining node degress and edges of graph
 * @throws MPIException
 */
public GraphParms getDims() throws MPIException
{
    MPI.check();
    return getDims(handle);
}

private native GraphParms getDims(long comm) throws MPIException;

/**
 * Provides adjacency information for general graph topology.
 * <p>Java binding of the MPI operations {@code MPI_GRAPH_NEIGHBORS_COUNT}
 * and {@code MPI_GRAPH_NEIGHBORS}.
 * <p>The number of neighbors can be extracted from the size of the result.
 * @param rank rank of a process in the group of this communicator
 * @return array of ranks of neighbouring processes to one specified
 * @throws MPIException
 */
public int[] getNeighbors(int rank) throws MPIException
{
    MPI.check();
    return getNeighbors(handle, rank);
}

private native int[] getNeighbors(long comm, int rank) throws MPIException;

/**
 * Gets the adjacency information for a distributed graph topology.
 * @return adjacency information for a distributed graph topology
 * @throws MPIException 
 */
public DistGraphNeighbors getDistGraphNeighbors() throws MPIException
{
    MPI.check();
    return getDistGraphNeighbors(handle);
}

private native DistGraphNeighbors getDistGraphNeighbors(long comm)
        throws MPIException;

/**
 * Compute an optimal placement.
 * <p>Java binding of the MPI operation {@code MPI_GRAPH_MAP}.
 * <p>The number of nodes is taken to be size of the {@code index} argument.
 * @param index node degrees
 * @param edges graph edges
 * @return reordered rank of calling process
 * @throws MPIException
 */
public int map(int[] index, int[] edges) throws MPIException
{
    MPI.check();
    return map(handle, index, edges);
}

private native int map(long comm, int[] index, int[] edges) throws MPIException;

} // Graphcomm
