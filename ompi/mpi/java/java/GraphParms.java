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
 * File         : GraphParms.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.1 $
 * Updated      : $Date: 1998/08/26 18:49:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Graph topology information associated with a communicator.
 */
public final class GraphParms
{
/** Node degrees. */
private final int[] index;

/** Graph edges. */
private final int[] edges;

/**
 * Constructs a graph topology information object.
 * @param index node degrees.
 * @param edges graph edges.
 */
protected GraphParms(int[] index, int[] edges)
{
    this.index = index;
    this.edges = edges;
}

/**
 * Returns the number of nodes.
 * @return number of nodes.
 */
public int getIndexCount()
{
    return index.length;
}

/**
 * Returns the index of the node {@code i}.
 * <p>{@code getIndex(0)} returns the degree of the node {@code 0}, and
 * {@code getIndex(i)-getIndex(i-1)} is the degree of the node {@code i}.
 * @param i position of the node.
 * @return the index.
 */
public int getIndex(int i)
{
    return index[i];
}

/**
 * Returns the number of edges.
 * @return number of edges.
 */
public int getEdgeCount()
{
    return edges.length;
}

/**
 * Returns the edge {@code i}.
 * <p>The list of neighbors of node zero is stored in {@code getEdge(j)},
 * for {@code 0} &le; {@code j} &le; {@code getIndex(0)-1} and the list
 * of neighbors of node {@code i}, {@code i} &gt; {@code 0}, is stored
 * in {@code getEdge(j)}, {@code getIndex(i-1)} &le; {@code j} &le;
 * {@code getIndex(i)-1}.
 * @param i index of the edge.
 * @return the edge.
 */
public int getEdge(int i)
{
    return edges[i];
}

} // GraphParms
