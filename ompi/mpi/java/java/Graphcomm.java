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

public class Graphcomm extends Intracomm {

  protected Graphcomm(long handle) throws MPIException {
    super(handle) ;
  }

  public Object clone() {
    try {
      return new Graphcomm(super.dup()) ;
    }
    catch (MPIException e) {
      throw new RuntimeException(e.getMessage()) ;
    }
  }

  /**
   * Returns graph topology information.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> object defining node degress and
   *                                      edges of graph </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GRAPHDIMS_GET</tt>.
   * <p>
   * The number of nodes and number of edges can be extracted
   * from the sizes of the <tt>index</tt> and <tt>edges</tt> fields
   * of the returned object.
   */

  public native GraphParms Get() throws MPIException ;

  /**
   * Provides adjacency information for general graph topology.
   * <p>
   * <table>
   * <tr><td><tt> rank     </tt></td><td> rank of a process in the group
   *                                      of this communicator </tr>
   * <tr><td><em> returns: </em></td><td> array of ranks of neighbouring
   *                                      processes to one specified </tr>
   * </table>
   * <p>
   * Java binding of the MPI operations <tt>MPI_GRAPH_NEIGHBOURS_COUNT</tt>
   * and <tt>MPI_GRAPH_NEIGHBOURS</tt>.
   * <p>
   * The number of neighbours can be extracted from the size of the result.
   */

  public native int [] Neighbours(int rank) throws MPIException ;

  /**
   * Compute an optimal placement.
   * <p>
   * <table>
   * <tr><td><tt> index    </tt></td><td> node degrees </tr>
   * <tr><td><tt> edges    </tt></td><td> graph edges </tr>
   * <tr><td><em> returns: </em></td><td> reordered rank of calling
   *                                      process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GRAPH_MAP</tt>.
   * <p>
   * The number of nodes is taken to be size of the <tt>index</tt> argument.
   */

  public native int Map(int [] index, int [] edges) throws MPIException ;

}

