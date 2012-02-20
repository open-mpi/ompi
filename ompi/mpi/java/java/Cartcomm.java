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
 * File         : Cartcomm.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.7 $
 * Updated      : $Date: 2001/10/22 21:07:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

public class Cartcomm extends Intracomm {

  protected Cartcomm(long handle) throws MPIException {
    super(handle) ;
  }

  public Object clone() {
    try {
      return new Cartcomm(super.dup()) ;
    }
    catch (MPIException e) {
      throw new RuntimeException(e.getMessage()) ;
    }
  }

  /**
   * Returns Cartesian topology information.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> object containing dimensions,
   *                                      periods and local coordinates </tr>
   * </table>
   * <p>
   * Java binding of the MPI operations <tt>MPI_CARTDIM_GET</tt> and
   * <tt>MPI_CART_GET</tt>.
   * <p>
   * The number of dimensions can be obtained from the size of (eg)
   * <tt>dims</tt> field of the returned object.
   */

  public native CartParms Get() throws MPIException ;

  /**
   * Translate logical process coordinates to process rank.
   * <p>
   * <table>
   * <tr><td><tt> coords   </tt></td><td> Cartesian coordinates of a
   *                                      process </tr>
   * <tr><td><em> returns: </em></td><td> rank of the specified process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_RANK</tt>.
   */

  public native int Rank(int[] coords) throws MPIException ;

  /**
   * Translate process rank to logical process coordinates.
   * <p>
   * <table>
   * <tr><td><tt> rank     </tt></td><td> rank of a process </tr>
   * <tr><td><em> returns: </em></td><td> Cartesian coordinates of the
   *                                      specified process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_COORDS</tt>.
   */

  public native int [] Coords(int rank) throws MPIException ;

  /**
   * Compute source and destination ranks for ``shift'' communication.
   * <p>
   * <table>
   * <tr><td><tt> direction </tt></td><td> coordinate dimension of shift </tr>
   * <tr><td><tt> disp      </tt></td><td> displacement </tr>
   * <tr><td><em> returns:  </em></td><td> object containing ranks of source
   *                                       and destination processes </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_SHIFT</tt>.
   */

  public native ShiftParms Shift(int direction, int disp) throws MPIException ;

  /**
   * Partition Cartesian communicator into subgroups of lower dimension.
   * <p>
   * <table>
   * <tr><td><tt> remain_dims </tt></td><td> by dimension, <tt>true</tt> if
   *                                         dimension is to be kept,
   *                                         <tt>false</tt> otherwise </tr>
   * <tr><td><em> returns:    </em></td><td> communicator containing subgrid
   *                                         including this process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_SUB</tt>.
   */

  public Cartcomm Sub(boolean [] remain_dims) throws MPIException {
    return new Cartcomm(sub(remain_dims)) ;
  }

  private native long sub(boolean [] remain_dims);

  /**
   * Compute an optimal placement.
   * <p>
   * <table>
   * <tr><td><tt> dims     </tt></td><td> the number of processes in each
   *                                      dimension </tr>
   * <tr><td><tt> periods  </tt></td><td> <tt>true</tt> if grid is periodic,
   *                                      <tt>false</tt> if not, in each
   *                                      dimension </tr>
   * <tr><td><em> returns: </em></td><td> reordered rank of calling
   *                                      process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_MAP</tt>.
   * <p>
   * The number of dimensions is taken to be size of the <tt>dims</tt> argument.
   */

  public native int Map(int [] dims, boolean [] periods) throws MPIException ;
 
  /**
   * Select a balanced distribution of processes per coordinate direction.
   * <p>
   * <table>
   * <tr><td><tt> nnodes   </tt></td><td> number of nodes in a grid </tr>
   * <tr><td><tt> ndims    </tt></td><td> number of dimensions of grid </tr>
   * <tr><td><tt> dims     </tt></td><td> array specifying the number of nodes
   *                                      in each dimension </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_DIMS_CREATE</tt>.
   * <p>
   * Size <tt>dims</tt> should be <tt>ndims</tt>.  Note that
   * <tt>dims</tt> is an <em>inout</em> parameter.
   */

  static public native void Dims_create(int nnodes, int[] dims)
                                                      throws MPIException ;
}

