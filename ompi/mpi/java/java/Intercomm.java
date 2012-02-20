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
 * File         : Intercomm.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.5 $
 * Updated      : $Date: 1999/09/14 20:50:11 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;
//import mpi.*;

public class Intercomm extends Comm {

  protected Intercomm(long handle) {super(handle) ;}

  public Object clone() {
    return new Intercomm(super.dup());
  }

  // Inter-Communication

  /**
   * Size of remote group.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> number of process in remote group
   *                                      of this communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_REMOTE_SIZE</tt>.
   */

  public native int Remote_size() throws MPIException ;

  /**
   * Return the remote group.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> remote group of this
   *                                      communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_REMOTE_GROUP</tt>.
   */

  public Group Remote_group() throws MPIException {
    return new Group(remote_group());
  }

  private native long remote_group();

  /**
   * Create an inter-communicator.
   * <p>
   * <table>
   * <tr><td><tt> high     </tt></td><td> true if the local group has higher
   *                                      ranks in combined group </tr>
   * <tr><td><em> returns: </em></td><td> new intra-communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_INTERCOMM_MERGE</tt>.
   */

  public Intracomm Merge(boolean high) throws MPIException {
    return new Intracomm(merge(high)) ;
  }

  private native long merge(boolean high);
}

