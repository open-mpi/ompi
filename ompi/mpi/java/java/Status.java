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
 * File         : Status.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.15 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

public class Status extends Freeable {

  public int index;
  public int source;
  public int tag;

  int elements; 

  //protected int count; 
  protected int object_count;

//  protected Status(long _handle) { handle = _handle;}

  public Status() {alloc() ;}
  private native void alloc() ;

  @SuppressWarnings("unchecked")
  public void finalize() throws MPIException {
      synchronized(MPI.class) {
          MPI.freeList.addFirst(this) ;
      }
  }

  native void free() ;

  /**
   * Get the number of received entries.
   * <p>
   * <table>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in receive
   *                                      buffer </tr>
   * <tr><td><em> returns: </em></td><td> number of received entries </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GET_COUNT</tt>.
   */

  public int Get_count(Datatype datatype) throws MPIException {
    
    if (datatype.isObject())
      return object_count;    // Is this correct?
    else
      return get_count(datatype);
  }

  private native int get_count(Datatype datatype);

  /**
   * Test if communication was cancelled.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> true if the operation was
   *                                      succesfully cancelled,
   *                                      false otherwise
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TEST_CANCELLED</tt>.
   */

  public native boolean Test_cancelled() throws MPIException ;

  /**
   * Retrieve number of basic elements from status.
   * <p>
   * <table>
   * <tr><td><tt> datatype </tt></td><td> datatype used by receive
   *                                      operation </tr>
   * <tr><td><em> returns: </em></td><td> number of received basic
   *                                      elements </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GET_ELEMENTS</tt>.
   */

  public int Get_elements(Datatype datatype) throws MPIException {
    if(datatype.isObject())
      return MPI.UNDEFINED;  // Is this correct?
    else 
      return get_elements(datatype) ;
  }

  private native int get_elements(Datatype datatype);

  private static native void init(); 

  protected long handle;

  static {
    init();
  }          

}

// Things to do
//

