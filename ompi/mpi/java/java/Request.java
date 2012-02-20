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
/* * File         : Request.java
 * Author       : Sang Lim, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2001/08/07 16:36:25 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

/*
 * Note: in a send request for a buffer containing objects, the primary
 * `MPI_Request' referenced by `handle' is the request to send the data.
 * The request to send the header is in the secondary field, `hdrReq'.
 * Conversely, in a *receive* request for a buffer containing objects
 * the primary `MPI_Request' is the request to send the header.
 * The receive of the data is not initiated until a `wait' or `test'
 * operation succeeds.
 */

/*
 * Probably `Request' should be an abstract class, and there should
 * be several concrete subclasses.  At the moment requests are created
 * in a few different ways, and the differently constructed requests are
 * typically using different subsets of fields.  DBC 7/12/01
 */

package mpi;

public class Request {
  protected final static int NULL = 0;

  protected final static int TYPE_NORMAL = 0;
  protected final static int TYPE_OBJECT = 1;

  protected final static int OP_SEND     = 0;
  protected final static int OP_RECV     = 1;

  protected Request hdrReq ;
  protected int typeTag = TYPE_NORMAL   ;
  protected int opTag ;
  protected int mode ;
  protected Object buf;
  protected int offset;
  protected int count;
  protected Datatype type;
  protected int dest;
  protected int tag;
  protected Comm comm;
  protected int[] length_buf;


  private static native void init();
 
  private native void GetReq(int Type);


  protected Request() {}

  protected Request(int Type) { GetReq(Type); }

  /**
   * Constructor used by <tt>Isend</tt>, etc.
   */

  protected Request(Request hdrReq) {

    typeTag = Request.TYPE_OBJECT ;
    opTag   = Request.OP_SEND ;

    this.hdrReq = hdrReq ;
  }

  /**
   * Constructor used by <tt>Irecv</tt>.
   */

  protected Request(Object buf, int offset, int count, Datatype type,
                    int tag, Comm comm,
                    int [] length_buf) {

    typeTag = Request.TYPE_OBJECT ;
    opTag   = Request.OP_RECV ;

    this.buf    = buf;
    this.offset = offset;
    this.count  = count;
    this.type   = type;
    this.tag    = tag;
    this.comm   = comm;

    this.length_buf = length_buf;
  }


  /**
   * Set the request object to be void.
   * Java binding of the MPI operation <tt>MPI_REQUEST_FREE</tt>.
   */

  public native void Free() throws MPIException ;

  /**
   * Mark a pending nonblocking communication for cancellation.
   * Java binding of the MPI operation <tt>MPI_CANCEL</tt>.
   */

  public native void Cancel() throws MPIException ;

  /**
   * Test if request object is void.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> true if the request object is void,
   *                                      false otherwise </tr>
   * </table>
   */

  public native boolean Is_null();

  /*
   * After initial wait succeeds with some status, complete as necessary.
   */

  private Status complete(Status status) throws MPIException {
    switch(typeTag) {
      case TYPE_NORMAL :

        break;
      case TYPE_OBJECT :
        switch(opTag) {
          case OP_SEND :
            hdrReq.Wait(new Status()) ;  // Data has already gone, but must
                                         // still do `wait' on header send.
            break;
          case OP_RECV :

            int index = status.index ;

            // Header has arrived, now read the actual data.

            byte[] byte_buf = new byte[length_buf[0]];
            status = comm.Recv(byte_buf, 0, length_buf[0], MPI.BYTE,
                               status.source, tag) ;
            comm.Object_Deserialize(buf, byte_buf, offset, length_buf[1],
                                    type);

            status.object_count = length_buf[1];
            status.index        = index ;

            break;
        }

        break ;
    }
    return status ;
  }

  /**
   * Blocks until the operation identified by the request is complete.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WAIT</tt>.
   * <p>
   * After the call returns, the request object becomes inactive.
   */

  public Status Wait() throws MPIException {
    Status result = new Status();
    Wait(result);

    return complete(result) ;
  }

  private native Status Wait(Status stat);

  /**
   * Returns a status object if the operation identified by the request
   * is complete, or a null reference otherwise.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> status object or null reference </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TEST</tt>.
   * <p>
   * After the call, if the operation is complete (ie, if the return value
   * is non-null), the request object becomes inactive.
   */

  public Status Test() throws MPIException {

    Status result = new Status();
    if (Test(result) == null)
      return null;
    else
      return complete(result) ;
  }

  private native Status Test(Status stat);

  /**
   * Blocks until one of the operations associated with the active
   * requests in the array has completed.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WAITANY</tt>.
   * <p>
   * The index in <tt>array_of_requests</tt> for the request that completed
   * can be obtained from the returned status object through the
   * <tt>Status.index</tt> field.  The corresponding element
   * of <tt>array_of_requests</tt> becomes inactive.
   */

  public static Status Waitany(Request [] array_of_request)
                                                      throws MPIException {
    Status result = new Status();
    Waitany(array_of_request, result);
    
    if(result == null)
      return null;
    else
      return array_of_request[result.index].complete(result) ; 
  }

  private static native Status Waitany(Request [] array_of_request, 
                                       Status stat);

  /**
   * Tests for completion of either one or none of the operations associated
   * with active requests.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> status object or
   *                                               null reference </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TESTANY</tt>.
   * <p>
   * If some request completed, the index in <tt>array_of_requests</tt>
   * for that request can be obtained from the returned status object
   * through the.  The corresponding element of <tt>array_of_requests</tt>
   * becomes inactive.
   * If no request completed, <tt>Testany</tt> returns a null reference.
   */

  public static Status Testany(Request [] array_of_request)
                                                      throws MPIException {
 
    Status result = new Status();
    result = Testany(array_of_request, result);
    
    if(result == null)
      return null;
    else
      return array_of_request[result.index].complete(result) ; 
  }

  private static native Status Testany(Request [] array_of_request, 
                                       Status     stat);

  /**
   * Blocks until all of the operations associated with the active
   * requests in the array have completed.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> array of status objects </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WAITALL</tt>.
   * <p>
   * The result array will be the same size as <tt>array_of_requests</tt>.
   * On exit, requests become inactive.  If the <em>input</em> value of
   * <tt>arrayOfRequests</tt> contains inactive requests, corresponding
   * elements of the result array will contain null status references.
   */

  public static Status[] Waitall (Request [] array_of_request)
                                                      throws MPIException {
    Status result[] = waitall(array_of_request);
            
    for (int i = 0 ; i < array_of_request.length ; i++)
      result [i] = array_of_request [i].complete(result [i]) ; 

    return result;
  }

  private static native Status[] waitall(Request [] array_of_request);

  /**
   * Tests for completion of <em>all</em> of the operations associated
   * with active requests.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> array of status objects </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TESTALL</tt>.
   * <p>
   * If all operations have completed, the exit value of the argument array
   * and the result array are as for <tt>Waitall</tt>.  If any
   * operation has not completed, the result value is null and no
   * element of the argument array is modified.
   */

  public static Status[] Testall(Request [] array_of_request)
                                                      throws MPIException {
    Status result[] = testall(array_of_request);

    if (result == null)
      return null;
    else {
      for (int i = 0 ; i < array_of_request.length ; i++)
        result [i] = array_of_request [i].complete(result [i]) ; 

      return result;
    }
  }

  private static native Status[] testall(Request [] array_of_request);

  /**
   * Blocks until at least one of the operations associated with the active
   * requests in the array has completed.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> array of status objects </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WAITSOME</tt>.
   * <p>
   * The size of the result array will be the number of operations that
   * completed.  The index in <tt>array_of_requests</tt> for each request that
   * completed can be obtained from the returned status objects through the
   * <tt>Status.index</tt> field.  The corresponding element in
   * <tt>array_of_requests</tt> becomes inactive.
   */

  public static Status[] Waitsome(Request [] array_of_request)
                                                      throws MPIException {
    Status result[] = waitsome(array_of_request);                         
            
    for (int i = 0 ; i < result.length ; i++)
      result [i] = array_of_request [result [i].index].complete(result [i]) ; 

    return result;    
  }

  private static native Status[] waitsome(Request [] array_of_request);

  /**
   * Behaves like <tt>Waitsome</tt>, except that it returns immediately.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * <tr><td><em> returns:          </em></td><td> array of status objects </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TESTSOME</tt>.
   * <p>
   * If no operation has completed, <tt>TestSome</tt> returns an array of
   * length zero and elements of <tt>array_of_requests</tt> are unchanged.
   * Otherwise, arguments and return value are as for <tt>Waitsome</tt>.
   */

  public static Status[] Testsome(Request [] array_of_request)
                                                      throws MPIException {
    Status result[] = testsome(array_of_request);

    if (result  == null)
      return null;
    else {
      for (int i = 0 ; i < result.length ; i++)
        result [i] = array_of_request [result [i].index].complete(result [i]) ; 

      return result;    
    }
  }

  private static native Status[] testsome(Request [] array_of_request);

  // Fields manipulated only by native methods...

  protected long handle;

  // `bufSave', etc, not generally the same as `buf', etc.
  // In `MPJ.OBJECT' receive case `buf', etc, refer to the array of objects,
  // `bufSave', etc refer to header buffer.

  protected Object bufSave ;
  protected int countSave, offsetSave ;

  protected long bufbaseSave, bufptrSave ;
  protected int baseTypeSave ;
  protected long commSave, typeSave ;

  static {
    init();
  }          
}

// Things to do
//
//   What happens to `Cancel' in the object case?

