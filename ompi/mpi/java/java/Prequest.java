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
 * File         : Prequest.java
 * Author       : Sang Lim, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2001/10/22 21:07:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */


/*
 * Note: in the end there is no sensible way to use the native
 * persistent requests here.  For every `start'/`wait' cycle you need
 * to do `get...Elements', `release...Elements', otherwise the behaviour
 * is wrong if pinning isn't supported (because then get/release ops
 * need active copying to move values between C and Java).
 *
 * (Even if pinning is supported, the arrays would have to be pinned
 * almost permanently, which presumably isn't a good thing.)
 */


package mpi;

public class Prequest extends Request {

  protected final static int MODE_STANDARD    = 0 ;
  protected final static int MODE_BUFFERED    = 1 ;
  protected final static int MODE_SYNCHRONOUS = 2 ;
  protected final static int MODE_READY       = 3 ;

  private int src ;

  /**
   * Constructor used by `Send_init', etc.
   */

  protected Prequest(int mode, 
                     Object buf, int offset, int count, Datatype type,
                     int dest, int tag, Comm comm) {

    opTag   = Request.OP_SEND ;

    this.mode   = mode ;

    this.buf    = buf;
    this.offset = offset;
    this.count  = count;
    this.type   = type;
    this.dest   = dest;
    this.tag    = tag;
    this.comm   = comm ;

    if(type.isObject()) {
      typeTag    = Request.TYPE_OBJECT ;

      length_buf = new int [2] ;
      hdrReq     = new Request() ;
    }
    else
      typeTag    = Request.TYPE_NORMAL ;
  }

  /**
   * Constructor used by `Recv_init'.
   */

  protected Prequest(Object buf, int offset, int count, Datatype type,
                     int source, int tag, Comm comm) {

    opTag   = Request.OP_RECV ;

    this.buf    = buf;
    this.offset = offset;
    this.count  = count;
    this.type   = type;
    this.src    = source;
    this.tag    = tag;
    this.comm   = comm;

    if(type.isObject()) {
      typeTag    = Request.TYPE_OBJECT ;

      length_buf = new int [2] ;
    }
    else
      typeTag    = Request.TYPE_NORMAL ;
  }


  /**
   * Activate a persistent communication request.
   * Java binding of the MPI operation <tt>MPI_START</tt>.
   * The communication is completed by using the request in
   * one of the <tt>wait</tt> or <tt>test</tt> operations.
   * On successful completion the request becomes inactive again.
   * It can be reactivated by a further call to <tt>Start</tt>.
   */

  public void Start() throws MPIException {
    switch(typeTag) {
      case TYPE_NORMAL :
        switch(opTag) {
          case OP_SEND :

            switch(mode) {
              case MODE_STANDARD :
	        comm.Isend(buf, offset, count, type, dest, tag, this);

                break;
              case MODE_BUFFERED :
	        comm.Ibsend(buf, offset, count, type, dest, tag, this);

                break;
              case MODE_SYNCHRONOUS :
	        comm.Issend(buf, offset, count, type, dest, tag, this);

                break;
              case MODE_READY :
	        comm.Irsend(buf, offset, count, type, dest, tag, this);

                break;
            }

            break ;
          case OP_RECV :
            comm.Irecv(buf, offset, count, type, src, tag, this) ;

            break ;
        }

        break ;
      case TYPE_OBJECT :
        switch(opTag) {
          case OP_SEND :

            byte [] byte_buf = comm.Object_Serialize(buf,offset,count,type);

            length_buf[0] = byte_buf.length;
            length_buf[1] = count ;

            switch(mode) {
              case MODE_STANDARD :
                comm.Isend(length_buf, 0, 2, MPI.INT, dest, tag, hdrReq) ;
	        comm.Isend(byte_buf, 0, byte_buf.length,
		           MPI.BYTE, dest, tag, this);

                break;
              case MODE_BUFFERED :
                comm.Ibsend(length_buf, 0, 2, MPI.INT, dest, tag, hdrReq) ;
                comm.Ibsend(byte_buf, 0, byte_buf.length, 
                            MPI.BYTE, dest, tag, this);

                break;
              case MODE_SYNCHRONOUS :
                comm.Issend(length_buf, 0, 2, MPI.INT, dest, tag, hdrReq) ;
	        comm.Isend(byte_buf, 0, byte_buf.length,
		           MPI.BYTE, dest, tag, this);

                break;
              case MODE_READY :
                comm.Irsend(length_buf, 0, 2, MPI.INT, dest, tag, hdrReq) ;
                comm.Isend(byte_buf, 0, byte_buf.length, 
                           MPI.BYTE, dest, tag, this);

                break;
            }

            break ;
          case OP_RECV :
            comm.Irecv(length_buf, 0, 2, MPI.INT, src, tag, this) ;

            break ;
        }

        break ;
    }   	   
  }

  //private native void start();

  /**
   * Activate a list of communication requests.
   * <p>
   * <table>
   * <tr><td><tt> array_of_requests </tt></td><td> array of requests </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_STARTALL</tt>.
   */

  public static void Startall(Prequest [] array_of_request)
                                                       throws MPIException {
    int req_length = array_of_request.length ;

    for (int i = 0; i<req_length; i++)
      array_of_request[i].Start() ;
  }

  //private static native void startall(Prequest [] array_of_request);

}

