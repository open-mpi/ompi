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
 * File         : Comm.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.20 $
 * Updated      : $Date: 2001/08/07 16:36:25 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;
import java.io.*;
import java.lang.*;

public class Comm {
  protected final static int SELF  = 1;
  protected final static int WORLD = 2;

  protected static long nullHandle ;

  Comm() {}

  void setType(int Type) {
    GetComm(Type);
  }

  private native void GetComm(int Type);

  protected Comm(long handle) {
    this.handle = handle;
  }  

  /**
   * Duplicate this communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> copy of this communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_DUP</tt>.
   * <p>
   * The new communicator is ``congruent'' to the old one, but has a
   * different context.
   */

  public Object clone() {
    return new Comm(dup());
  }

  protected native long dup();

  /**
   * Size of group of this communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> number of processors in the group
   *                                      of this communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_SIZE</tt>.
   */

  public native int Size() throws MPIException ;

  /**
   * Rank of this process in group of this communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> rank of the calling process in the
   *                                      group of this communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_RANK</tt>.
   */

  public native int Rank() throws MPIException ;

  /**
   * Compare two communicators.
   * <p>
   * <table>
   * <tr><td><tt> comm1    </tt></td><td> first communicator </tr>
   * <tr><td><tt> comm2    </tt></td><td> second communicator </tr>
   * <tr><td><em> returns: </em></td><td> result </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_COMPARE</tt>.
   * <p>
   * <tt>MPI.IDENT</tt> results if the <tt>comm1</tt> and <tt>comm2</tt>
   * are references to the same object (ie, if <tt>comm1 == comm2</tt>).
   * <tt>MPI.CONGRUENT</tt> results if the underlying groups are identical
   * but the communicators differ by context.
   * <tt>MPI.SIMILAR</tt> results if the underlying groups are similar
   * but the communicators differ by context.
   * <tt>MPI.UNEQUAL</tt> results otherwise.
   */

  public static native int Compare(Comm comm1, Comm comm2) throws MPIException ;

  /**
   * Destroy this communicator.
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_FREE</tt>.
   */

  public native void Free() throws MPIException ;

  /**
   * Test if communicator object is void (has been freed).
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> true if the comm object is void,
   *                                      false otherwise </tr>
   * </table>
   */

  public native boolean Is_null();

  /**
   * Return group associated with a communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> group corresponding to this
   *                                      communicator group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_GROUP</tt>.
   */

  public Group Group() throws MPIException {
    return new Group(group());
  }

  private native long group();

  // Inter-communication

  /**
   * Test if this communicator is an inter-communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> <tt>true</tt> if this is an
   *                                      inter-communicator,
   *                                      <tt>false</tt> otherwise </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_TEST_INTER</tt>.
   */

  public native boolean Test_inter() throws MPIException ;

  /**
   * Create an inter-communicator.
   * <p>
   * <table>
   * <tr><td><tt> local_comm    </tt></td><td> local intra-communicator </tr>
   * <tr><td><tt> local_leader  </tt></td><td> rank of local group leader
   *                                           in <tt>localComm</tt> </tr>
   * <tr><td><tt> remote_leader </tt></td><td> rank of remote group leader
   *                                           in this communictor </tr>
   * <tr><td><tt> tag           </tt></td><td> ``safe'' tag </tr>
   * <tr><td><em> returns:      </em></td><td> new inter-communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_INTERCOMM_CREATE</tt>.
   * <p>
   * (This operation is defined as a method on the ``peer communicator'',
   * making it analogous to a <tt>send</tt> or <tt>recv</tt> communication
   * with the remote group leader.)
   */

  public Intercomm Create_intercomm(Comm local_comm, 
                                    int  local_leader,
	                            int  remote_leader, 
                                    int  tag) throws MPIException {
    return new Intercomm(GetIntercomm(local_comm, local_leader,
                                      remote_leader, tag)) ;
  }

  public native long GetIntercomm(Comm local_comm, 
                                  int  local_leader,
	                          int  remote_leader, 
                                  int  tag) ;


  // Object serialization support
    
  public byte[] Object_Serialize(Object   buf,
                                 int      offset,
                                 int      count, 
                                 Datatype type) throws MPIException {
    if(type.Size() != 0) {
	byte[] byte_buf ; 
	Object buf_els [] = (Object[])buf;
	try {
	  ByteArrayOutputStream o = new ByteArrayOutputStream();
	  ObjectOutputStream out = new ObjectOutputStream(o);
	  int base;
	  for (int i = 0; i < count; i++){
	    base = type.Extent() * i;
	    for (int j = 0 ; j < type.displacements.length ; j++)
	      out.writeObject(buf_els[base + offset + 
				     type.displacements[j]]);     
	  }
	      
	  out.flush();
	  out.close();
	  byte_buf  = o.toByteArray();

	} catch(Exception ex){
	    ex.printStackTrace();
	    byte_buf  = null ;
	}
	return byte_buf ;
    }
    else return new byte[0];
  }
   
  public void Object_Deserialize(Object   buf,
                                 byte[]   byte_buf,
                                 int      offset,
                                 int      count,
                                 Datatype type) throws MPIException {

    if(type.Size() != 0) {
      Object buf_els [] = (Object[])buf;          

      try {      
        ByteArrayInputStream in = new ByteArrayInputStream(byte_buf);   
	ObjectInputStream s = new ObjectInputStream(in);
	int base;
	for (int i = 0; i < count; i++){
	  base = type.Extent() * i;
	  for (int j = 0 ; j < type.displacements.length ; j++)
	    buf_els[base + offset + type.displacements[j]]=s.readObject();
	} 
	s.close(); 
      }catch(Exception ex){ex.printStackTrace();}	
    }  
  }

    
  // Blocking Send and Recv
    
  /**
   * Blocking send operation.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SEND</tt>.
   * <p>
   * The actual argument associated with <tt>buf</tt> must be
   * one-dimensional array.  The value <tt>offset</tt> is a subscript in
   * this array, defining the position of the first item of the message.
   * <p>
   * If the <tt>datatype</tt> argument represents an MPI basic type, its
   * value must agree with the element type of <tt>buf</tt>---either
   * a primitive type or a reference (object) type.  If the
   * <tt>datatype</tt> argument represents an MPI derived type, its
   * <em>base type</em> must agree with the element type of <tt>buf</tt>
   */

  public void Send(Object   buf,
                   int      offset, 
                   int      count, 
                   Datatype type,
                   int      dest,  
                   int      tag) throws MPIException {

    if (type.isObject()){   
      byte[] byte_buf = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;

      send(length_buf, 0, 2, MPI.INT, dest, tag);  // header

      send(byte_buf, 0, byte_buf.length, MPI.BYTE,dest, tag) ;
    }
    else {
      send(buf, offset, count, type, dest, tag);
    }
  }
  
  private native void send(Object   buf, 
                           int      offset, 
                           int      count, 
                           Datatype type, 
                           int      dest, 
                           int      tag);

  /**
   * Blocking receive operation.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in receive buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items in receive
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in receive
   *                                      buffer </tr>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_RECV</tt>.
   * <p>
   * The actual argument associated with <tt>buf</tt> must be
   * one-dimensional array.  The value <tt>offset</tt> is a subscript in
   * this array, defining the position into which the first item of the
   * incoming message will be copied.
   * <p>
   * If the <tt>datatype</tt> argument represents an MPI basic type, its
   * value must agree with the element type of <tt>buf</tt>---either
   * a primitive type or a reference (object) type.  If the
   * <tt>datatype</tt> argument represents an MPI derived type, its
   * <em>base type</em> must agree with the element type of <tt>buf</tt>
   */

  public Status Recv(Object   buf, 
                     int      offset, 
                     int      count, 
                     Datatype type,
                     int      source, 
                     int      tag) throws MPIException {

    if (type.isObject()){
      Status status = new Status();

      int[] length_buf= new int[2];
      Recv(length_buf,0,2, MPI.INT, source, tag, status); 

      byte[] byte_buf = new byte[length_buf[0]];
      Recv(byte_buf,0,length_buf[0], MPI.BYTE, status.source, tag, status);

      Object_Deserialize(buf,byte_buf,offset,length_buf[1],type);

      status.object_count = length_buf[1];
      return status;
    }
    else
      return Recv(buf, offset, count, type, source, tag, new Status());
  }

  private native Status Recv(Object   buf, 
                             int      offset, 
                             int      count, 
                             Datatype type,
                             int      source, 
                             int      tag, 
                             Status   stat);

  // Send-Recv

  /**
   * Execute a blocking send and receive operation.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send
   *                                        buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> dest       </tt></td><td> rank of destination </tr>
   * <tr><td><tt> sendtag    </tt></td><td> send tag </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * <tr><td><tt> source     </tt></td><td> rank of source </tr>
   * <tr><td><tt> recvtag    </tt></td><td> receive tag </tr>
   * <tr><td><em> returns:   </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SENDRECV</tt>.
   * <p>
   * Further comments as for <tt>Send</tt> and <tt>Recv</tt>.
   */

  public Status Sendrecv(Object   sendbuf, 
                         int      sendoffset,
                         int      sendcount, 
                         Datatype sendtype,
                         int      dest, 
                         int      sendtag,
                         Object   recvbuf, 
                         int      recvoffset,
                         int      recvcount, 
                         Datatype recvtype,
                         int      source, 
                         int      recvtag) throws MPIException {

    if(sendtype.isObject() || recvtype.isObject()) {
      Request reqs [] = {Isend(sendbuf, sendoffset, sendcount, sendtype,
                               dest, sendtag),
                         Irecv(recvbuf, recvoffset, recvcount, recvtype,
                               source, recvtag)} ;

      Status stas [] = Request.Waitall(reqs) ;

      return stas [1] ;      
    }
    else  return Sendrecv(sendbuf,   sendoffset,
                          sendcount, sendtype,
                          dest,      sendtag,
                          recvbuf,   recvoffset,
                          recvcount, recvtype, 
                          source,    recvtag,
                          new Status());
  }

  private native Status Sendrecv(Object   sbuf, 
                                 int      soffset, 
                                 int      scount,
                                 Datatype stype, 
                                 int      dest, 
                                 int      stag,
                                 Object   rbuf, 
                                 int      roffset, 
                                 int      rcount, 
                                 Datatype rtype, 
                                 int      source, 
                                 int      rtag,
                                 Status   stat);

  /**
   * Execute a blocking send and receive operation, receiving message
   * into send buffer.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> type     </tt></td><td> datatype of each item in
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> sendtag  </tt></td><td> send tag </tr>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> recvtag  </tt></td><td> receive tag </tr>
   * <tr><td><em> returns: </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SENDRECV_REPLACE</tt>.
   * <p>
   * Further comments as for <tt>Send</tt> and <tt>Recv</tt>.
   */

  public Status Sendrecv_replace(Object   buf, 
                                 int      offset, 
                                 int      count,
                                 Datatype type, 
                                 int      dest, 
                                 int      sendtag,
                                 int      source, 
                                 int      recvtag) throws MPIException {

    if(type.isObject()) {

      // Might as well do this natively, to avoid allocation of one more
      // buffer.

      Status status = new Status() ;

      byte[] sendbytes = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {sendbytes.length, count} ;

      Sendrecv_replace(length_buf, 0, 2, MPI.INT,
                       dest, sendtag, source, recvtag,
                       status) ;

      byte [] recvbytes = new byte [length_buf[0]] ;

      Sendrecv(sendbytes, 0, sendbytes.length, MPI.BYTE, dest, sendtag,
               recvbytes, 0, recvbytes.length, MPI.BYTE, status.source, recvtag,
               status) ;

      Object_Deserialize(buf,recvbytes,offset,length_buf[1],type);

      status.object_count = length_buf[1] ;
      return status;
    }
    else
      return Sendrecv_replace(buf, offset, count, type, 
                              dest, sendtag, source, recvtag, new Status());
  }

  private native Status Sendrecv_replace(Object   buf, 
                                         int      offset, 
                                         int      count,
                                         Datatype type, 
                                         int      dest, 
                                         int      stag,
                                         int      source, 
                                         int      rtag, 
                                         Status   stat);

  // Communication Modes

  /**
   * Send in buffered mode.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_BSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public void Bsend(Object   buf, 
                    int      offset, 
                    int      count, 
                    Datatype type,
                    int      dest, 
                    int      tag) throws MPIException {

    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;
       
      bsend(length_buf, 0, 2, MPI.INT, dest, tag);
      bsend(byte_buf, 0, length_buf[0], MPI.BYTE, dest, tag);
    }
    else bsend(buf, offset, count, type, dest, tag);
  }

  private native void bsend(Object   buf, 
                            int      offset, 
                            int      count, 
                            Datatype type,
                            int      dest, 
                            int      tag) ;

  /**
   * Send in synchronous mode.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public void Ssend(Object   buf, 
                    int      offset, 
                    int      count,
                    Datatype type,
                    int      dest, 
                    int      tag) throws MPIException {

    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;

      send(length_buf, 0, 2, MPI.INT, dest, tag); 
      ssend(byte_buf , 0, byte_buf.length, MPI.BYTE, dest, tag);
    }
    else ssend(buf, offset, count, type, dest,  tag);
  }

  private native void ssend(Object   buf,
                            int      offset, 
                            int      count, 
                            Datatype type,
                            int      dest, 
                            int      tag);

  /**
   * Send in ready mode.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_RSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public void Rsend(Object   buf, 
                    int      offset, 
                    int      count,
                    Datatype type,
                    int      dest, 
                    int      tag) throws MPIException {

    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;
       
      rsend(length_buf, 0, 2, MPI.INT, dest, tag);       
      rsend(byte_buf , 0, byte_buf.length, MPI.BYTE, dest, tag);
    }
    else rsend(buf, offset, count, type, dest, tag);
  }

  private native void rsend(Object   buf,
                            int      offset, 
                            int      count, 
                            Datatype type,
                            int      dest, 
                            int      tag) ;

  // Nonblocking communication 

  /**
   * Start a standard mode, nonblocking send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ISEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Request Isend(Object   buf, 
                       int      offset, 
                       int      count,
                       Datatype type, 
                       int      dest, 
                       int      tag) throws MPIException {
    if (type.isObject()) {
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;
       
      Request hdrReq = Isend(length_buf, 0, 2, MPI.INT, dest, tag,
                             new Request());
      Request req = new Request(hdrReq) ;

      Isend(byte_buf, 0, byte_buf.length, MPI.BYTE, dest, tag, req);

      return req;
    }
    else
      return Isend(buf, offset, count, type, dest, tag, new Request());
  }

  /**
   * Protected member used internally by <tt>Prequest.Start</tt>
   */

  protected native Request Isend(Object   buf, 
                                 int      offset, 
                                 int      count,
                                 Datatype type,
                                 int      dest, 
                                 int      tag,  
                                 Request  req);

  /**
   * Start a buffered mode, nonblocking send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_IBSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Request Ibsend(Object   buf, 
                        int      offset, 
                        int      count,
                        Datatype type, 
                        int      dest, 
                        int      tag) throws MPIException {

    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;
       
      Request hdrReq = Ibsend(length_buf, 0, 2, MPI.INT, dest, tag,
                              new Request());
      Request req = new Request(hdrReq) ;

      Ibsend(byte_buf, 0, byte_buf.length, MPI.BYTE, dest, tag, req);

      return req;     
    }
    else
      return Ibsend(buf, offset, count, type, dest, tag, new Request());
  }

  /**
   * Protected member used internally by <tt>Prequest.Start</tt>
   */

  protected native Request Ibsend(Object   buf, 
                                  int      offset, 
                                  int      count,
                                  Datatype type,
                                  int      dest, 
                                  int      tag, 
                                  Request req);

  /**
   * Start a synchronous mode, nonblocking send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ISSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Request Issend(Object   buf, 
                        int      offset, 
                        int      count,
                        Datatype type,
                        int      dest, 
                        int      tag) throws MPIException {
    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;

      Request hdrReq = Issend(length_buf, 0, 2, MPI.INT, dest, tag,
                              new Request());
      Request req = new Request(hdrReq) ;

      Isend(byte_buf, 0, byte_buf.length, MPI.BYTE, dest, tag, req);

      return req;     
    }
    else
      return Issend(buf, offset, count, type, dest, tag, new Request());
  }

  /**
   * Protected member used internally by <tt>Prequest.Start</tt>
   */

  protected native Request Issend(Object   buf, 
                                  int      offset, 
                                  int      count,
                                  Datatype type,
                                  int      dest, 
                                  int      tag, 
                                  Request  req);

  /**
   * Start a ready mode, nonblocking send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_IRSEND</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Request Irsend(Object   buf, 
                        int      offset, 
                        int      count,
                        Datatype type,
                        int      dest,
                        int      tag) throws MPIException {

    if (type.isObject()){
      byte[] byte_buf  = Object_Serialize(buf,offset,count,type);

      int[] length_buf = {byte_buf.length, count} ;
       
      Request hdrReq = Irsend(length_buf, 0, 2, MPI.INT, dest, tag,
                              new Request());
      Request req = new Request(hdrReq) ;

      Isend(byte_buf, 0, byte_buf.length, MPI.BYTE, dest, tag, req);

      return req;     
    }
    else
      return Irsend(buf, offset, count, type, dest, tag, new Request()); 
  }

  /**
   * Protected member used internally by <tt>Prequest.Start</tt>
   */

  protected native Request Irsend(Object   buf,
                                  int      offset, 
                                  int      count,
                                  Datatype type,
                                  int      dest, 
                                  int      tag,
                                  Request  req);

  /**
   * Start a nonblocking receive.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in receive
   *                                      buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items in receive
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in receive
   *                                      buffer </tr>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_IRECV</tt>.
   * <p>
   * Further comments as for <tt>Recv</tt>.
   */

  public Request Irecv(Object   buf, 
                       int      offset, 
                       int      count,
                       Datatype type,
                       int      source,
                       int      tag) throws MPIException {

    if (type.isObject()){
      int[] length_buf= new int[2];

      Request req = new Request(buf, offset, count, type,
                                tag, this, length_buf) ;

      Irecv(length_buf, 0, 2, MPI.INT, source, tag, req);

      return req;
    }
    else
      return Irecv(buf, offset, count, type, source, tag, new Request());
  }

  /**
   * Protected member used internally by <tt>Prequest.Start</tt>
   */

  protected native Request Irecv(Object   buf, 
                                 int      offset, 
                                 int      count,
                                 Datatype type,
                                 int      source, 
                                 int      tag,
                                 Request  req);


  // Persistent communication  requests

  /**
   * Creates a persistent communication request for a standard mode send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> persistent communication
   *                                      request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SEND_INIT</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Prequest Send_init(Object   buf, 
                            int      offset, 
                            int      count,
                            Datatype type,
                            int      dest, 
                            int      tag) throws MPIException {

    return new Prequest(Prequest.MODE_STANDARD, buf, offset, count, type,
                        dest, tag, this) ;
  }

  /**
   * Creates a persistent communication request for a buffered mode send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> persistent communication
   *                                      request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_BSEND_INIT</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Prequest Bsend_init(Object   buf, 
                             int      offset, 
                             int      count,
                             Datatype type,
                             int      dest, 
                             int      tag) throws MPIException {

    return new Prequest(Prequest.MODE_BUFFERED, buf, offset, count, type,
                        dest, tag, this) ;
  }

  /**
   * Creates a persistent communication request for a synchronous mode send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> persistent communication
   *                                      request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SSEND_INIT</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Prequest Ssend_init(Object   buf, 
                             int      offset, 
                             int      count,
                             Datatype type,
                             int      dest, 
                             int      tag) throws MPIException {

    return new Prequest(Prequest.MODE_SYNCHRONOUS, buf, offset, count, type,
                        dest, tag, this) ;
  }

  /**
   * Creates a persistent communication request for a ready mode send.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> send buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items to send </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in send
   *                                      buffer </tr>
   * <tr><td><tt> dest     </tt></td><td> rank of destination </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> persistent communication
   *                                      request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_RSEND_INIT</tt>.
   * <p>
   * Further comments as for <tt>Send</tt>.
   */

  public Prequest Rsend_init(Object   buf, 
                             int      offset, 
                             int      count,
                             Datatype type,
                             int      dest, 
                             int      tag) throws MPIException {

    return new Prequest(Prequest.MODE_READY, buf, offset, count, type,
                        dest, tag, this) ;
  }


  /**
   * Creates a persistent communication request for a receive operation.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in receive
   *                                      buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items in receive
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in receive
   *                                      buffer </tr>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> communication request </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_RECV_INIT</tt>.
   * <p>
   * Further comments as for <tt>Recv</tt>.
   */

  public Prequest Recv_init(Object   buf, 
                            int      offset, 
                            int      count,
                            Datatype type,
                            int      source, 
                            int      tag) throws MPIException {

    return new Prequest(buf, offset, count, type, source, tag, this) ;
  }


  // Pack and Unpack 

  /**
   * Packs message in send buffer <tt>inbuf</tt> into space specified in
   * <tt>outbuf</tt>.
   * <p>
   * <table>
   * <tr><td><tt> inbuf    </tt></td><td> input buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in input buffer </tr>
   * <tr><td><tt> incount  </tt></td><td> number of items in input
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in input
   *                                      buffer </tr>
   * <tr><td><tt> outbuf   </tt></td><td> output buffer </tr>
   * <tr><td><tt> position </tt></td><td> initial position in output buffer
   *                                      </tr>
   * <tr><td><em> returns: </em></td><td> final position in output buffer
   *                                      </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_PACK</tt>.
   * <p>
   * The return value is the output value of <tt>position</tt> - the
   * inital value incremented by the number of bytes written.
   */

  public int Pack(Object   inbuf, 
                  int      offset, 
                  int      incount,
                  Datatype datatype, 
                  byte[]   outbuf, 
                  int      position) throws MPIException {

    if (datatype.isObject()){
      byte[] byte_buf = Object_Serialize(inbuf,offset,incount,datatype);

      System.arraycopy(byte_buf,0,outbuf,position,byte_buf.length);
  
      return (position + byte_buf.length);

    }
    else
      return pack(inbuf, offset, incount, datatype, outbuf, position);
  }

  private native int pack(Object   inbuf,
                          int      offset,
                          int      incount,
                          Datatype data, 
                          byte[]   outbuf, 
                          int      position);

  /**
   * Unpacks message in receive buffer <tt>outbuf</tt> into space specified in
   * <tt>inbuf</tt>.
   * <p>
   * <table>
   * <tr><td><tt> inbuf    </tt></td><td> input buffer </tr>
   * <tr><td><tt> position </tt></td><td> initial position in input buffer
   *                                      </tr>
   * <tr><td><tt> outbuf   </tt></td><td> output buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in output buffer </tr>
   * <tr><td><tt> outcount </tt></td><td> number of items in output
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in output
   *                                      buffer </tr>
   * <tr><td><em> returns: </em></td><td> final position in input buffer
   *                                      </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_UNPACK</tt>.
   * <p>
   * The return value is the output value of <tt>position</tt> - the
   * inital value incremented by the number of bytes read.
   */

  public int Unpack(byte[]   inbuf, 
                    int      position, 
                    Object   outbuf, 
                    int      offset, 
                    int      outcount, 
                    Datatype datatype) throws MPIException {

    if (datatype.isObject()){
   
      Object buf_els [] = (Object[])outbuf;
      int ava=0;   

      try {      
        ByteArrayInputStream in = 
                       new ByteArrayInputStream(inbuf,position,
                                                inbuf.length-position);
  
        ObjectInputStream s = new ObjectInputStream(in);
            
        int base;
        for (int i = 0; i < outcount; i++){
          base = datatype.Extent() * i;
          for (int j = 0 ; j < datatype.displacements.length ; j++)
             buf_els[base + offset + datatype.displacements[j]]=s.readObject();
        }  
        ava= in.available();
        s.close(); 
      }catch(Exception ex){ex.printStackTrace();}	

      return inbuf.length- ava;
    }
    else
      return unpack(inbuf, position, outbuf, offset, outcount, datatype);
  }

  private native int unpack(byte[]   inbuf,
                            int      position, 
                            Object   outbuf, 
                            int      offset, 
                            int      outcount, 
                            Datatype type);

  /**
   * Returns an upper bound on the increment of <tt>position</tt> effected
   * by <tt>pack</tt>.
   * <p>
   * <table>
   * <tr><td><tt> incount  </tt></td><td> number of items in input
   *                                      buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in input
   *                                      buffer </tr>
   * <tr><td><em> returns: </em></td><td> upper bound on size of packed
   *                                      message </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_PACK_SIZE</tt>.
   * <p>
   * <em>It is an error to call this function if the base type of
   * <tt>datatype</tt> is <tt>MPI.OBJECT</tt></em>.
   */

  public native int Pack_size(int incount, Datatype datatype)
                                                  throws MPIException ;

  // Probe and Cancel

  /**
   * Check if there is an incoming message matching the pattern specified.
   * <p>
   * <table>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> status object or null handle </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_IPROBE</tt>.
   * <p>
   * If such a message is currently available, a status object
   * similar to the return value of a matching <tt>Recv</tt> operation
   * is returned.
   * Otherwise a null handle is returned.
   */

  public Status Iprobe(int source, int tag) throws MPIException {
    return Iprobe(source,tag,new Status());
  }

  private native Status Iprobe(int source, int tag,Status stat)
                                                   throws MPIException ;

  /**
   * Wait until there is an incoming message matching the pattern specified.
   * <p>
   * <table>
   * <tr><td><tt> source   </tt></td><td> rank of source </tr>
   * <tr><td><tt> tag      </tt></td><td> message tag </tr>
   * <tr><td><em> returns: </em></td><td> status object </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_PROBE</tt>.
   * <p>
   * Returns a status object similar to the return value of a matching
   * <tt>Recv</tt> operation.
   */

  public Status Probe(int source, int tag) throws MPIException {
    return Probe(source,tag,new Status());
  }

  private native Status Probe(int source, int tag,Status stat)
                                                   throws MPIException ;

  // Caching

  /**
   * Retrieves attribute value by key.
   * <p>
   * <table>
   * <tr><td><tt> keyval   </tt></td><td> one of the key values predefined
   *                                      by the implementation </tr>
   * <tr><td><em> returns: </em></td><td> attribute value </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ATTR_GET</tt>.
   */

  public native int Attr_get(int keyval) throws MPIException ;

  public native void Attr_delete(int keyval);

    public native void Attr_put(int keyval, int value);

  // Process Topologies

  /**
   * Returns the type of topology associated with the communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> topology type of communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TOPO_TEST</tt>.
   * <p>
   * The return value will be one of <tt>MPI.GRAPH</tt>, <tt>MPI.CART</tt>
   * or <tt>MPI.UNDEFINED</tt>.
   */

  public native int Topo_test() throws MPIException ;

  // Enviromental Management 

  /**
   * Abort MPI.
   * <p>
   * <table>
   * <tr><td><tt> errorcode </tt></td><td> error code for Unix or POSIX
   *              environments </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ABORT</tt>.
   */

  public native void Abort(int errorcode) throws MPIException ;

  // Error handler

  /**
   * Associates a new error handler with communicator at the calling process.
   * <p>
   * <table>
   * <tr><td><tt> errhandler </tt></td><td> new MPI error handler for
   *                                        communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ERRORHANDLER_SET</tt>.
   */

  public native void Errhandler_set(Errhandler errhandler) throws MPIException ;

  /**
   * Returns the error handler currently associated with the communicator.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> MPI error handler currently
   *                                      associated with communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ERRORHANDLER_GET</tt>.
   */

  public Errhandler Errorhandler_get() throws MPIException {
    return new Errhandler(errorhandler_get()) ;
  }

  private native long errorhandler_get();

  protected long handle;

  static {
    init();
  }

  private static native void init();
}

// Things to do:
//
//   Should `Object_Serialize', `Object_Deserialize' really be instance 
//   methods on `Comm'?
//
//   Mystery: Why does `startO' test hang on the `Ssend_init' case if
//   header is sent standard-mode and data is sent synchronous?
//
//   Selection of constructors on the basis of `long' vs `int' argument
//   is bad.  Too easy to get the wrong one.

