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
 * File         : Intracommm.java
 * Author       : Sang Lim, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.14 $
 * Updated      : $Date: 2002/12/16 15:25:13 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

public class Intracomm extends Comm {

  Intracomm() {}

  void setType(int type) {
    super.setType(type) ;

    shadow = new Comm(dup()) ;
  }

  protected Intracomm(long handle) throws MPIException {
    super(handle) ;

    shadow = new Comm(dup()) ;
  }

  public Object clone() {
    try {
      return new Intracomm(dup()) ;
    }
    catch (MPIException e) {
      throw new RuntimeException(e.getMessage()) ;
    }
  }

  /**
   * Partition the group associated with this communicator and create
   * a new communicator within each subgroup.
   * <p>
   * <table>
   * <tr><td><tt> color    </tt></td><td> control of subset assignment </tr>
   * <tr><td><tt> key      </tt></td><td> control of rank assignment </tr>
   * <tr><td><em> returns: </em></td><td> new communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_SPLIT</tt>.
   */

  public Intracomm Split(int colour, int key) throws MPIException {
    long splitHandle = split(colour,key) ;
    if(splitHandle == nullHandle)
      return null ;
    else
      return new Intracomm(splitHandle) ;
  }

  private native long split(int colour, int key);

  /**
   * Create a new communicator.
   * <p>
   * <table>
   * <tr><td><tt> group    </tt></td><td> group which is a subset of the
   *                                      group of this communicator </tr>
   * <tr><td><em> returns: </em></td><td> new communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_COMM_CREATE</tt>.
   */

  public Intracomm Creat(Group group) throws MPIException {
    long creatHandle = creat(group) ;
    if(creatHandle == nullHandle)
      return null ;
    else
      return new Intracomm(creatHandle) ;
  }

  private native long creat(Group group);

  // Collective Communication

  /**
   * A call to <tt>Barrier</tt> blocks the caller until all process
   * in the group have called it.
   * <p>
   * Java binding of the MPI operation <tt>MPI_BARRIER</tt>.
   */

  public native void Barrier() throws MPIException ;

  /*
   * The type signature of `incount * intype' must be equal to the type
   * signature of `outcount * outtype' (ie they must represent the same
   * number of basic elements of the same type).
   */

  private void copyBuffer(Object inbuf,
                          int inoffset, int incount, Datatype intype,
                          Object outbuf,
                          int outoffset, int outcount, Datatype outtype)
                                                      throws MPIException {
    if(intype.isObject()) {
      Object [] inbufArray = (Object[])inbuf;
      Object [] outbufArray = (Object[])outbuf;

      int outbase = outoffset, inbase = inoffset ;
      int kout = 0 ;
      for (int j = 0 ; j < incount ; j++) {
        for (int k = 0 ; k < intype.displacements.length ; k++)
          outbufArray [outbase + outtype.displacements [kout]] = 
               inbufArray [inbase + intype.displacements [k]] ;

        inbase += intype.Extent() ;

        kout++;
        if (kout == outtype.displacements.length){
          kout = 0;
          outbase += outtype.Extent() ;
        }
      }
    }
    else {
      byte [] tmpbuf = new byte [Pack_size(incount, intype)] ;
      Pack(inbuf, inoffset, incount, intype, tmpbuf, 0) ;
      Unpack(tmpbuf, 0, outbuf, outoffset, outcount, outtype) ;
    }
  }

  private Object newBuffer(Object template) {
    if(template instanceof Object[])
      return new Object [((Object[]) template).length] ;

    if(template instanceof byte[])
      return new byte [((byte[]) template).length] ;

    if(template instanceof char[])
      return new char [((char[]) template).length] ;

    if(template instanceof short[])
      return new short [((short[]) template).length] ;

    if(template instanceof boolean[])
      return new boolean [((boolean[]) template).length] ;

    if(template instanceof int[])
      return new int [((int[]) template).length] ;

    if(template instanceof long[])
      return new long [((long[]) template).length] ;

    if(template instanceof float[])
      return new float [((float[]) template).length] ;

    if(template instanceof double[])
      return new double [((double[]) template).length] ;

    return null ;
  }

  /**
   * Broadcast a message from the process with rank <tt>root</tt>
   * to all processes of the group.
   * <p>
   * <table>
   * <tr><td><tt> buf      </tt></td><td> buffer array </tr>
   * <tr><td><tt> offset   </tt></td><td> initial offset in buffer </tr>
   * <tr><td><tt> count    </tt></td><td> number of items in buffer </tr>
   * <tr><td><tt> datatype </tt></td><td> datatype of each item in
   *                                      buffer </tr>
   * <tr><td><tt> root     </tt></td><td> rank of broadcast root </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_BCST</tt>.
   */

  public void Bcast(Object   buf, 
                    int      offset, 
                    int      count, 
                    Datatype type,
                    int      root) throws MPIException {

    if (type.isObject()){
      if (Rank() == root){
        for (int dst = 0; dst < Size(); dst++)
          if (dst != root)
            shadow.Send(buf, offset, count, type, dst, 0);
      }
      else
        shadow.Recv(buf, offset, count, type, root, 0);
    }
    else
       bcast(buf, offset*type.Size(), count, type, root);   
  }

  private native void bcast(Object   buf, 
                            int      offset, 
                            int      count, 
                            Datatype type,
                            int      root);

  /**
   * Each process sends the contents of its send buffer to the
   * root process.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items to receive </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * <tr><td><tt> root       </tt></td><td> rank of receiving process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GATHER</tt>.
   */
  
  public void Gather(Object   sendbuf, 
                     int      sendoffset, 
                     int      sendcount,
                     Datatype sendtype, 
                     Object   recvbuf, 
                     int      recvoffset, 
                     int      recvcount,
                     Datatype recvtype, 
                     int      root) throws MPIException {
  
    if (sendtype.isObject()) {
      if (Rank() == root) {
        for (int src = 0; src < Size(); src++) {
          int dstOffset = recvoffset + recvcount * recvtype.Extent() * src ;
          if (src == root)
            copyBuffer(sendbuf, sendoffset, sendcount, sendtype,
                       recvbuf, dstOffset, recvcount, recvtype) ;
          else
            shadow.Recv(recvbuf, dstOffset, recvcount, recvtype, src, 0);
	}
      }
      else
        shadow.Send(sendbuf, sendoffset, sendcount, sendtype, root, 0);
    }
    else
      gather(sendbuf, sendoffset*sendtype.Size(), sendcount,sendtype,
	     recvbuf, recvoffset*recvtype.Size(), recvcount,recvtype,
             root);
  }

  private native void gather(Object   sendbuf, 
                             int      sendoffset, 
                             int      sendcount,
	                     Datatype sendtype, 
                             Object   recvbuf, 
                             int      recvoffset, 
                             int      recvcount,
	                     Datatype recvtype, 
                             int      root);

  /**
   * Extends functionality of <tt>Gather</tt> by allowing varying
   * counts of data from each process.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcounts </tt></td><td> number of elements received from
   *                                        each process </tr>
   * <tr><td><tt> displs     </tt></td><td> displacements at which to place
   *                                        incoming data </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * <tr><td><tt> root       </tt></td><td> rank of receiving process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GATHERV</tt>.
   * <p>
   * The sizes of arrays <tt>recvcounts</tt> and <tt>displs</tt> should be the
   * size of the group.  Entry <em>i</em> of <tt>displs</tt> specifies the
   * displacement relative to element <tt>recvoffset</tt> of <tt>recvbuf</tt>
   * at which to place incoming data.
   */

  public void Gatherv(Object   sendbuf,  
                      int      sendoffset, 
                      int      sendcount,
                      Datatype sendtype, 
                      Object   recvbuf, 
                      int      recvoffset, 
                      int []   recvcount,
                      int []   displs, 
                      Datatype recvtype, 
                      int      root) throws MPIException {

    if (sendtype.isObject()){
      if (Rank() == root){
        for (int src = 0; src < Size(); src++){
          int dstOffset = recvoffset + sendtype.Extent() * displs[src] ;
          if (src == root)
            copyBuffer(sendbuf, sendoffset, sendcount,sendtype,
                       recvbuf, dstOffset, recvcount[src], recvtype);
          else
            shadow.Recv(recvbuf, dstOffset, recvcount[src], recvtype, src, 0);
	}
      }
      else
        shadow.Send(sendbuf, sendoffset, sendcount, sendtype, root, 0);
    }
    else
      gatherv(sendbuf  , sendoffset*sendtype.Size(), 
              sendcount, sendtype,
              recvbuf  , recvoffset*recvtype.Size(),
              recvcount, displs,
              recvtype , root);
  }

  private native void gatherv(Object   sendbuf, 
                              int      sendoffset, 
                              int      sendcount,
  	                      Datatype sendtype, 
                              Object   recvbuf, 
                              int      recvoffset, 
                              int []   recvcount,
 	                      int []   displs, 
                              Datatype recvtype, 
                              int      root);

  /**
   * Inverse of the operation <tt>Gather</tt>.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items to receive </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * <tr><td><tt> root       </tt></td><td> rank of sending process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SCATTER</tt>.
   */

  public void Scatter(Object   sendbuf, 
                      int      sendoffset, 
                      int      sendcount,
                      Datatype sendtype, 
                      Object   recvbuf, 
                      int      recvoffset, 
                      int      recvcount,
                      Datatype recvtype, 
                      int      root) throws MPIException {

    if (sendtype.isObject()){
      if (Rank() == root){
        for (int dst = 0; dst < Size() ; dst++){
          int srcOffset = sendoffset + sendcount * sendtype.Extent() * dst ;
          if (dst == root)
            copyBuffer(sendbuf, srcOffset, sendcount, sendtype,
                       recvbuf, recvoffset, recvcount, recvtype);
          else 
            shadow.Send(sendbuf, srcOffset, sendcount, sendtype, dst, 0);
        }
      }
      else 
        shadow.Recv(recvbuf, recvoffset, recvcount, recvtype, root, 0); 
    }
    else 
      scatter(sendbuf, sendoffset*sendtype.Size(), sendcount, sendtype, 
              recvbuf, recvoffset*recvtype.Size(), recvcount, recvtype, 
              root);
  }

  private native void scatter(Object   sendbuf, 
                              int      sendoffset, 
                              int      sendcount,
                              Datatype sendtype, 
                              Object   recvbuf, 
                              int      recvoffset, 
                              int      recvcount,
                              Datatype recvtype, 
                              int      root);

  /**
   * Inverse of the operation <tt>Gatherv</tt>.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcounts </tt></td><td> number of items sent to each
   *                                        process </tr>
   * <tr><td><tt> displs     </tt></td><td> displacements from which to take
   *                                        outgoing data </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items to receive </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * <tr><td><tt> root       </tt></td><td> rank of sending process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SCATTERV</tt>.
   */

  public void Scatterv(Object   sendbuf, 
                       int      sendoffset,
                       int []   sendcount,
                       int []   displs,
                       Datatype sendtype, 
                       Object   recvbuf, 
                       int      recvoffset, 
                       int      recvcount,
                       Datatype recvtype, 
                       int      root) throws MPIException {
  
    if (sendtype.isObject()){
      if (Rank() == root){
        for (int dst = 0 ; dst < Size() ; dst++){
          int srcOffset = sendoffset + sendtype.Extent() * displs[dst] ;
          if (dst == root)
            copyBuffer(sendbuf, srcOffset, sendcount[dst], sendtype,
                       recvbuf, recvoffset, recvcount, recvtype);
          else 
            shadow.Send(sendbuf, srcOffset, sendcount[dst], sendtype, dst, 0);
        }
      }
      else 
        shadow.Recv(recvbuf, recvoffset, recvcount, recvtype, root, 0); 
    }
    else 
      scatterv(sendbuf, sendoffset * sendtype.Size(), sendcount,
               displs, sendtype,
               recvbuf, recvoffset * recvtype.Size(), recvcount, recvtype,
               root);
  }

  private native void scatterv(Object   sendbuf, 
                               int      sendoffset, 
          	               int []   sendcount,
                               int []   displs,
                               Datatype sendtype, 
                               Object   recvbuf, 
                               int      recvoffset, 
                               int      recvcount,
                               Datatype recvtype, 
                               int      root);

  /**
   * Similar to <tt>Gather</tt>, but all processes receive the result.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items to receive </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ALLGATHER</tt>.
   */

  public void Allgather(Object   sendbuf,
			int      sendoffset,
                        int      sendcount,
                        Datatype sendtype, 
                        Object   recvbuf,
			int      recvoffset,
                        int      recvcount,
                        Datatype recvtype) throws MPIException {
 
    if (sendtype.isObject()){
	Gather(sendbuf, sendoffset, sendcount, sendtype, 
               recvbuf, recvoffset, recvcount, recvtype, 0);
	Bcast(recvbuf, recvoffset, Size() * recvcount, recvtype, 0);
    }
    else 
	allgather(sendbuf, sendoffset*sendtype.Size(),
		  sendcount, sendtype, 
		  recvbuf, recvoffset*recvtype.Size(),
		  recvcount, recvtype);
  }

  private native void allgather(Object   sendbuf,
				int      sendoffset,
                                int      sendcount,
                                Datatype sendtype, 
                                Object   recvbuf,
				int      recvoffset,
                                int      recvcount,
                                Datatype recvtype);


  /**
   * Similar to <tt>Gatherv</tt>, but all processes receive the result.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items to send </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcounts </tt></td><td> number of elements received from
   *                                        each process </tr>
   * <tr><td><tt> displs     </tt></td><td> displacements at which to place
   *                                        incoming data </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ALLGATHERV</tt>.
   */

  public void Allgatherv(Object   sendbuf,
			 int      sendoffset,
                         int      sendcount,
                         Datatype sendtype, 
                         Object   recvbuf,
			 int      recvoffset,
                         int []   recvcount,
                         int []   displs, 
                         Datatype recvtype) throws MPIException {

    if (sendtype.isObject()){
	Gatherv(sendbuf, sendoffset, sendcount, sendtype, 
		recvbuf, recvoffset, recvcount, displs, recvtype, 0);

      for (int src = 0; src < Size(); src++){
        int dstOffset = recvoffset + sendtype.Extent() * displs[src] ;
	Bcast(recvbuf, dstOffset, recvcount[src], recvtype, 0);
      }
    }
    else 
      allgatherv(sendbuf  , sendoffset*sendtype.Size(), 
                 sendcount, sendtype, 
                 recvbuf  , recvoffset*recvtype.Size(), 
                 recvcount, displs, 
                 recvtype);
  }

  private native void allgatherv(Object   sendbuf,
				 int      sendoffset,
                                 int      sendcount,
                                 Datatype sendtype,
                                 Object   recvbuf,
				 int      recvoffset,
                                 int []   recvcount,
                                 int []   displs, 
                                 Datatype recvtype);

  /**
   * Extension of <tt>Allgather</tt> to the case where each process sends
   * distinct data to each of the receivers.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcount  </tt></td><td> number of items sent to each
   *                                        process </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype send buffer items </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcount  </tt></td><td> number of items received from any
   *                                        process
   * <tr><td><tt> recvtype   </tt></td><td> datatype of receive buffer
   *                                        items </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ALLTOALL</tt>.
   */

  public void Alltoall(Object   sendbuf, 
                       int      sendoffset, 
                       int      sendcount,
                       Datatype sendtype, 
                       Object   recvbuf, 
                       int      recvoffset, 
                       int      recvcount,
                       Datatype recvtype) throws MPIException {

    if (sendtype.isObject())
      for (int dst = 0; dst < Size(); dst++) {
        int srcOffset = sendoffset + sendcount * sendtype.Extent() * dst ;
        Gather(sendbuf, srcOffset, sendcount, sendtype, 
               recvbuf, recvoffset, recvcount, recvtype, dst);
      }
    else 
      alltoall(sendbuf,   sendoffset*sendtype.Size(), 
               sendcount, sendtype, 
               recvbuf,   recvoffset*recvtype.Size(), 
               recvcount, recvtype);
  }

  private native void alltoall(Object   sendbuf, 
                               int      sendoffset, 
                               int      sendcount,
                               Datatype sendtype, 
                               Object   recvbuf, 
                               int      recvoffset, 
                               int      recvcount,
                               Datatype recvtype);

  /**
   * Adds flexibility to <tt>Alltoall</tt>: location of data for send is
   * specified by <tt>sdispls</tt> and location to place data on receive
   * side is specified by <tt>rdispls</tt>.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> sendcounts </tt></td><td> number of items sent to each
   *                                        process </tr>
   * <tr><td><tt> sdispls    </tt></td><td> displacements from which to take
   *                                        outgoing data </tr>
   * <tr><td><tt> sendtype   </tt></td><td> datatype send buffer items </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcounts </tt></td><td> number of elements received from
   *                                        each process
   * <tr><td><tt> rdispls    </tt></td><td> displacements at which to place
   *                                        incoming data </tr>
   * <tr><td><tt> recvtype   </tt></td><td> datatype of each item in receive
   *                                        buffer </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ALLTOALLV</tt>.
   */

  public void Alltoallv(Object   sendbuf, 
                        int      sendoffset, 
                        int []   sendcount,
                        int []   sdispls,
                        Datatype sendtype, 
                        Object   recvbuf, 
                        int      recvoffset, 
                        int []   recvcount,
                        int []   rdispls, 
                        Datatype recvtype) throws MPIException {

    if (sendtype.isObject())
      for (int dst = 0; dst < Size(); dst++) {
        int srcOffset = sendoffset + sendtype.Extent() * sdispls[dst] ;
        Gatherv(sendbuf, srcOffset, sendcount[dst], sendtype, 
                recvbuf, recvoffset, recvcount, rdispls, recvtype, dst);
      }
    else 
      alltoallv(sendbuf,   sendoffset*sendtype.Size(), 
                sendcount, sdispls, sendtype, 
                recvbuf,   recvoffset*recvtype.Size(), 
                recvcount, rdispls, recvtype);
  }

  private native void alltoallv(Object   sendbuf, 
                                int      sendoffset, 
                                int []   sendcount,
           	                int []   sdispls,
                                Datatype sendtype, 
                                Object   recvbuf, 
                                int      recvoffset, 
                                int []   recvcount,
                                int []   displs, 
                                Datatype recvtype);

  /**
   * Combine elements in input buffer of each process using the reduce
   * operation, and return the combined value in the output buffer of the
   * root process.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> count      </tt></td><td> number of items in send buffer </tr>
   * <tr><td><tt> datatype   </tt></td><td> data type of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> op         </tt></td><td> reduce operation </tr>
   * <tr><td><tt> root       </tt></td><td> rank of root process </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_REDUCE</tt>.
   * <p>
   * The predefined operations are available in Java as <tt>MPI.MAX</tt>,
   * <tt>MPI.MIN</tt>, <tt>MPI.SUM</tt>, <tt>MPI.PROD</tt>, <tt>MPI.LAND</tt>,
   * <tt>MPI.BAND</tt>, <tt>MPI.LOR</tt>, <tt>MPI.BOR</tt>, <tt>MPI.LXOR</tt>,
   * <tt>MPI.BXOR</tt>, <tt>MPI.MINLOC</tt> and <tt>MPI.MAXLOC</tt>.
   */
  
  public void Reduce(Object sendbuf, int sendoffset,
                     Object recvbuf, int recvoffset, int count,
                     Datatype datatype, Op op, int root) throws MPIException {

    if (op.isUser()) {
      if (Rank() == root) {
        copyBuffer(sendbuf,sendoffset,count,datatype,
                   recvbuf,recvoffset,count,datatype);

        Object tempbuf = newBuffer(recvbuf) ;
        for (int src = 0; src < Size(); src++)
          if(src != root) {
            shadow.Recv(tempbuf, 0, count, datatype, src, 0);
            op.Call(tempbuf, 0, recvbuf, recvoffset, count, datatype);
          }
      }
      else
        shadow.Send(sendbuf, sendoffset, count, datatype, root, 0);
    }
    else
      reduce(sendbuf, sendoffset, recvbuf, recvoffset, count,
             datatype, op, root) ;
  }

  private native void reduce(Object sendbuf, int sendoffset, 
                             Object recvbuf, int recvoffset, int count,
                             Datatype datatype, Op op, int root);

  /**
   * Same as <tt>reduce</tt> except that the result appears in receive
   * buffer of all process in the group.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> count      </tt></td><td> number of items in send buffer </tr>
   * <tr><td><tt> datatype   </tt></td><td> data type of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> op         </tt></td><td> reduce operation </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_ALLREDUCE</tt>.
   */

  public void Allreduce(Object sendbuf, int sendoffset,
                        Object recvbuf, int recvoffset, int count,
                        Datatype datatype, Op op) throws MPIException {

    if (op.isUser()){
      Reduce(sendbuf, sendoffset,
             recvbuf, recvoffset, count, datatype, op, 0);

      Bcast(recvbuf, recvoffset, count, datatype, 0);
    }
    else {
      allreduce(sendbuf, sendoffset, recvbuf, recvoffset, count,
                datatype, op) ;
    }
  }

  private native void allreduce(Object sendbuf, int sendoffset, 
                                Object recvbuf, int recvoffset, int count,
                                Datatype datatype, Op op) ;

  /**
   * Combine elements in input buffer of each process using the reduce
   * operation, and scatter the combined values over the output buffers
   * of the processes.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> recvcounts </tt></td><td> numbers of result elements
   *                                        distributed to each process </tr>
   * <tr><td><tt> datatype   </tt></td><td> data type of each item in send
   *                                        buffer </tr>
   * <tr><td><tt> op         </tt></td><td> reduce operation </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_REDUCE_SCATTER</tt>.
   */

  public void Reduce_scatter(Object sendbuf, int sendoffset,
                             Object recvbuf, int recvoffset, int [] recvcounts,
                             Datatype datatype, Op op) throws MPIException {

    if (op.isUser()) {
      int [] displs = new int [recvcounts.length] ;
      int count = 0 ;
      for (int i = 0; i < recvcounts.length; i++) {
        displs [i] = count ;
        count += recvcounts [i] ;
      }

      Object tempbuf = newBuffer(sendbuf) ;
      copyBuffer(sendbuf,sendoffset,count,datatype,
                 tempbuf,sendoffset,count,datatype);

      Reduce(tempbuf, sendoffset, sendbuf, sendoffset, count,
             datatype, op, 0);
      
      Scatterv(tempbuf, sendoffset, recvcounts, displs, datatype, 
               recvbuf, recvoffset, recvcounts[Rank()], datatype, 0);
    }
    else 
      reduce_scatter(sendbuf, sendoffset, recvbuf, recvoffset, recvcounts, 
	             datatype, op) ;
  }

  private native void reduce_scatter(Object sendbuf, int sendoffset, 
                                     Object recvbuf, int recvoffset,
                                     int [] recvcounts, 
	                             Datatype datatype, Op op) ;

  /**
   * Combine elements in input buffer of each process using the reduce
   * operation, and scatter the combined values over the output buffers
   * of the processes.
   * <p>
   * <table>
   * <tr><td><tt> inbuf      </tt></td><td> input buffer array </tr>
   * <tr><td><tt> inoutbuf   </tt></td><td> input buffer array, will contain combined output </tr>
   * <tr><td><tt> count      </tt></td><td> number of elements </tr>
   * <tr><td><tt> datatype   </tt></td><td> data type of each item </tr>
   * <tr><td><tt> op         </tt></td><td> reduce operation </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_REDUCE_LOCAL</tt>.
   */
  public void Reduce_local(Object inbuf, Object inoutbuf, int count,
                             Datatype datatype, Op op) throws MPIException {

      reduce_local(inbuf, inoutbuf, count, datatype, op) ;
  }
  private native void reduce_local(Object inbuf, Object inoutbuf, int count,
				   Datatype datatype, Op op);

  /**
   * Perform a prefix reduction on data distributed across the group.
   * <p>
   * <table>
   * <tr><td><tt> sendbuf    </tt></td><td> send buffer array </tr>
   * <tr><td><tt> sendoffset </tt></td><td> initial offset in send buffer </tr>
   * <tr><td><tt> recvbuf    </tt></td><td> receive buffer array </tr>
   * <tr><td><tt> recvoffset </tt></td><td> initial offset in receive
   *                                        buffer </tr>
   * <tr><td><tt> count      </tt></td><td> number of items in input
   *                                        buffer </tr>
   * <tr><td><tt> datatype   </tt></td><td> data type of each item in input
   *                                        buffer </tr>
   * <tr><td><tt> op         </tt></td><td> reduce operation </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_SCAN</tt>.
   */

  public void Scan(Object sendbuf, int sendoffset,
                   Object recvbuf, int recvoffset, int count,
                   Datatype datatype, Op op) throws MPIException {

    if (op.isUser()){
      if (Rank() == 0)
        copyBuffer(sendbuf,sendoffset,count,datatype,
                        recvbuf,recvoffset,count,datatype);
      else{
        shadow.Recv(recvbuf, recvoffset, count, datatype, Rank() - 1, 0);

        op.Call(sendbuf, sendoffset, recvbuf, recvoffset, count, datatype);
      }
      if (Rank() < Size() - 1) 
        shadow.Send(recvbuf, recvoffset, count, datatype, Rank() + 1, 0);
    }
    else
      scan(sendbuf, sendoffset, recvbuf, recvoffset, count, datatype, op);
  }

  private native void scan(Object sendbuf, int sendoffset, 
                           Object recvbuf, int recvoffset, int count,
                           Datatype datatype, Op op) ;

  // Topology Constructors

  /**
   * Create a Cartesian topology communicator whose group is a subset
   * of the group of this communicator.
   * <p>
   * <table>
   * <tr><td><tt> dims     </tt></td><td> the number of processes in each
   *                                      dimension </tr>
   * <tr><td><tt> periods  </tt></td><td> <tt>true</tt> if grid is periodic,
   *                                      <tt>false</tt> if not, in each
   *                                      dimension </tr>
   * <tr><td><tt> reorder  </tt></td><td> <tt>true</tt> if ranking may be
   *                                      reordered, <tt>false</tt> if not </tr>
   * <tr><td><em> returns: </em></td><td> new Cartesian topology
   *                                      communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_CART_CREATE</tt>.
   * <p>
   * The number of dimensions of the Cartesian grid is taken to be the size
   * of the <tt>dims</tt> argument.  The array <tt>periods</tt> must be the
   * same size.
   */

  public Cartcomm Create_cart(int [] dims, boolean [] periods, 
	                      boolean reorder) throws MPIException {
    long cartHandle = GetCart(dims, periods, reorder) ;
    if(cartHandle == nullHandle)
      return null ;
    else
      return new Cartcomm(cartHandle) ;
  }

  private native long GetCart(int [] dims, boolean [] periods,
                              boolean reorder) ;

  /**
   * Create a graph topology communicator whose group is a subset
   * of the group of this communicator.
   * <p>
   * <table>
   * <tr><td><tt> index    </tt></td><td> node degrees </tr>
   * <tr><td><tt> edges    </tt></td><td> graph edges </tr>
   * <tr><td><tt> reorder  </tt></td><td> <tt>true</tt> if ranking may be
   *                                      reordered, <tt>false</tt> if not </tr>
   * <tr><td><em> returns: </em></td><td> new graph topology communicator </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GRAPH_CREATE</tt>.
   * <p>
   * The number of nodes in the graph, <em>nnodes</em>, is taken
   * to be size of the <tt>index</tt> argument.  The size of array
   * <tt>edges</tt> must be <tt>index [nnodes} - 1]</tt>.
   */

  public Graphcomm Create_graph(int [] index, int [] edges, boolean reorder)
                                                         throws MPIException {
    long graphHandle = GetGraph(index,edges,reorder) ;
    if(graphHandle == nullHandle)
      return null ;
    else
      return new Graphcomm(graphHandle) ;
  }

  private native long GetGraph(int [] index,int [] edges, boolean reorder);

  private Comm shadow ;  // Used by non-native collectives.
}


// Things to do
//

