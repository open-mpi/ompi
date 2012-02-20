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
 * File         : Op.java
 * Author       : Xinying Li, Sang LIm
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;
//import mpi.*;

public class Op extends Freeable {
  private final static int NULL  = 0;
  private final static int MAX   = 1;
  private final static int MIN   = 2;
  private final static int SUM   = 3;
  private final static int PROD  = 4;
  private final static int LAND  = 5;
  private final static int BAND  = 6;
  private final static int LOR   = 7;
  private final static int BOR   = 8;
  private final static int LXOR  = 9;
  private final static int BXOR  =10;
  private final static int MINLOC=11;
  private final static int MAXLOC=12;

  private static native void init();

  private User_function uf = null ;

  protected Op(int Type) { GetOp(Type);}

  /**
   * Bind a user-defined global reduction operation to an <tt>Op</tt> object.
   * <p>
   * <table>
   * <tr><td><tt> function </tt></td><td> user defined function </tr>
   * <tr><td><tt> commute  </tt></td><td> <tt>true</tt> if commutative,
   *                                      <tt>false</tt> otherwise </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_OP_CREATE</tt>.
   */

  public Op(User_function function, boolean commute) throws MPIException {
    uf = function;
  }
  
  protected boolean isUser() {
    return uf != null ;
  }

  public final void Call(Object invec, int inoffset,
                         Object outvec, int outoffset,
                         int count, Datatype datatype) {
    uf.Call(invec, inoffset, outvec, outoffset, count, datatype);
  }

  private native void GetOp(int Type);

  protected long handle ;

  @SuppressWarnings("unchecked")
  public void finalize() throws MPIException {
      synchronized(MPI.class) {
          MPI.freeList.addFirst(this) ;
      }
  }

  native void free() ;

  static {
    init();
  }
}

