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
 * File         : MPI.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 *                (contributions from MAEDA Atusi)
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.18 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

import java.util.LinkedList ;

public class MPI {

  static int MAX_PROCESSOR_NAME = 256;

  static public Intracomm COMM_WORLD;
  static public Comm COMM_SELF;

  static public int GRAPH, CART;
  static public int ANY_SOURCE, ANY_TAG;

  static public Op MAX, MIN, SUM, PROD, LAND, BAND,
                   LOR, BOR, LXOR, BXOR, MINLOC, MAXLOC;

  static public Datatype BYTE, CHAR, SHORT, BOOLEAN, 
    INT, LONG, FLOAT, DOUBLE, PACKED, LB, UB, OBJECT;
  static public Datatype SHORT2, INT2, LONG2, FLOAT2, DOUBLE2;

  static public Request REQUEST_NULL;
  static public Group GROUP_EMPTY;

  static public int PROC_NULL;
  static public int BSEND_OVERHEAD;
  static public int UNDEFINED;
  static public int IDENT, CONGRUENT, SIMILAR, UNEQUAL;
  static public int TAG_UB, HOST, IO;

  static Errhandler ERRORS_ARE_FATAL, ERRORS_RETURN;

  static {
   
      // System.loadLibrary("savesignals");
            // Actually only needed for JVMs that don't provide
            // JDK 1.4-like signal chaining, but doesn't do any harm.

      //saveSignalHandlers();
      
      System.loadLibrary("mpi_java");
      if (!loadGlobalLibraries()) {
	  System.out.println("JAVA BINDINGS FAILED TO LOAD REQUIRED LIBRARIES");
	  System.exit(1);
      }


      //restoreSignalHandlers();
            // On SP2, JVM signal handlers overridden during loadLibrary().

    try {
      BYTE    = new Datatype();
      CHAR    = new Datatype();
      SHORT   = new Datatype();
      BOOLEAN = new Datatype();
      INT     = new Datatype();
      LONG    = new Datatype();
      FLOAT   = new Datatype();
      DOUBLE  = new Datatype();
      PACKED  = new Datatype();
      LB      = new Datatype();
      UB      = new Datatype();
      OBJECT  = new Datatype();

      SHORT2  = new Datatype() ;
      INT2    = new Datatype() ;
      LONG2   = new Datatype() ;
      FLOAT2  = new Datatype() ;
      DOUBLE2 = new Datatype() ;

      MAX  = new Op(1);
      MIN  = new Op(2);
      SUM  = new Op(3);
      PROD = new Op(4);
      LAND = new Op(5);
      BAND = new Op(6);
      LOR  = new Op(7);
      BOR  = new Op(8);
      LXOR = new Op(9);
      BXOR = new Op(10);

      MINLOC = new Op(new Minloc(), true);
      MAXLOC = new Op(new Maxloc(), true);

      GROUP_EMPTY  = new Group(Group.EMPTY);
      REQUEST_NULL = new Request(Request.NULL);
      
      // Constant
      SetConstant(); 

      ERRORS_ARE_FATAL = new Errhandler(Errhandler.FATAL);
      ERRORS_RETURN    = new Errhandler(Errhandler.RETURN);       

      COMM_WORLD = new Intracomm() ;
    }
    catch (MPIException e) {
      System.out.println(e.getMessage()) ;
      System.exit(1) ;
    }
  }

    static private native boolean loadGlobalLibraries();
    static private native void saveSignalHandlers();
    //    static private native void restoreSignalHandlers(); 

  /**
   * Initialize MPI.
   * <p>
   * <table>
   * <tr><td><tt> args </tt></td><td> arguments to <tt>main</tt> method. </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_INIT</tt>.
   */

  static public String [] Init(String[] args) throws MPIException {

    String [] newArgs = InitNative(args);

    // restoreSignalHandlers();
          // On MPICH, etc, JVM signal handlers overridden during MPI_Init().


    BYTE.setBasic(1);
    CHAR.setBasic(2);
    SHORT.setBasic(3);
    BOOLEAN.setBasic(4);
    INT.setBasic(5);
    LONG.setBasic(6);
    FLOAT.setBasic(7);
    DOUBLE.setBasic(8);
    PACKED.setBasic(9);
    LB.setBasic(10);
    UB.setBasic(11);
    OBJECT.setBasic(12);

    SHORT2.setContiguous(2, MPI.SHORT);
    INT2.setContiguous(2, MPI.INT);
    LONG2.setContiguous(2, MPI.LONG);
    FLOAT2.setContiguous(2, MPI.FLOAT);
    DOUBLE2.setContiguous(2, MPI.DOUBLE);

    SHORT2.Commit();
    INT2.Commit();
    LONG2.Commit();
    FLOAT2.Commit();
    DOUBLE2.Commit();

    COMM_WORLD.setType(Intracomm.WORLD);

    return newArgs ;
  }

  static private native String [] InitNative(String[] args);
  static private native void SetConstant();

  /**
   * Finalize MPI.
   * <p>
   * Java binding of the MPI operation <tt>MPI_FINALIZE</tt>.
   */

  static public native void Finalize() throws MPIException ;

  /**
   * Returns wallclock time.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> elapsed wallclock time in seconds
   *                                      since some time in the past </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WTIME</tt>.
   */

  static public native double Wtime();

  /**
   * Returns resolution of timer.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> resolution of <tt>wtime</tt> in
   *                                      seconds. </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_WTICK</tt>.
   */

  static public native double Wtick();

  /**
   * Returns the name of the processor on which it is called.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> A unique specifier for the actual
   *                                      node. </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GET_PROCESSOR_NAME</tt>.
   */

  static public String Get_processor_name() throws MPIException {
    byte[] buf = new byte[MAX_PROCESSOR_NAME] ;
    int lengh = Get_processor_name(buf) ;
    return new String(buf,0,lengh) ;
  }

  static private native int Get_processor_name(byte[] buf) ;

  /**
   * Test if MPI has been initialized.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> <tt>true</tt> if <tt>Init</tt> has
   *                                      been called, <tt>false</tt>
   *                                      otherwise. </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_INITIALIZED</tt>.
   */

  static public native boolean Initialized() throws MPIException ;

  // Buffer allocation

  private static byte [] buffer = null ;

  /**
   * Provides to MPI a buffer in user's memory to be used for buffering
   * outgoing messages.
   * Java binding of the MPI operation <tt>MPI_BUFFER_ATTACH</tt>.
   */

  static public void Buffer_attach(byte[] buffer) throws MPIException {
    MPI.buffer = buffer ;
    Buffer_attach_native(buffer);
  }

  static private native void Buffer_attach_native(byte[] buffer);


  /**
   * Detach the buffer currently associated with MPI.
   * Java binding of the MPI operation <tt>MPI_BUFFER_DETACH</tt>.
   */

  static public byte[] Buffer_detach() throws MPIException {
    Buffer_detach_native(buffer);
    byte [] result = MPI.buffer ;
    MPI.buffer = null ;
    return result ;
  }

  static private native void Buffer_detach_native(byte[] buffer);

  static LinkedList freeList = new LinkedList() ;

  synchronized static void clearFreeList() {

      while(!freeList.isEmpty())
	  ((Freeable) freeList.removeFirst()).free() ;
  }
}


// Minloc and Maxloc
// BC Note: moved to separate source files
/*
class Maxloc extends User_function{
  public void Call(Object invec, int inoffset, Object outvec, int outoffset,
                   int count, Datatype datatype){

    // *** should work also for derived datatypes with following as
    //     as bases ? ***

    if(datatype == MPI.SHORT2) {
      short [] in_array = (short[])invec;
      short [] out_array = (short[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2) {  
        short inval  = in_array  [indisp] ;
        short outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          short inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.INT2) {
      int [] in_array = (int[])invec;
      int [] out_array = (int[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        int inval  = in_array  [indisp] ;
        int outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          int inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.LONG2) {
      long [] in_array = (long[])invec;
      long [] out_array = (long[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        long inval  = in_array  [indisp] ;
        long outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          long inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.FLOAT2) {
      float [] in_array = (float[])invec;
      float [] out_array = (float[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        float inval  = in_array  [indisp] ;
        float outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          float inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.DOUBLE2) {
      double [] in_array = (double[])invec;
      double [] out_array = (double[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        double inval  = in_array  [indisp] ;
        double outval = out_array [outdisp] ;

        if(inval > outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          double inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else {
      System.out.println("MPI.MAXLOC: invalid datatype") ;
      try {
        MPI.COMM_WORLD.Abort(1);
      }
      catch(MPIException e) {}
    }
  }
}

class Minloc extends User_function{
  public void Call(Object invec, int inoffset, Object outvec, int outoffset,
                   int count, Datatype datatype){
    if(datatype == MPI.SHORT2) {
      short [] in_array = (short[])invec;
      short [] out_array = (short[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        short inval  = in_array  [indisp] ;
        short outval = out_array [outdisp] ;

        if(inval < outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          short inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.INT2) {
      int [] in_array = (int[])invec;
      int [] out_array = (int[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        int inval  = in_array  [indisp] ;
        int outval = out_array [outdisp] ;

        if(inval < outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          int inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.LONG2) {
      long [] in_array = (long[])invec;
      long [] out_array = (long[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        long inval  = in_array  [indisp] ;
        long outval = out_array [outdisp] ;

        if(inval < outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          long inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.FLOAT2) {
      float [] in_array = (float[])invec;
      float [] out_array = (float[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        float inval  = in_array  [indisp] ;
        float outval = out_array [outdisp] ;

        if(inval < outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          float inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else if(datatype == MPI.DOUBLE2) {
      double [] in_array = (double[])invec;
      double [] out_array = (double[])outvec;

      int indisp  = inoffset ;
      int outdisp = outoffset ;
      for (int i = 0; i < count; i++, indisp += 2, outdisp += 2){
        
        double inval  = in_array  [indisp] ;
        double outval = out_array [outdisp] ;

        if(inval < outval) {
          out_array [outdisp    ] = inval ;
          out_array [outdisp + 1] = in_array [outdisp + 1] ;
        }
        else if(inval == outval) {
          double inloc = in_array [indisp + 1] ;

          if(inloc < out_array [outdisp + 1])
            out_array [outdisp + 1] = inloc ;
        }
      }
    }
    else {
      System.out.println("MPI.MINLOC: invalid datatype") ;
      try {
        MPI.COMM_WORLD.Abort(1);
      }
      catch(MPIException e) {}
    }
  }
}
*/

// Things to do:
//
//   Check if `Maxloc'/`Minloc' should work with derived types.

