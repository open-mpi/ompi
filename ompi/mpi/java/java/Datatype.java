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
 * File         : Datatype.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.14 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

public class Datatype extends Freeable {

  private final static int UNDEFINED = -1 ;
  private final static int NULL      =  0 ;
  private final static int BYTE      =  1 ;
  private final static int CHAR      =  2 ;
  private final static int SHORT     =  3 ;
  private final static int BOOLEAN   =  4 ;
  private final static int INT       =  5 ;
  private final static int LONG      =  6 ;
  private final static int FLOAT     =  7 ;
  private final static int DOUBLE    =  8 ;  
  private final static int PACKED    =  9 ;
  private final static int LB        = 10 ;
  private final static int UB        = 11 ;
  private final static int OBJECT    = 12 ;

  private static native void init();


  /*
   * Constructor used in static initializer of `MPI'.
   *
   * (Called before MPI.Init(), so cannot make any native MPI calls.)
   */

  Datatype() {}
  //public Datatype() {}  // debug

  /*
   * Constructor for basic datatypes.
   *
   * (Initialization done in separate `setBasic', so can create
   * datatype objects for `BYTE', etc in static initializers invoked before
   * MPI.Init(), then initialize objects after MPI initialized.)
   */

  Datatype(int Type) { 
    setBasic(Type) ;
  }

  void setBasic (int Type) { 
    switch(Type) {
      case OBJECT :
        baseType = OBJECT ;
        displacements = new int [1] ;
        lb = 0 ;
        ub = 1 ;
        lbSet = false ;
        ubSet = false ;

        break ;

      case LB :
        baseType = UNDEFINED ;
        displacements = new int [0] ;
        lb = 0 ;
        ub = 0 ;
        lbSet = true ;
        ubSet = false ;

        break ;

      case UB :
        baseType = UNDEFINED ;
        displacements = new int [0] ;
        lb = 0 ;
        ub = 0 ;
        lbSet = false ;
        ubSet = true ;

        break ;

      default :  // Native case

        baseType = Type ;  // what about PACKED?
        GetDatatype(Type); 

	baseSize = size() ;
    }
  }

  private native void GetDatatype(int Type);

  /*
   * Constructor used by `Contiguous'
   *
   * (Initialization done in separate `setContiguous', so can create
   * datatype objects for `SHORT2', etc in static initializers invoked before
   * MPI.Init(), then initialize objects after MPI initialized.)
   */

  private Datatype(int count, Datatype oldtype) throws MPIException {
    setContiguous(count, oldtype) ;
  }

  void setContiguous(int count, Datatype oldtype) throws MPIException {

    baseType = oldtype.baseType ;

    if(baseType == OBJECT || baseType == UNDEFINED) {

      int oldSize  = oldtype.Size() ;
      boolean oldUbSet = oldtype.ubSet ;
      boolean oldLbSet = oldtype.lbSet ;

      displacements = new int [count * oldSize] ;

      ubSet = count > 0 && oldUbSet ;
      lbSet = count > 0 && oldLbSet ;

      lb = Integer.MAX_VALUE ;
      ub = Integer.MIN_VALUE ;

      if(oldSize != 0 || oldLbSet || oldUbSet) {

	// `oldType.ub', `oldType.lb', `oldType.Extent()' all well-defined.

	int oldExtent  = oldtype.Extent() ;

	if(count > 0) {

	  // Compose proper displacements...

          int ptr = 0 ;
          for (int i = 0 ; i < count ; i++) {
	    int startElement = i * oldExtent ;

	    for (int l = 0; l < oldSize; l++, ptr++)
	      displacements [ptr] = startElement + oldtype.displacements[l] ;
          }

	  // Now maximize/minimize upper/lower bounds

          int maxStartElement = oldExtent > 0 ? (count - 1) * oldExtent : 0 ;
          int max_ub = maxStartElement + oldtype.ub ;
          if (max_ub > ub)
            ub = max_ub ;
            
          int minStartElement = oldExtent > 0 ? 0 : (count - 1) * oldExtent ;
          int min_lb = minStartElement + oldtype.lb ;
          if (min_lb < lb)
            lb = min_lb ;
        }
      }
      else {

        // `oldType.ub', `oldType.lb' and `oldType.Extent()' are undefined.
        // Can ignore unless...

        if(count > 1) {
          System.out.println("Datatype.Contiguous: repeat-count specified " +
                             "for component with undefined extent");
          MPI.COMM_WORLD.Abort(1);
        }
      }
    }
    else {
      baseSize = oldtype.baseSize ;

      GetContiguous(count, oldtype) ;
    }
  }

  private native void GetContiguous(int count, Datatype oldtype);


  /*
   * Constructor used by `Vector', `Hvector'
   */

  private Datatype(int count, int blocklength, int stride, Datatype oldtype,
                   boolean unitsOfOldExtent) throws MPIException {

    baseType = oldtype.baseType ;

    if(baseType == OBJECT || baseType == UNDEFINED) {

      int oldSize  = oldtype.Size() ;
      boolean oldUbSet = oldtype.ubSet ;
      boolean oldLbSet = oldtype.lbSet ;

      int repetitions = count * blocklength ;

      displacements = new int [repetitions * oldSize] ;

      ubSet = repetitions > 0 && oldUbSet ;
      lbSet = repetitions > 0 && oldLbSet ;

      lb = Integer.MAX_VALUE ;
      ub = Integer.MIN_VALUE ;

      if(repetitions > 0) {
        if(oldSize != 0 || oldLbSet || oldUbSet) {

	  // `oldType.ub', `oldType.lb', `oldType.Extent()' all well-defined.

	  int oldExtent  = oldtype.Extent() ;

          int ptr = 0 ;
          for (int i = 0 ; i < count ; i++) {

	    int startBlock = stride * i ;
            if(unitsOfOldExtent) startBlock *= oldExtent ;

	    // Compose proper displacements...

	    for (int j = 0; j < blocklength ; j++) {
	      int startElement = startBlock + j * oldExtent ;

	      for (int l = 0; l < oldSize; l++, ptr++)
		displacements [ptr] = startElement + oldtype.displacements[l] ;
	    }

	    // Now maximize/minimize upper/lower bounds

            int maxStartElement = 
                oldExtent > 0 ? startBlock + (blocklength - 1) * oldExtent
                              : startBlock ;
            int max_ub = maxStartElement + oldtype.ub ;
            if (max_ub > ub)
              ub = max_ub ;
            
            int minStartElement = 
                oldExtent > 0 ? startBlock 
                              : startBlock + (blocklength - 1) * oldExtent ;
            int min_lb = minStartElement + oldtype.lb ;
            if (min_lb < lb)
              lb = min_lb ;
          }
        }
        else {

          // `oldType.ub', `oldType.lb' and `oldType.Extent()' are undefined.

          if(unitsOfOldExtent) {
	    System.out.println("Datatype.Vector: " +
                               "old type has undefined extent");
	    MPI.COMM_WORLD.Abort(1);
          }
          else {

            // For `Hvector' can ignore unless...

            if(blocklength > 1) {
              System.out.println("Datatype.Hvector: repeat-count specified " +
                                 "for component with undefined extent");
              MPI.COMM_WORLD.Abort(1);
            }
          }
        }
      }
    }
    else {
      baseSize = oldtype.baseSize ;

      if(unitsOfOldExtent)
        GetVector(count, blocklength, stride, oldtype) ;
      else
        GetHvector(count, blocklength, stride, oldtype) ;
    }
  }

  private native void GetVector(int count, int blocklength, int stride,
                                Datatype oldtype);

  private native void GetHvector(int count, int blocklength, int stride,
                                 Datatype oldtype) ;


  /*
   * Constructor used by `Indexed', `Hindexed'
   */

  private Datatype(int[] array_of_blocklengths, int[] array_of_displacements,
                   Datatype oldtype, boolean unitsOfOldExtent)
                                                      throws MPIException {

    baseType = oldtype.baseType ;

    if(baseType == OBJECT || baseType == UNDEFINED) {

      int oldSize  = oldtype.Size() ;
      boolean oldUbSet = oldtype.ubSet ;
      boolean oldLbSet = oldtype.lbSet ;

      int count = 0 ;
      for (int i = 0; i < array_of_blocklengths.length; i++)
        count += array_of_blocklengths[i] ;

      displacements = new int [count * oldSize] ;

      ubSet = count > 0 && oldUbSet ;
      lbSet = count > 0 && oldLbSet ;

      lb = Integer.MAX_VALUE ;
      ub = Integer.MIN_VALUE ;

      if(oldSize != 0 || oldLbSet || oldUbSet) {

	// `oldType.ub', `oldType.lb', `oldType.Extent()' all well-defined.

	int oldExtent  = oldtype.Extent() ;

        int ptr = 0 ;
        for (int i = 0; i < array_of_blocklengths.length; i++) {
	  int blockLen = array_of_blocklengths [i] ;
	  if(blockLen > 0) {

	    int startBlock = array_of_displacements [i] ;
            if(unitsOfOldExtent) startBlock *= oldExtent ;

	    // Compose proper displacements...

	    for (int j = 0; j < blockLen ; j++) {
	      int startElement = startBlock + j * oldExtent ;

	      for (int l = 0; l < oldSize; l++, ptr++)
		displacements [ptr] = startElement + oldtype.displacements[l] ;
	    }

	    // Now maximize/minimize upper/lower bounds

            int maxStartElement = 
                oldExtent > 0 ? startBlock + (blockLen - 1) * oldExtent
                              : startBlock ;
            int max_ub = maxStartElement + oldtype.ub ;
            if (max_ub > ub)
              ub = max_ub ;
            
            int minStartElement = 
                oldExtent > 0 ? startBlock 
                              : startBlock + (blockLen - 1) * oldExtent ;
            int min_lb = minStartElement + oldtype.lb ;
            if (min_lb < lb)
              lb = min_lb ;
          }
        }
      }
      else {

        // `oldType.ub', `oldType.lb' and `oldType.Extent()' are undefined.

        if(unitsOfOldExtent) {
	  System.out.println("Datatype.Indexed: old type has undefined extent");
	  MPI.COMM_WORLD.Abort(1);
        }
        else {
          // Can ignore unless...

          for (int i = 0; i < array_of_blocklengths.length; i++)
            if(array_of_blocklengths [i] > 1) {
              System.out.println("Datatype.Hindexed: repeat-count specified " +
                                 "for component with undefined extent");
              MPI.COMM_WORLD.Abort(1);
            }
        }
      }
    }
    else {
      baseSize = oldtype.baseSize ;

      if(unitsOfOldExtent)
        GetIndexed(array_of_blocklengths, array_of_displacements, oldtype) ;
      else
        GetHindexed(array_of_blocklengths, array_of_displacements, oldtype) ;
    }
  }

  private native void GetIndexed(int[] array_of_blocklengths,
                                 int[] array_of_displacements, 
                                 Datatype oldtype) ;

  private native void GetHindexed(int[] array_of_blocklengths,
                                  int[] array_of_displacements,
                                  Datatype oldtype) ;


  /*
   * Constructor used by `Struct'
   */

  private Datatype(int[] array_of_blocklengths, int[] array_of_displacements,
                   Datatype[] array_of_types) throws MPIException {

    // Compute new base type

    baseType = UNDEFINED;
    for (int i = 0; i < array_of_types.length; i++) {
      int oldBaseType = array_of_types[i].baseType ;  
      if(oldBaseType != baseType) {
        if(baseType == UNDEFINED) {
          baseType = oldBaseType ;

          if(baseType != OBJECT)
            baseSize = array_of_types[i].baseSize ;
        }
        else if(oldBaseType != UNDEFINED) {
          System.out.println("Datatype.Struct: All base types must agree...");
          MPI.COMM_WORLD.Abort(1);
        }
      }
    }

    // Allocate `displacements' if required

    if(baseType == OBJECT || baseType == UNDEFINED) {
      int size = 0 ;
      for (int i = 0; i < array_of_blocklengths.length; i++)
        size += array_of_blocklengths[i] * array_of_types[i].Size();

      displacements = new int [size] ;
    }

    ubSet = false ;
    lbSet = false ;

    lb = Integer.MAX_VALUE ;
    ub = Integer.MIN_VALUE ;

    int ptr = 0 ;
    for (int i = 0; i < array_of_blocklengths.length; i++) {
      int blockLen     = array_of_blocklengths [i] ;
      if(blockLen > 0) {
        Datatype oldtype = array_of_types [i] ;
        int oldBaseType  = oldtype.baseType ;

        if(oldBaseType == OBJECT || oldBaseType == UNDEFINED) {

          int oldSize  = oldtype.Size() ;
          boolean oldUbSet = oldtype.ubSet ;
          boolean oldLbSet = oldtype.lbSet ;

          if(oldSize != 0 || oldLbSet || oldUbSet) {

            // `oldType.ub', `oldType.lb', `oldType.Extent()' all well-defined.

            int oldExtent  = oldtype.Extent() ;

            int startBlock = array_of_displacements [i] ;

            // Compose normal displacements...

            for (int j = 0; j < blockLen ; j++) {
              int startElement = startBlock + j * oldExtent ;

              for (int l = 0; l < oldSize; l++, ptr++)
                displacements [ptr] = startElement + oldtype.displacements[l] ;
            }

            // Now maximize/minimize upper/lower bounds

            // `ubSet' acts like a most significant positive bit in
            // the maximization operation.

            if (oldUbSet == ubSet) {
              int maxStartElement = 
                  oldExtent > 0 ? startBlock + (blockLen - 1) * oldExtent
                                : startBlock ;
              int max_ub = maxStartElement + oldtype.ub ;
              if (max_ub > ub)
                ub = max_ub ;
            }
            else if(oldUbSet) {
              int maxStartElement = 
                  oldExtent > 0 ? startBlock + (blockLen - 1) * oldExtent
                                : startBlock ;
              ub    = maxStartElement + oldtype.ub ;
              ubSet = true ;
            }
            
            // `lbSet' acts like a most significant negative bit in
            // the minimization operation.

            if (oldLbSet == lbSet) {
              int minStartElement = 
                  oldExtent > 0 ? startBlock 
                                : startBlock + (blockLen - 1) * oldExtent ;
              int min_lb = minStartElement + oldtype.lb ;
              if (min_lb < lb)
                lb = min_lb ;
            }
            else if(oldLbSet) {
              int minStartElement = 
                  oldExtent > 0 ? startBlock 
                                : startBlock + (blockLen - 1) * oldExtent ;
              lb    = minStartElement + oldtype.lb ;
              lbSet = true ;
            }
          }
          else {

            // `oldType.ub', `oldType.lb' and `oldType.Extent()' are undefined.
            // Can ignore unless...

            if(blockLen > 1) {
              System.out.println("Datatype.Struct: repeat-count specified " +
                                 "for component with undefined extent");
              MPI.COMM_WORLD.Abort(1);
            }
          }
        }
      }
    }

    if(baseType != OBJECT && baseType != UNDEFINED)
      GetStruct(array_of_blocklengths, array_of_displacements, array_of_types,
                lbSet, lb, ubSet, ub) ;
  }


  private native void GetStruct(int[] array_of_blocklengths,
                                int[] array_of_displacements, 
                                Datatype[] array_of_types,
                                boolean lbSet, int lb, boolean ubSet, int ub) ;


  protected boolean isObject() {
    return baseType == OBJECT || baseType == UNDEFINED ;
  }

  /**
   * Returns the extent of a datatype - the difference between
   * upper and lower bound.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> datatype extent </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_EXTENT</tt>.
   */

  public int Extent() throws MPIException {
    if(baseType == OBJECT || baseType == UNDEFINED)
      return ub - lb ;
    else
      return extent() / baseSize ;  
  }

  private native int extent();

  /**
   * Returns the total size of a datatype - the number of buffer
   * elements it represents.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> datatype size </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_SIZE</tt>.
   */

  public int Size() throws MPIException {
    if(baseType == OBJECT || baseType == UNDEFINED)
      return displacements.length;
    else 
      return size() / baseSize ;  
  }

  private native int size();

  /**
   * Find the lower bound of a datatype - the least value
   * in its displacement sequence.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> displacement of lower bound
   *                                      from origin </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_LB</tt>.
   */

  public int Lb() throws MPIException {
    if(baseType == OBJECT || baseType == UNDEFINED)
      return lb;
    else 
      return lB() / baseSize ;
  }

  private native int lB();

  /**
   * Find the upper bound of a datatype - the greatest value
   * in its displacement sequence.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> displacement of upper bound
   *                                      from origin </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_UB</tt>.
   */

  public int Ub() throws MPIException {
    if(baseType == OBJECT || baseType == UNDEFINED)
      return ub;
    else 
      return uB() / baseSize ; 
  }

  private native int uB();

  /**
   * Commit a derived datatype.
   * Java binding of the MPI operation <tt>MPI_TYPE_COMMIT</tt>.
   */

  public void Commit() throws MPIException {
    if (baseType != OBJECT && baseType != UNDEFINED)
      commit() ;
  }

  private native void commit();

  @SuppressWarnings("unchecked")
  public void finalize() throws MPIException {
      synchronized(MPI.class) {
          MPI.freeList.addFirst(this) ;
      }
  }

  native void free() ;

  /**
   * Construct new datatype representing replication of old datatype into
   * contiguous locations.
   * <p>
   * <table>
   * <tr><td><tt> count    </tt></td><td> replication count </tr>
   * <tr><td><tt> oldtype  </tt></td><td> old datatype </tr>
   * <tr><td><em> returns: </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_CONTIGUOUS</tt>.
   * <p>
   * The base type of the new datatype is the same as the base type of
   * <tt>oldtype</tt>.
   */

  public static Datatype Contiguous(int      count, 
                                    Datatype oldtype) throws MPIException { 

    return new Datatype(count, oldtype) ;
  }

  /**
   * Construct new datatype representing replication of old datatype into
   * locations that consist of equally spaced blocks.
   * <p>
   * <table>
   * <tr><td><tt> count       </tt></td><td> number of blocks </tr>
   * <tr><td><tt> blocklength </tt></td><td> number of elements in each
   *                                         block </tr>
   * <tr><td><tt> stride      </tt></td><td> number of elements between
   *                                         start of each block </tr>
   * <tr><td><tt> oldtype     </tt></td><td> old datatype </tr>
   * <tr><td><em> returns:    </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_VECTOR</tt>.
   * <p>
   * The base type of the new datatype is the same as the base type of
   * <tt>oldtype</tt>.
   */

  public static Datatype Vector(int      count, 
                                int      blocklength, 
                                int      stride, 
                                Datatype oldtype) throws MPIException {
 
    return new Datatype(count, blocklength, stride, oldtype, true) ;
  }

  /**
   * Identical to <tt>vector</tt> except that the stride is expressed
   * directly in terms of the buffer index, rather than the units of
   * the old type.
   * <p>
   * <table>
   * <tr><td><tt> count       </tt></td><td> number of blocks </tr>
   * <tr><td><tt> blocklength </tt></td><td> number of elements in each
   *                                         block </tr>
   * <tr><td><tt> stride      </tt></td><td> number of elements between
   *                                         start of each block </tr>
   * <tr><td><tt> oldtype     </tt></td><td> old datatype </tr>
   * <tr><td><em> returns:    </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_HVECTOR</tt>.
   * <p>
   * <em>Unlike other language bindings</em>, the value of <tt>stride</tt>
   * is <em>not</em> measured in bytes.
   */

  public static Datatype Hvector(int      count, 
                                 int      blocklength, 
                                 int      stride, 
                                 Datatype oldtype) throws MPIException {
 
    return new Datatype(count, blocklength, stride, oldtype, false) ;
  }

  /**
   * Construct new datatype representing replication of old datatype into
   * a sequence of blocks where each block can contain a different number
   * of copies and have a different displacement.
   * <p>
   * <table>
   * <tr><td><tt> array_of_blocklengths  </tt></td><td> number of elements per
   *                                                    block </tr>
   * <tr><td><tt> array_of_displacements </tt></td><td> displacement of each
   *                                                    block in units of
   *                                                    old type </tr>
   * <tr><td><tt> oldtype                </tt></td><td> old datatype </tr>
   * <tr><td><em> returns:               </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_INDEXED</tt>.
   * <p>
   * The number of blocks is taken to be size of the
   * <tt>array_of_blocklengths</tt> argument.  The second argument,
   * <tt>array_of_displacements</tt>, should be the same size.
   * The base type of the new datatype is the same as the base type of
   * <tt>oldtype</tt>.
   */

  public static Datatype Indexed(int[]    array_of_blocklengths, 
                                 int[]    array_of_displacements, 
                                 Datatype oldtype) throws MPIException {
  
    return new Datatype(array_of_blocklengths, array_of_displacements,
                        oldtype, true) ;
  }

  /**
   * Identical to <tt>indexed</tt> except that the displacements are
   * expressed directly in terms of the buffer index, rather than the
   * units of the old type.
   * <p>
   * <table>
   * <tr><td><tt> array_of_blocklengths  </tt></td><td> number of elements per
   *                                                    block </tr>
   * <tr><td><tt> array_of_displacements </tt></td><td> displacement in buffer
   *                                                    for each block </tr>
   * <tr><td><tt> oldtype                </tt></td><td> old datatype </tr>
   * <tr><td><em> returns:               </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_HINDEXED</tt>.
   * <p>
   * <em>Unlike other language bindings</em>, the values in
   * <tt>array_of_displacements</tt> are <em>not</em> measured in bytes.
   */

  public static Datatype Hindexed(int[]    array_of_blocklengths, 
                                  int[]    array_of_displacements, 
                                  Datatype oldtype) throws MPIException {
    return new Datatype(array_of_blocklengths, array_of_displacements,
                        oldtype, false) ;
  }

  /**
   * The most general type constructor.
   * <p>
   * <table>
   * <tr><td><tt> array_of_blocklengths  </tt></td><td> number of elements per
   *                                                    block </tr>
   * <tr><td><tt> array_of_displacements </tt></td><td> displacement in buffer
   *                                                    for each block </tr>
   * <tr><td><tt> array_of_types         </tt></td><td> type of elements in
   *                                                    each block </tr>
   * <tr><td><em> returns:               </em></td><td> new datatype </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_TYPE_STRUCT</tt>.
   * <p>
   * The number of blocks is taken to be size of the
   * <tt>array_of_blocklengths</tt> argument.  The second and third
   * arguments, <tt>array_of_displacements</tt>, and <tt>array_of_types</tt>,
   * should be the same size.
   * <em>Unlike other language bindings</em>, the values in
   * <tt>array_of_displacements</tt> are <em>not</em> measured in bytes.
   * All elements of <tt>array_of_types</tt> with definite base types
   * <em>must have the <em>same</em> base type</em>: this will be the base
   * type of new datatype.
   */

  public static Datatype Struct(int[] array_of_blocklengths,   
                                int[] array_of_displacements, 
                                Datatype[] array_of_types) throws MPIException {
    return new Datatype(array_of_blocklengths, array_of_displacements,
                        array_of_types) ;
  }

    // Created by JMS -- add proper documentation later
    // JMS Aint's are not ints!  :-( Need to fix that throughout the
    // API...
    public static Datatype Resized(Datatype oldtype,
				   int lb,
				   int extent) throws MPIException { 
	return new Datatype_resized(oldtype) ;
    }

  protected long handle;
  protected int baseType ;
  protected int baseSize ;  // or private

  protected int displacements[] ;

  protected int lb, ub ;

  protected boolean ubSet, lbSet ;
    // Flags set if MPI.UB, MPI.LB respectively appears as a component type.

  static {
    init();
  }

}

// Things to do:
//
//   Initialization and use of `baseSize' should probably be done entirely
//   on JNI side.
//
//   `baseType' could just take values from {UNDEFINED, OBJECT, NATIVE}?
//   (But in future may want to add runtime checks using exact value.)

