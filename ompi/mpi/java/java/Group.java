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
 * File         : Group.java
 * Author       : Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.8 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;
//import mpi.*;

public class Group extends Freeable {
  protected final static int EMPTY = 0;

  private static native void init(); 
  protected long handle;

  //public Group() {}
  protected Group(int Type) { GetGroup(Type); }
  protected Group(long _handle) { handle = _handle;}
  private native void GetGroup(int Type);

  /**
   * Size of group.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> number of processors in the
   *                                      group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_SIZE</tt>.
   */

  public native int Size() throws MPIException ;

  /**
   * Rank of this process in group.
   * <p>
   * <table>
   * <tr><td><em> returns: </em></td><td> rank of the calling process in
   *                                      the group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_RANK</tt>.
   *
   * Result value is <tt>MPI.UNDEFINED</tt> if this process is not
   * a member of the group.
   */

  public native int Rank() throws MPIException ;

  /**
   * Destructor.
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_FREE</tt>.
   */

  @SuppressWarnings("unchecked")
  public void finalize() throws MPIException {
      synchronized(MPI.class) {
          MPI.freeList.addFirst(this) ;
      }
  }

  native void free() ;

  /**
   * Translate ranks within one group to ranks within another.
   * <p>
   * <table>
   * <tr><td><tt> group1   </tt></td><td> a group </tr>
   * <tr><td><tt> ranks1   </tt></td><td> array of valid ranks in
   *                                      <tt>group1</tt> </tr>
   * <tr><td><tt> group2   </tt></td><td> another group </tr>
   * <tr><td><em> returns: </em></td><td> array of corresponding ranks in
   *                                      <tt>group2</tt> </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_TRANSLATE_RANKS</tt>.
   * <p>
   * Result elements are <tt>MPI.UNDEFINED</tt> where no correspondence
   * exists.
   */

  public static native int [] Translate_ranks(Group group1,int [] ranks1,
	                                      Group group2)
                                                     throws MPIException ;

  /**
   * Compare two groups.
   * <p>
   * <table>
   * <tr><td><tt> group1   </tt></td><td> first group </tr>
   * <tr><td><tt> group2   </tt></td><td> second group </tr>
   * <tr><td><em> returns: </em></td><td> result </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_COMPARE</tt>.
   * <p>
   * <tt>MPI.IDENT</tt> results if the group members and group order are
   * exactly the same in both groups.  <tt>MPI.SIMILAR</tt> results if
   * the group members are the same but the order is different.
   * <tt>MPI.UNEQUAL</tt> results otherwise.
   */

  public static native int Compare(Group group1, Group group2)
                                                     throws MPIException ;
 
  /**
   * Set union of two groups.
   * <p>
   * <table>
   * <tr><td><tt> group1   </tt></td><td> first group </tr>
   * <tr><td><tt> group2   </tt></td><td> second group </tr>
   * <tr><td><em> returns: </em></td><td> union group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_UNION</tt>.
   */

  public static Group Union(Group group1, Group group2) throws MPIException {
    return new Group(union(group1, group2)) ;
  }

  private static native long union(Group group1, Group group2);

  /**
   * Set intersection of two groups.
   * <p>
   * <table>
   * <tr><td><tt> group1   </tt></td><td> first group </tr>
   * <tr><td><tt> group2   </tt></td><td> second group </tr>
   * <tr><td><em> returns: </em></td><td> intersection group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_INTERSECTION</tt>.
   */

  public static Group Intersection(Group group1,Group group2)
                                                        throws MPIException {
    return new Group(intersection(group1, group2)) ;
  }

  private static native long intersection(Group group1, Group group2);

  /**
   * Result contains all elements of the first group that are not in the
   * second group.
   * <p>
   * <table>
   * <tr><td><tt> group1   </tt></td><td> first group </tr>
   * <tr><td><tt> group2   </tt></td><td> second group </tr>
   * <tr><td><em> returns: </em></td><td> difference group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_DIFFERENCE</tt>.
   */

  public static Group Difference(Group group1, Group group2)
                                                        throws MPIException {
    return new Group(difference(group1, group2)) ;
  }

  private static native long difference(Group group1, Group group2) ;

  /**
   * Create a subset group including specified processes.
   * <p>
   * <table>
   * <tr><td><tt> ranks    </tt></td><td> ranks from this group to appear in
   *                                      new group </tr>
   * <tr><td><em> returns: </em></td><td> new group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_INCL</tt>.
   */

  public Group Incl(int [] ranks) throws MPIException {
    return new Group(incl(ranks)) ;
  }

  private native long incl(int [] ranks);

  /**
   * Create a subset group excluding specified processes.
   * <p>
   * <table>
   * <tr><td><tt> ranks    </tt></td><td> ranks from this group <em>not</em>
   *                                      to appear in new group </tr>
   * <tr><td><em> returns: </em></td><td> new group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_EXCL</tt>.
   */

  public Group Excl(int [] ranks) throws MPIException {
    return new Group(excl(ranks)) ;
  }

  private native long excl(int [] ranks) ;

  /**
   * Create a subset group including processes specified
   * by strided intervals of ranks.
   * <p>
   * <table>
   * <tr><td><tt> ranges   </tt></td><td> array of integer triplets </tr>
   * <tr><td><em> returns: </em></td><td> new group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_RANGE_INCL</tt>.
   * <p>
   * The triplets are of the form (first rank, last rank, stride)
   * indicating ranks in this group to be included in the new group.
   * The size of the first dimension of <tt>ranges</tt> is the number
   * of triplets.  The size of the second dimension is 3.
   */

  public Group Range_incl(int [][] ranges) throws MPIException {
    return new Group(range_incl(ranges)) ;
  }

  private native long range_incl(int [][] ranges) ;

  /**
   * Create a subset group excluding processes specified
   * by strided intervals of ranks.
   * <p>
   * <table>
   * <tr><td><tt> ranges   </tt></td><td> array of integer triplets </tr>
   * <tr><td><em> returns: </em></td><td> new group </tr>
   * </table>
   * <p>
   * Java binding of the MPI operation <tt>MPI_GROUP_RANGE_EXCL</tt>.
   * <p>
   * Triplet array is defined as for <tt>Range_incl</tt>, the ranges
   * indicating ranks in this group to be excluded from the new group.
   */

  public Group Range_excl(int [][] ranges) throws MPIException {
    return new Group(range_excl(ranges)) ;
  }

  private native long range_excl(int [][] ranges) ;

  static {
    init();
  }          

}

