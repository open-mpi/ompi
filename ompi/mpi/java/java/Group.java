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

/**
 * This class represents {@code MPI_Group}.
 */
public final class Group implements Freeable
{
protected long handle;
private static long nullHandle;

static
{
    init();
}

private static native void init();

protected static native long getEmpty();

protected Group(long handle)
{
    this.handle = handle;
}

/**
 * Java binding of the MPI operation {@code MPI_GROUP_SIZE}.
 * @return number of processes in the group
 * @throws MPIException 
 */
public int getSize() throws MPIException
{
    MPI.check();
    return getSize(handle);
}

private native int getSize(long group) throws MPIException;

/**
 * Rank of this process in the group.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_RANK}.
 * @return rank of this process in the group, or {@code MPI.UNDEFINED}
 *         if this process is not a member of the group.
 * @throws MPIException
 */
public int getRank() throws MPIException
{
    MPI.check();
    return getRank(handle);
}

private native int getRank(long group) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_GROUP_FREE}.
 */
@Override public void free() throws MPIException
{
    MPI.check();
    handle = free(handle);
}

private native long free(long group);

/**
 * Test if group object is null.
 * @return true if the group object is null.
 */
public boolean isNull()
{
    return handle == nullHandle;
}

/**
 * Translate ranks within one group to ranks within another.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_TRANSLATE_RANKS}.
 * <p>Result elements are {@code MPI.UNDEFINED} where no correspondence exists.
 * @param group1 a group
 * @param ranks1 array of valid ranks in group1
 * @param group2 another group
 * @return array of corresponding ranks in group2
 * @throws MPIException 
 */
public static int[] translateRanks(Group group1, int[] ranks1, Group group2)
    throws MPIException
{
    MPI.check();
    return translateRanks(group1.handle, ranks1, group2.handle);
}

private static native int[] translateRanks(
        long group1, int[] ranks1, long group2) throws MPIException;

/**
 * Compare two groups.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_COMPARE}.
 * @param group1 first group
 * @param group2 second group
 * @return {@code MPI.IDENT} if the group members and group order are exactly
 *         the same in both groups, {@code MPI.SIMILAR} if the group members are
 *         the same but the order is different, {@code MPI.UNEQUAL} otherwise.
 * @throws MPIException 
 */
public static int compare(Group group1, Group group2) throws MPIException
{
    MPI.check();
    return compare(group1.handle, group2.handle);
}

private static native int compare(long group1, long group2) throws MPIException;

/**
 * Set union of two groups.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_UNION}.
 * @param group1 first group
 * @param group2 second group
 * @return union group
 * @throws MPIException 
 */
public static Group union(Group group1, Group group2) throws MPIException
{
    MPI.check();
    return new Group(union(group1.handle, group2.handle));
}

private static native long union(long group1, long group2);

/**
 * Set intersection of two groups.
 * Java binding of the MPI operation {@code MPI_GROUP_INTERSECTION}.
 * @param group1 first group
 * @param group2 second group
 * @return intersection group
 * @throws MPIException 
 */
public static Group intersection(Group group1, Group group2) throws MPIException
{
    MPI.check();
    return new Group(intersection(group1.handle, group2.handle));
}

private static native long intersection(long group1, long group2);

/**
 * Set difference of two groups.
 * Java binding of the MPI operation {@code MPI_GROUP_DIFFERENCE}.
 * @param group1 first group
 * @param group2 second group
 * @return difference group
 * @throws MPIException 
 */
public static Group difference(Group group1, Group group2) throws MPIException
{
    MPI.check();
    return new Group(difference(group1.handle, group2.handle));
}

private static native long difference(long group1, long group2);

/**
 * Create a subset group including specified processes.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_INCL}.
 * @param ranks ranks from this group to appear in new group
 * @return new group
 * @throws MPIException 
 */
public Group incl(int[] ranks) throws MPIException
{
    MPI.check();
    return new Group(incl(handle, ranks));
}

private native long incl(long group, int[] ranks);

/**
 * Create a subset group excluding specified processes.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_EXCL}.
 * @param ranks ranks from this group <em>not</em> to appear in new group
 * @return new group
 * @throws MPIException 
 */
public Group excl(int[] ranks) throws MPIException
{
    MPI.check();
    return new Group(excl(handle, ranks));
}

private native long excl(long group, int[] ranks);

/**
 * Create a subset group including processes specified
 * by strided intervals of ranks.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_RANGE_INCL}.
 * <p>The triplets are of the form (first rank, last rank, stride)
 * indicating ranks in this group to be included in the new group.
 * The size of the first dimension of {@code ranges} is the number
 * of triplets.  The size of the second dimension is 3.
 * @param ranges array of integer triplets
 * @return new group
 * @throws MPIException 
 */
public Group rangeIncl(int[][] ranges) throws MPIException
{
    MPI.check();
    return new Group(rangeIncl(handle, ranges));
}

private native long rangeIncl(long group, int[][] ranges);

/**
 * Create a subset group excluding processes specified
 * by strided intervals of ranks.
 * <p>Java binding of the MPI operation {@code MPI_GROUP_RANGE_EXCL}.
 * <p>Triplet array is defined as for {@code rangeIncl}, the ranges
 * indicating ranks in this group to be excluded from the new group.
 * @param ranges array of integer triplets
 * @return new group
 * @throws MPIException 
 */
public Group rangeExcl(int[][] ranges) throws MPIException
{
    MPI.check();
    return new Group(rangeExcl(handle, ranges));
}

private native long rangeExcl(long group, int[][] ranges);

} // Group
