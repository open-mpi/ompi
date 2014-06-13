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
 * File         : Cartcomm.java
 * Author       : Xinying Li
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.7 $
 * Updated      : $Date: 2001/10/22 21:07:55 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * Communicator with cartesian structure.
 */
public final class CartComm extends Intracomm
{
static
{
    init();
}

private static native void init();

protected CartComm(long handle) throws MPIException
{
    super(handle);
}

protected CartComm(long[] commRequest)
{
    super(commRequest);
}

/**
 * Duplicates this communicator.
 * <p>Java binding of {@code MPI_COMM_DUP}.
 * <p>It is recommended to use {@link #dup} instead of {@link #clone}
 * because the last can't throw an {@link mpi.MPIException}.
 * @return copy of this communicator
 */
@Override public CartComm clone()
{
    try
    {
        return dup();
    }
    catch(MPIException e)
    {
        throw new RuntimeException(e.getMessage());
    }
}

/**
 * Duplicates this communicator.
 * <p>Java binding of {@code MPI_COMM_DUP}.
 * @return copy of this communicator
 * @throws MPIException
 */
@Override public CartComm dup() throws MPIException
{
    MPI.check();
    return new CartComm(dup(handle));
}

/**
 * Duplicates this communicator.
 * <p>Java binding of {@code MPI_COMM_IDUP}.
 * <p>The new communicator can't be used before the operation completes.
 * The request object must be obtained calling {@link #getRequest}.
 * @return copy of this communicator
 * @throws MPIException
 */
@Override public CartComm iDup() throws MPIException
{
    MPI.check();
    return new CartComm(iDup(handle));
}

/**
 * Returns cartesian topology information.
 * <p>Java binding of the MPI operations {@code MPI_CARTDIM_GET} and
 * {@code MPI_CART_GET}.
 * <p>The number of dimensions can be obtained from the size of (eg)
 * {@code dims} field of the returned object.
 * @return object containing dimensions, periods and local coordinates
 * @throws MPIException
 */
public CartParms getTopo() throws MPIException
{
    MPI.check();
    return getTopo(handle);
}

private native CartParms getTopo(long comm) throws MPIException;

/**
 * Translate logical process coordinates to process rank.
 * <p>Java binding of the MPI operation {@code MPI_CART_RANK}.
 * @param coords Cartesian coordinates of a process
 * @return rank of the specified process
 * @throws MPIException
 */
public int getRank(int[] coords) throws MPIException
{
    MPI.check();
    return getRank(handle, coords);
}

private native int getRank(long comm, int[] coords) throws MPIException;

/**
 * Translate process rank to logical process coordinates.
 * <p>Java binding of the MPI operation {@code MPI_CART_COORDS}.
 * @param rank rank of a process
 * @return Cartesian coordinates of the specified process
 * @throws MPIException
 */
public int[] getCoords(int rank) throws MPIException
{
    MPI.check();
    return getCoords(handle, rank);
}

private native int[] getCoords(long comm, int rank) throws MPIException;

/**
 * Compute source and destination ranks for "shift" communication.
 * <p>Java binding of the MPI operation {@code MPI_CART_SHIFT}.
 * @param direction coordinate dimension of shift
 * @param disp      displacement
 * @return object containing ranks of source and destination processes
 * @throws MPIException
 */
public ShiftParms shift(int direction, int disp) throws MPIException
{
    MPI.check();
    return shift(handle, direction, disp);
}

private native ShiftParms shift(long comm, int direction, int disp)
        throws MPIException;

/**
 * Partition cartesian communicator into subgroups of lower dimension.
 * <p>Java binding of the MPI operation {@code MPI_CART_SUB}.
 * @param remainDims by dimension, {@code true} if dimension is to be kept,
 *                   {@code false} otherwise
 * @return communicator containing subgrid including this process
 * @throws MPIException
 */
public CartComm sub(boolean[] remainDims) throws MPIException
{
    MPI.check();
    return new CartComm(sub(handle, remainDims));
}

private native long sub(long comm, boolean[] remainDims) throws MPIException;

/**
 * Compute an optimal placement.
 * <p>Java binding of the MPI operation {@code MPI_CART_MAP}.
 * <p>The number of dimensions is taken to be size of the {@code dims} argument.
 * @param dims    the number of processes in each dimension
 * @param periods {@code true} if grid is periodic,
 *                {@code false} if not, in each dimension
 * @return reordered rank of calling process
 * @throws MPIException
 */
public int map(int[] dims, boolean[] periods) throws MPIException
{
    MPI.check();
    return map(handle, dims, periods);
}

private native int map(long comm, int[] dims, boolean[] periods)
        throws MPIException;

/**
 * Select a balanced distribution of processes per coordinate direction.
 * <p>Java binding of the MPI operation {@code MPI_DIMS_CREATE}.
 * @param nnodes number of nodes in a grid
 * @param dims   array specifying the number of nodes in each dimension
 * @throws MPIException
 */
public static void createDims(int nnodes, int[] dims) throws MPIException
{
    MPI.check();
    createDims_jni(nnodes, dims);
}

private static native void createDims_jni(int nnodes, int[] dims)
        throws MPIException;

} // Cartcomm
