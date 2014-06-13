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
 * File         : Status.java
 * Author       : Sang Lim, Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.15 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

/**
 * This class represents {@code MPI_Status}.
 */
public final class Status
{
protected final long[] data;

static
{
    init();
}

private static native void init();

/**
 * Status objects must be created only by the MPI methods.
 */
protected Status()
{
    data = new long[6];
}

/**
 * Returns the number of received entries.
 * <p>Java binding of the MPI operation {@code MPI_GET_COUNT}.
 * @param datatype datatype of each item in receive buffer
 * @return number of received entries
 * @throws MPIException 
 */
public int getCount(Datatype datatype) throws MPIException
{
    MPI.check();
    int  i = 0;
    int  source    = (int)data[i++];
    int  tag       = (int)data[i++];
    int  error     = (int)data[i++];
    int  cancelled = (int)data[i++];
    long ucount    = data[i++];
    return getCount(source, tag, error, cancelled, ucount, datatype.handle);
}

private native int getCount(
        int source, int tag, int error,
        int cancelled, long ucount, long datatype) throws MPIException;

/**
 * Tests if the communication was cancelled.
 * <p>Java binding of the MPI operation {@code MPI_TEST_CANCELLED}.
 * @return true if the operation was succesfully cancelled, false otherwise
 * @throws MPIException 
 */
public boolean isCancelled() throws MPIException
{
    MPI.check();
    int  i = 0;
    int  source    = (int)data[i++];
    int  tag       = (int)data[i++];
    int  error     = (int)data[i++];
    int  cancelled = (int)data[i++];
    long ucount    = data[i++];
    return isCancelled(source, tag, error, cancelled, ucount);
}

private native boolean isCancelled(
        int source, int tag, int error, int cancelled, long ucount)
        throws MPIException;

/**
 * Retrieves the number of basic elements from status.
 * <p>Java binding of the MPI operation {@code MPI_GET_ELEMENTS}.
 * @param datatype datatype used by receive operation
 * @return number of received basic elements
 * @throws MPIException 
 */
public int getElements(Datatype datatype) throws MPIException
{
    MPI.check();
    int  i = 0;
    int  source    = (int)data[i++];
    int  tag       = (int)data[i++];
    int  error     = (int)data[i++];
    int  cancelled = (int)data[i++];
    long ucount    = data[i++];
    return getElements(source, tag, error, cancelled, ucount, datatype.handle);
}

private native int getElements(
        int source, int tag, int error,
        int cancelled, long ucount, long datatype) throws MPIException;

/**
 * Returns the "source" of message.
 * <p>Java binding of the MPI value {@code MPI_SOURCE}.
 * @return source of message
 */
public int getSource()
{
    return (int)data[0];
}

/**
 * Returns the "tag" of message.
 * <p>Java binding of the MPI value {@code MPI_TAG}.
 * @return tag of message
 */
public int getTag()
{
    return (int)data[1];
}

/**
 * Returns the {@code MPI_ERROR} of message.
 * @return error of message.
 */
public int getError()
{
    return (int)data[2];
}

/**
 * Returns the index of message.
 * @return index of message.
 */
public int getIndex()
{
    return (int)data[5];
}

} // Status
