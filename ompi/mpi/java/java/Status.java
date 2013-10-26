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
private int  source;
private int  tag;
private int  error;
private int  index;
private int  elements;
private int  _cancelled;
private long _ucount;

static
{
    init();
}

/**
 * Status objects must be created only by the MPI methods.
 */
protected Status()
{
}

private static native void init();

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
    return getCount_jni(datatype);
}

private native int getCount_jni(Datatype datatype) throws MPIException;

/**
 * Tests if the communication was cancelled.
 * <p>Java binding of the MPI operation {@code MPI_TEST_CANCELLED}.
 * @return true if the operation was succesfully cancelled, false otherwise
 * @throws MPIException 
 */
public boolean isCancelled() throws MPIException
{
    MPI.check();
    return isCancelled_jni();
}

private native boolean isCancelled_jni() throws MPIException;

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
    return getElements_jni(datatype);
}

private native int getElements_jni(Datatype datatype) throws MPIException;

/**
 * Returns the "source" of message.
 * <p>Java binding of the MPI value {@code MPI_SOURCE}.
 * @return source of message
 */
public int getSource()
{
    return source;
}

/**
 * Returns the "tag" of message.
 * <p>Java binding of the MPI value {@code MPI_TAG}.
 * @return tag of message
 */
public int getTag()
{
    return tag;
}

/**
 * Returns the {@code MPI_ERROR} of message.
 * @return error of message.
 */
public int getError()
{
    return error;
}

/**
 * Returns the index of message.
 * @return index of message.
 */
public int getIndex()
{
    return index;
}

} // Status
