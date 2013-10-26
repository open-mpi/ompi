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
/* File         : Request.java
 * Author       : Sang Lim, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2001/08/07 16:36:25 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

/*
 * Note: in a send request for a buffer containing objects, the primary
 * `MPI_Request' referenced by `handle' is the request to send the data.
 * The request to send the header is in the secondary field, `hdrReq'.
 * Conversely, in a *receive* request for a buffer containing objects
 * the primary `MPI_Request' is the request to send the header.
 * The receive of the data is not initiated until a `wait' or `test'
 * operation succeeds.
 */

/*
 * Probably `Request' should be an abstract class, and there should
 * be several concrete subclasses.  At the moment requests are created
 * in a few different ways, and the differently constructed requests are
 * typically using different subsets of fields.  DBC 7/12/01
 */

package mpi;

/**
 * Request object.
 */
public class Request implements Freeable
{
protected long handle;

static
{
    init();
}

private static native void init();

protected static native long getNull();

protected Request(long handle)
{
    this.handle = handle;
}

/**
 * Set the request object to be void.
 * Java binding of the MPI operation {@code MPI_REQUEST_FREE}.
 */
@Override public void free() throws MPIException
{
    MPI.check();
    handle = free(handle);
}

private native long free(long req) throws MPIException;

/**
 * Mark a pending nonblocking communication for cancellation.
 * Java binding of the MPI operation {@code MPI_CANCEL}.
 */
public final void cancel() throws MPIException
{
    MPI.check();
    cancel_jni();
}

private native void cancel_jni() throws MPIException;

/**
 * Test if request object is null.
 * @return true if the request object is null, false otherwise
 */
public final native boolean isNull();

/**
 * Blocks until the operation identified by the request is complete.
 * <p>Java binding of the MPI operation {@code MPI_WAIT}.
 * <p>After the call returns, the request object becomes inactive.
 * @return status object
 * @throws MPIException 
 */
public final Status waitStatus() throws MPIException
{
    MPI.check();
    Status stat = new Status();
    waitStatus(stat);
    return stat;
}

private native void waitStatus(Status stat) throws MPIException;

/**
 * Blocks until the operation identified by the request is complete.
 * <p>Java binding of the MPI operation {@code MPI_WAIT}.
 * <p>After the call returns, the request object becomes inactive.
 * @throws MPIException 
 */
public final void waitFor() throws MPIException
{
    MPI.check();
    waitNoStatus();
}

private native void waitNoStatus() throws MPIException;

/**
 * Returns a status object if the operation identified by the request
 * is complete, or a null reference otherwise.
 * <p>Java binding of the MPI operation {@code MPI_TEST}.
 * <p>After the call, if the operation is complete (ie, if the return
 * value is non-null), the request object becomes inactive.
 * @return status object
 * @throws MPIException 
 */
public final Status testStatus() throws MPIException
{
    MPI.check();
    return testStatus_jni();
}

private native Status testStatus_jni() throws MPIException;

/**
 * Returns true if the operation identified by the request
 * is complete, or false otherwise.
 * <p>Java binding of the MPI operation {@code MPI_TEST}.
 * <p>After the call, if the operation is complete (ie, if the return
 * value is true), the request object becomes inactive.
 * @return true if the operation identified by the request, false otherwise
 * @throws MPIException 
 */
public final boolean test() throws MPIException
{
    MPI.check();
    return testNoStatus();
}

private native boolean testNoStatus() throws MPIException;

/**
 * Blocks until one of the operations associated with the active
 * requests in the array has completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITANY}.
 * <p>The index in array of {@code requests} for the request that
 * completed can be obtained from the returned status object through
 * the {@code Status.getIndex()} method. The corresponding element
 * of array of {@code requests} becomes inactive.
 * @param requests array of requests
 * @return status object
 * @throws MPIException 
 */
public static Status waitAnyStatus(Request[] requests) throws MPIException
{
    MPI.check();
    Status stat = new Status();
    waitAnyStatus(requests, stat);
    return stat;
}

private static native void waitAnyStatus(Request[] requests, Status stat)
        throws MPIException;

/**
 * Blocks until one of the operations associated with the active
 * requests in the array has completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITANY}.
 * <p>The request that completed becomes inactive.
 * @param requests array of requests
 * @return The index in array of {@code requests} for the request that
 * completed. If all of the requests are MPI_REQUEST_NULL, then index
 * is returned as {@code MPI.UNDEFINED}.
 * @throws MPIException 
 */
public static int waitAny(Request[] requests) throws MPIException
{
    MPI.check();
    return waitAnyNoStatus(requests);
}

private static native int waitAnyNoStatus(Request[] requests)
        throws MPIException;

/**
 * Tests for completion of either one or none of the operations
 * associated with active requests.
 * <p>Java binding of the MPI operation {@code MPI_TESTANY}.
 * <p>If some request completed, the index in array of {@code requests}
 * for that request can be obtained from the returned status object.
 * The corresponding element in array of {@code requests} becomes inactive.
 * If no request completed, {testAny} returns {@code null}.
 * @param requests array of requests
 * @return status object if one request completed, {@code null} otherwise.
 * @throws MPIException 
 */
public static Status testAnyStatus(Request[] requests) throws MPIException
{
    MPI.check();
    return testAnyStatus_jni(requests);
}

private static native Status testAnyStatus_jni(Request[] requests)
        throws MPIException;

/**
 * Tests for completion of either one or none of the operations
 * associated with active requests.
 * <p>Java binding of the MPI operation {@code MPI_TESTANY}.
 * <p>If some request completed, ii becomes inactive.
 * @param requests array of requests
 * @return index of operation that completed, or {@code MPI.UNDEFINED}
 * if none completed.
 * @throws MPIException 
 */
public static int testAny(Request[] requests) throws MPIException
{
    MPI.check();
    return testAnyNoStatus(requests);
}

private static native int testAnyNoStatus(Request[] requests)
        throws MPIException;

/**
 * Blocks until all of the operations associated with the active
 * requests in the array have completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITALL}.
 * <p>On exit, requests become inactive.  If the <em>input</em> value of
 * array of {@code requests} contains inactive requests, corresponding
 * elements of the status array will contain null status references.
 * @param requests array of requests
 * @return array of statuses
 * @throws MPIException 
 */
public static Status[] waitAllStatus(Request[] requests) throws MPIException
{
    MPI.check();
    Status[] statuses = new Status[requests.length];
    waitAllStatus(requests, statuses);
    return statuses;
}

private static native void waitAllStatus(Request[] requests, Status[] statuses)
        throws MPIException;

/**
 * Blocks until all of the operations associated with the active
 * requests in the array have completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITALL}.
 * @param requests array of requests
 * @throws MPIException 
 */
public static void waitAll(Request[] requests) throws MPIException
{
    MPI.check();
    waitAllNoStatus(requests);
}

private static native void waitAllNoStatus(Request[] requests)
        throws MPIException;

/**
 * Tests for completion of <em>all</em> of the operations associated
 * with active requests.
 * <p>Java binding of the MPI operation {@code MPI_TESTALL}.
 * <p>If all operations have completed, the exit value of the argument array
 * is as for {@code waitAllStatus}.
 * @param requests array of requests
 * @return array of statuses if all operations have completed,
 *         {@code null} otherwise.
 * @throws MPIException 
 */
public static Status[] testAllStatus(Request[] requests) throws MPIException
{
    MPI.check();
    return testAllStatus_jni(requests);
}

private static native Status[] testAllStatus_jni(Request[] requests)
        throws MPIException;

/**
 * Tests for completion of <em>all</em> of the operations associated
 * with active requests.
 * <p>Java binding of the MPI operation {@code MPI_TESTALL}.
 * @param requests array of requests
 * @return {@code true} if all operations have completed,
 *         {@code false} otherwise.
 * @throws MPIException 
 */
public static boolean testAll(Request[] requests) throws MPIException
{
    MPI.check();
    return testAllNoStatus(requests);
}

private static native boolean testAllNoStatus(Request[] requests)
        throws MPIException;

/**
 * Blocks until at least one of the operations associated with the active
 * requests in the array has completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITSOME}.
 * <p>The size of the result array will be the number of operations that
 * completed. The index in array of {@code requests} for each request that
 * completed can be obtained from the returned status objects through the
 * {@code Status.getIndex()} method. The corresponding element in
 * array of {@code requests} becomes inactive.
 * @param requests array of requests
 * @return array of statuses or {@code null} if the number of operations
 *         completed is {@code MPI_UNDEFINED}.
 * @throws MPIException
 */
public static Status[] waitSomeStatus(Request[] requests) throws MPIException
{
    MPI.check();
    return waitSomeStatus_jni(requests);
}

private static native Status[] waitSomeStatus_jni(Request[] requests)
        throws MPIException;

/**
 * Blocks until at least one of the operations associated with the active
 * active requests in the array has completed.
 * <p>Java binding of the MPI operation {@code MPI_WAITSOME}.
 * <p>The size of the result array will be the number of operations that
 * completed. The corresponding element in array of {@code requests} becomes
 * inactive.
 * @param requests array of requests
 * @return array of indexes of {@code requests} that completed or {@code null}
 *         if the number of operations completed is {@code MPI_UNDEFINED}.
 * @throws MPIException 
 */
public static int[] waitSome(Request[] requests) throws MPIException
{
    MPI.check();
    return waitSomeNoStatus(requests);
}

private static native int[] waitSomeNoStatus(Request[] requests)
        throws MPIException;

/**
 * Behaves like {@code waitSome}, except that it returns immediately.
 * <p>Java binding of the MPI operation {@code MPI_TESTSOME}.
 * <p>If no operation has completed, {@code testSome} returns an array of
 * length zero, otherwise the return value are as for {@code waitSome}.
 * @param requests array of requests
 * @return array of statuses
 * @throws MPIException 
 */
public static Status[] testSomeStatus(Request[] requests) throws MPIException
{
    MPI.check();
    return testSomeStatus_jni(requests);
}

private static native Status[] testSomeStatus_jni(Request[] requests)
        throws MPIException;

/**
 * Behaves like {@code waitSome}, except that it returns immediately.
 * <p>Java binding of the MPI operation {@code MPI_TESTSOME}.
 * <p>If no operation has completed, {@code testSome} returns an array of
 * length zero, otherwise the return value are as for {@code waitSome}.
 * @param requests array of requests
 * @return array of indexes of {@code requests} that completed.
 * @throws MPIException 
 */
public static int[] testSome(Request[] requests) throws MPIException
{
    MPI.check();
    return testSomeNoStatus(requests);
}

private static native int[] testSomeNoStatus(Request[] requests)
        throws MPIException;

} // Request
