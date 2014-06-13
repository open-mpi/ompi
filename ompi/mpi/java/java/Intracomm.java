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

/*
 * IMPLEMENTATION DETAILS
 * 
 * All methods with buffers that can be direct or non direct have
 * a companion argument 'db' which is true if the buffer is direct.
 * For example, if the buffer argument is recvBuf, the companion
 * argument will be 'rdb', meaning if the receive buffer is direct.
 * 
 * Checking if a buffer is direct is faster in Java than C.
 */
package mpi;

import java.nio.*;
import static mpi.MPI.assertDirectBuffer;

/**
 * This class represents intracommunicator.
 */
public class Intracomm extends Comm
{
protected Intracomm()
{
}

protected Intracomm(long handle)
{
    super(handle);
}

protected Intracomm(long[] commRequest)
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
@Override public Intracomm clone()
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
@Override public Intracomm dup() throws MPIException
{
    MPI.check();
    return new Intracomm(dup(handle));
}

/**
 * Duplicates this communicator.
 * <p>Java binding of {@code MPI_COMM_IDUP}.
 * <p>The new communicator can't be used before the operation completes.
 * The request object must be obtained calling {@link #getRequest}.
 * @return copy of this communicator
 * @throws MPIException
 */
@Override public Intracomm iDup() throws MPIException
{
    MPI.check();
    return new Intracomm(iDup(handle));
}

/**
 * Partition the group associated with this communicator and create
 * a new communicator within each subgroup.
 * <p>Java binding of the MPI operation {@code MPI_COMM_SPLIT}.
 * @param colour control of subset assignment
 * @param key    control of rank assignment
 * @return new communicator
 * @throws MPIException
 */
public final Intracomm split(int colour, int key) throws MPIException
{
    MPI.check();
    return new Intracomm(split(handle, colour, key));
}

private native long split(long comm, int colour, int key) throws MPIException;

/**
 * Create a new communicator.
 * <p>Java binding of the MPI operation {@code MPI_COMM_CREATE}.
 * @param group group which is a subset of the group of this communicator
 * @return new communicator
 * @throws MPIException
 */
public final Intracomm create(Group group) throws MPIException
{
    MPI.check();
    return new Intracomm(create(handle, group.handle));
}

private native long create(long comm, long group);

// Topology Constructors

/**
 * Creates a communicator to which the Cartesian topology
 * information is attached.
 * Create a cartesian topology communicator whose group is a subset
 * of the group of this communicator.
 * <p>Java binding of the MPI operation {@code MPI_CART_CREATE}.
 * <p>The number of dimensions of the Cartesian grid is taken to be the
 * size of the {@code dims} argument. The array {@code periods} must
 * be the same size.
 * @param dims    the number of processes in each dimension
 * @param periods {@code true}  if grid is periodic,
 *                {@code false} if not, in each dimension
 * @param reorder {@code true}  if ranking may be reordered,
 *                {@code false} if not
 * @return new cartesian topology communicator
 * @throws MPIException
 */
public final CartComm createCart(int[] dims, boolean[] periods, boolean reorder)
    throws MPIException
{
    MPI.check();
    return new CartComm(createCart(handle, dims, periods, reorder));
}

private native long createCart(
        long comm, int[] dims, boolean[] periods, boolean reorder)
        throws MPIException;

/**
 * Creates a communicator to which the graph topology information is attached.
 * <p>Java binding of the MPI operation {@code MPI_GRAPH_CREATE}.
 * <p>The number of nodes in the graph, <em>nnodes</em>, is taken
 * to be size of the {@code index} argument.
 * @param index   node degrees
 * @param edges   graph edges
 * @param reorder {@code true} if ranking may be reordered,
 *                {@code false} if not
 * @return new graph topology communicator
 * @throws MPIException
 */
public final GraphComm createGraph(int[] index, int[] edges, boolean reorder)
    throws MPIException
{
    MPI.check();
    return new GraphComm(createGraph(handle, index, edges, reorder));
}

private native long createGraph(
        long comm, int[] index, int[] edges, boolean reorder)
        throws MPIException;

/**
 * Creates a communicator to which the distributed graph topology
 * information is attached.
 * <p>Java binding of the MPI operation {@code MPI_DIST_GRAPH_CREATE}.
 * <p>The number of source nodes is the size of the {@code sources} argument.
 * @param sources      source nodes for which this process specifies edges
 * @param degrees      number of destinations for each source node
 * @param destinations destination nodes for the source nodes
 * @param weights      weights for source to destination edges
 * @param info         hints on optimization and interpretation of weights
 * @param reorder      the process may be reordered (true) or not (false)
 * @return communicator with distributed graph topology
 * @throws MPIException 
 */
public final GraphComm createDistGraph(
        int[] sources, int[] degrees, int[] destinations,
        int[] weights, Info info, boolean reorder)
    throws MPIException
{
    MPI.check();

    return new GraphComm(createDistGraph(
            handle, sources, degrees, destinations,
            weights, info.handle, reorder, true));
}

/**
 * Creates a communicator to which the distributed graph topology
 * information is attached.
 * <p>Java binding of the MPI operation {@code MPI_DIST_GRAPH_CREATE}
 * using {@code MPI_UNWEIGHTED}.
 * <p>The number of source nodes is the size of the {@code sources} argument.
 * @param sources      source nodes for which this process specifies edges
 * @param degrees      number of destinations for each source node
 * @param destinations destination nodes for the source nodes
 * @param info         hints on optimization and interpretation of weights
 * @param reorder      the process may be reordered (true) or not (false)
 * @return communicator with distributed graph topology
 * @throws MPIException 
 */
public final GraphComm createDistGraph(
        int[] sources, int[] degrees, int[] destinations,
        Info info, boolean reorder)
    throws MPIException
{
    MPI.check();

    return new GraphComm(createDistGraph(
            handle, sources, degrees, destinations,
            null, info.handle, reorder, false));
}

private native long createDistGraph(
        long comm, int[] sources, int[] degrees, int[] destinations,
        int[] weights, long info, boolean reorder, boolean weighted)
        throws MPIException;


/**
 * Creates a communicator to which the distributed graph topology
 * information is attached.
 * <p>Java binding of the MPI operation {@code MPI_DIST_GRAPH_CREATE_ADJACENT}.
 * <p>The number of source/destination nodes is the size of the
 * {@code sources}/{@code destinations} argument.
 * @param sources       ranks of processes for which the calling process
 *                      is a destination
 * @param sourceWeights weights of the edges into the calling process
 * @param destinations  ranks of processes for which the calling process
 *                      is a source
 * @param destWeights   weights of the edges out of the calling process
 * @param info          hints on optimization and interpretation of weights
 * @param reorder       the process may be reordered (true) or not (false)
 * @return communicator with distributed graph topology
 * @throws MPIException 
 */
public final GraphComm createDistGraphAdjacent(
        int[] sources, int[] sourceWeights,
        int[] destinations, int[] destWeights, Info info, boolean reorder)
    throws MPIException
{
    MPI.check();

    return new GraphComm(createDistGraphAdjacent(
            handle, sources, sourceWeights, destinations,
            destWeights, info.handle, reorder, true));
}

/**
 * Creates a communicator to which the distributed graph topology
 * information is attached.
 * <p>Java binding of the MPI operation {@code MPI_DIST_GRAPH_CREATE_ADJACENT}
 * using {@code MPI_UNWEIGHTED}.
 * <p>The number of source/destination nodes is the size of the
 * {@code sources}/{@code destinations} argument.
 * @param sources      ranks of processes for which the calling process
 *                     is a destination
 * @param destinations ranks of processes for which the calling process
 *                     is a source
 * @param info         hints on optimization and interpretation of weights
 * @param reorder      the process may be reordered (true) or not (false)
 * @return communicator with distributed graph topology
 * @throws MPIException 
 */
public final GraphComm createDistGraphAdjacent(
        int[] sources, int[] destinations, Info info, boolean reorder)
    throws MPIException
{
    MPI.check();

    return new GraphComm(createDistGraphAdjacent(
            handle, sources, null, destinations, null,
            info.handle, reorder, false));
}

private native long createDistGraphAdjacent(
        long comm, int[] sources, int []sourceweights, int[] destinations,
        int[] distweights, long info, boolean reorder, boolean weighted)
        throws MPIException;


/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_SCAN}.
 * @param sendbuf send buffer array
 * @param recvbuf receive buffer array
 * @param count   number of items in input buffer
 * @param type    data type of each item in input buffer
 * @param op      reduce operation
 * @throws MPIException
 */
public final void scan(Object sendbuf, Object recvbuf,
                       int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();

    int sendoff = 0,
        recvoff = 0;

    boolean sdb = false,
            rdb = false;

    if(sendbuf instanceof Buffer && !(sdb = ((Buffer)sendbuf).isDirect()))
    {
        sendoff = type.getOffset(sendbuf);
        sendbuf = ((Buffer)sendbuf).array();
    }

    if(recvbuf instanceof Buffer && !(rdb = ((Buffer)recvbuf).isDirect()))
    {
        recvoff = type.getOffset(recvbuf);
        recvbuf = ((Buffer)recvbuf).array();
    }

    op.setDatatype(type);

    scan(handle, sendbuf, sdb, sendoff, recvbuf, rdb, recvoff,
         count, type.handle, type.baseType, op, op.handle);
}

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_SCAN}
 * using {@code MPI_IN_PLACE} instead of the send buffer.
 * @param recvbuf receive buffer array
 * @param count   number of items in input buffer
 * @param type    data type of each item in input buffer
 * @param op      reduce operation
 * @throws MPIException
 */
public final void scan(Object recvbuf, int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    int recvoff = 0;
    boolean rdb = false;

    if(recvbuf instanceof Buffer && !(rdb = ((Buffer)recvbuf).isDirect()))
    {
        recvoff = type.getOffset(recvbuf);
        recvbuf = ((Buffer)recvbuf).array();
    }

    op.setDatatype(type);

    scan(handle, null, false, 0, recvbuf, rdb, recvoff,
         count, type.handle, type.baseType, op, op.handle);
}

private native void scan(
        long comm, Object sendbuf, boolean sdb, int sendoff,
        Object recvbuf, boolean rdb, int recvoff, int count,
        long type, int baseType, Op jOp, long hOp) throws MPIException;

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_ISCAN}.
 * @param sendbuf send buffer array
 * @param recvbuf receive buffer array
 * @param count   number of items in input buffer
 * @param type    data type of each item in input buffer
 * @param op      reduce operation
 * @return communication request
 * @throws MPIException
 */
public final Request iScan(Buffer sendbuf, Buffer recvbuf,
                           int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    op.setDatatype(type);
    assertDirectBuffer(sendbuf, recvbuf);

    return new Request(iScan(handle, sendbuf, recvbuf, count,
                             type.handle, type.baseType, op, op.handle));
}

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_ISCAN}
 * using {@code MPI_IN_PLACE} instead of the send buffer.
 * @param buf   send/receive buffer array
 * @param count number of items in buffer
 * @param type  data type of each item in buffer
 * @param op    reduce operation
 * @return communication request
 * @throws MPIException
 */
public final Request iScan(Buffer buf, int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    op.setDatatype(type);
    assertDirectBuffer(buf);

    return new Request(iScan(
            handle, null, buf, count,
            type.handle, type.baseType, op, op.handle));
}

private native long iScan(
        long comm, Buffer sendbuf, Buffer recvbuf, int count,
        long type, int baseType, Op jOp, long hOp) throws MPIException;

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_EXSCAN}.
 * @param sendbuf send buffer array
 * @param recvbuf receive buffer array
 * @param count   number of items in input buffer
 * @param type    data type of each item in input buffer
 * @param op      reduce operation
 * @throws MPIException
 */
public final void exScan(Object sendbuf, Object recvbuf,
                         int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();

    int sendoff = 0,
        recvoff = 0;

    boolean sdb = false,
            rdb = false;

    if(sendbuf instanceof Buffer && !(sdb = ((Buffer)sendbuf).isDirect()))
    {
        sendoff = type.getOffset(sendbuf);
        sendbuf = ((Buffer)sendbuf).array();
    }

    if(recvbuf instanceof Buffer && !(rdb = ((Buffer)recvbuf).isDirect()))
    {
        recvoff = type.getOffset(recvbuf);
        recvbuf = ((Buffer)recvbuf).array();
    }

    op.setDatatype(type);

    exScan(handle, sendbuf, sdb, sendoff, recvbuf, rdb, recvoff,
           count, type.handle, type.baseType, op, op.handle);
}

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_EXSCAN}
 * using {@code MPI_IN_PLACE} instead of the send buffer.
 * @param buf   receive buffer array
 * @param count number of items in input buffer
 * @param type  data type of each item in input buffer
 * @param op    reduce operation
 * @throws MPIException
 */
public final void exScan(Object buf, int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    int off = 0;
    boolean db = false;

    if(buf instanceof Buffer && !(db = ((Buffer)buf).isDirect()))
    {
        off = type.getOffset(buf);
        buf = ((Buffer)buf).array();
    }

    op.setDatatype(type);

    exScan(handle, null, false, 0, buf, db, off, count,
           type.handle, type.baseType, op, op.handle);
}

private native void exScan(
        long comm, Object sendbuf, boolean sdb, int sendoff,
        Object recvbuf, boolean rdb, int recvoff, int count,
        long type, int baseType, Op jOp, long hOp) throws MPIException;

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_IEXSCAN}.
 * @param sendbuf send buffer array
 * @param recvbuf receive buffer array
 * @param count   number of items in input buffer
 * @param type    data type of each item in input buffer
 * @param op      reduce operation
 * @return communication request
 * @throws MPIException
 */
public final Request iExScan(Buffer sendbuf, Buffer recvbuf,
                             int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    op.setDatatype(type);
    assertDirectBuffer(sendbuf, recvbuf);

    return new Request(iExScan(handle, sendbuf, recvbuf, count,
                               type.handle, type.baseType, op, op.handle));
}

/**
 * Perform a prefix reduction on data distributed across the group.
 * <p>Java binding of the MPI operation {@code MPI_IEXSCAN}
 * using {@code MPI_IN_PLACE} instead of the send buffer.
 * @param buf   receive buffer array
 * @param count number of items in input buffer
 * @param type  data type of each item in input buffer
 * @param op    reduce operation
 * @return communication request
 * @throws MPIException
 */
public final Request iExScan(Buffer buf, int count, Datatype type, Op op)
    throws MPIException
{
    MPI.check();
    op.setDatatype(type);
    assertDirectBuffer(buf);

    return new Request(iExScan(
            handle, null, buf, count,
            type.handle, type.baseType, op, op.handle));
}

private native long iExScan(
        long comm, Buffer sendbuf, Buffer recvbuf, int count,
        long type, int baseType, Op jOp, long hOp) throws MPIException;

/**
 * Java binding of {@code MPI_OPEN_PORT} using {@code MPI_INFO_NULL}.
 * @return port name
 * @throws MPIException
 */
public static String openPort() throws MPIException
{
    MPI.check();
    return openPort(Info.NULL);
}

/**
 * Java binding of {@code MPI_OPEN_PORT}.
 * @param info implementation-specific information
 * @return port name
 * @throws MPIException
 */
public static String openPort(Info info) throws MPIException
{
    MPI.check();
    return openPort(info.handle);
}

private native static String openPort(long info) throws MPIException;

/**
 * Java binding of {@code MPI_CLOSE_PORT}.
 * @param name port name
 * @throws MPIException
 */
public static void closePort(String name) throws MPIException
{
    MPI.check();
    closePort_jni(name);
}

private native static void closePort_jni(String name) throws MPIException;

/**
 * Java binding of {@code MPI_COMM_ACCEPT} using {@code MPI_INFO_NULL}.
 * @param port port name
 * @param root rank in comm of root node
 * @return intercommunicator with client as remote group
 */
public final Intercomm accept(String port, int root) throws MPIException
{
    MPI.check();
    return new Intercomm(accept(handle, port, Info.NULL, root));
}

/**
 * Java binding of {@code MPI_COMM_ACCEPT}.
 * @param port port name
 * @param info implementation-specific information
 * @param root rank in comm of root node
 * @return intercommunicator with client as remote group
 * @throws MPIException
 */
public final Intercomm accept(String port, Info info, int root)
    throws MPIException
{
    MPI.check();
    return new Intercomm(accept(handle, port, info.handle, root));
}

private native long accept(long comm, String port, long info, int root)
    throws MPIException;

/**
 * Java binding of {@code MPI_COMM_CONNECT} using {@code MPI_INFO_NULL}.
 * @param port port name
 * @param root rank in comm of root node
 * @return intercommunicator with server as remote group
 * @throws MPIException
 */
public final Intercomm connect(String port, int root) throws MPIException
{
    MPI.check();
    return new Intercomm(connect(handle, port, Info.NULL, root));
}

/**
 * Java binding of {@code MPI_COMM_CONNECT}.
 * @param port port name
 * @param info implementation-specific information
 * @param root rank in comm of root node
 * @return intercommunicator with server as remote group
 * @throws MPIException
 */
public final Intercomm connect(String port, Info info, int root)
    throws MPIException
{
    MPI.check();
    return new Intercomm(connect(handle, port, info.handle, root));
}

private native long connect(long comm, String port, long info, int root)
    throws MPIException;

/**
 * Java binding of {@code MPI_PUBLISH_NAME} using {@code MPI_INFO_NULL}.
 * @param service service name
 * @param port    port name
 * @throws MPIException
 */
public static void publishName(String service, String port)
    throws MPIException
{
    MPI.check();
    publishName(service, Info.NULL, port);
}

/**
 * Java binding of {@code MPI_PUBLISH_NAME}.
 * @param service service name
 * @param info    implementation-specific information
 * @param port    port name
 * @throws MPIException
 */
public static void publishName(String service, Info info, String port)
    throws MPIException
{
    MPI.check();
    publishName(service, info.handle, port);
}

private native static void publishName(String service, long info, String port)
    throws MPIException;

/**
 * Java binding of {@code MPI_UNPUBLISH_NAME} using {@code MPI_INFO_NULL}.
 * @param service service name
 * @param port    port name
 * @throws MPIException
 */
public static void unpublishName(String service, String port)
    throws MPIException
{
    MPI.check();
    unpublishName(service, Info.NULL, port);
}

/**
 * Java binding of {@code MPI_UNPUBLISH_NAME}.
 * @param service service name
 * @param info    implementation-specific information
 * @param port    port name
 * @throws MPIException
 */
public static void unpublishName(String service, Info info, String port)
    throws MPIException
{
    MPI.check();
    unpublishName(service, info.handle, port);
}

private native static void unpublishName(String service, long info, String port)
    throws MPIException;

/**
 * Java binding of {@code MPI_LOOKUP_NAME} using {@code MPI_INFO_NULL}.
 * @param service service name
 * @return port name
 * @throws MPIException
 */
public static String lookupName(String service) throws MPIException
{
    MPI.check();
    return lookupName(service, Info.NULL);
}

/**
 * Java binding of {@code MPI_LOOKUP_NAME}.
 * @param service service name
 * @param info    mplementation-specific information
 * @return port name
 * @throws MPIException
 */
public static String lookupName(String service, Info info) throws MPIException
{
    MPI.check();
    return lookupName(service, info.handle);
}

private native static String lookupName(String service, long info)
    throws MPIException;

/**
 * Java binding of {@code MPI_COMM_SPAWN}.
 * This intracommunicator will contain the group of spawned processes.
 * @param command  name of program to be spawned
 * @param argv     arguments to command; if this parameter is null,
 *                 {@code MPI_ARGV_NULL} will be used.
 * @param maxprocs maximum number of processes to start
 * @param info     info object telling the runtime where
 *                 and how to start the processes
 * @param root     rank of process in which previous arguments are examined
 * @param errcodes one code per process; if this parameter is null,
 *                 {@code MPI_ERRCODES_IGNORE} will be used.
 * @return intercommunicator between original group and the newly spawned group
 * @throws MPIException 
 */
public final Intercomm spawn(String command, String[] argv, int maxprocs,
                             Info info, int root, int[] errcodes)
    throws MPIException
{
    MPI.check();

    return new Intercomm(spawn(handle, command, argv, maxprocs,
                               info.handle, root, errcodes));
}

private native long spawn(long comm, String command, String[] argv,
                          int maxprocs, long info, int root, int[] errcodes)
                          throws MPIException;

/**
 * Java binding of {@code MPI_COMM_SPAWN_MULTIPLE}.
 * This intracommunicator will contain the group of spawned processes.
 * @param commands programs to be executed
 * @param argv     arguments for commands; if this parameter is null,
 *                 {@code MPI_ARGVS_NULL} will be used.
 * @param maxprocs maximum number of processes to start for each command
 * @param info     info objects telling the runtime where
 *                 and how to start the processes
 * @param root     rank of process in which previous arguments are examined
 * @param errcodes one code per process; if this parameter is null,
 *                 {@code MPI_ERRCODES_IGNORE} will be used.
 * @return intercommunicator between original group and the newly spawned group
 * @throws MPIException 
 */
public final Intercomm spawnMultiple(
        String[] commands, String[][] argv, int[] maxprocs,
        Info[] info, int root, int[] errcodes)
    throws MPIException
{
    MPI.check();
    
    long hInfo[] = new long[info.length];
    
    for(int i = 0; i < info.length; i++)
        hInfo[i] = info[i].handle;

    return new Intercomm(spawnMultiple(handle, commands, argv, maxprocs,
                                       hInfo, root, errcodes));
}

private native long spawnMultiple(
        long comm, String[] commands, String[][] argv, int[] maxprocs,
        long[] info, int root, int[] errcodes) throws MPIException;

} // Intracomm
