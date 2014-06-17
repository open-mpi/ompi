/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow.
 */
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

import java.io.*;
import java.nio.*;

/**
 * MPI environment.
 */
public final class MPI
{
private static boolean initialized, finalized;
private static byte[] buffer = null; // Buffer allocation
private static final int MAX_PROCESSOR_NAME = 256;
private static final ByteOrder nativeOrder = ByteOrder.nativeOrder();

public static final Intracomm COMM_WORLD, COMM_SELF;

public static final int THREAD_SINGLE, THREAD_FUNNELED, THREAD_SERIALIZED,
                        THREAD_MULTIPLE;

public static final int GRAPH, DIST_GRAPH, CART;
public static final int ANY_SOURCE, ANY_TAG;

public static final Op MAX, MIN, SUM, PROD, LAND, BAND,
                       LOR, BOR, LXOR, BXOR;

/**
 * Global minimum operator.
 * <p>{@code MINLOC} and {@link #MAXLOC} can be used with each of the following
 * datatypes: {@link #INT2}, {@link #SHORT_INT}, {@link #LONG_INT}, 
 * {@link #FLOAT_INT} and {@link #DOUBLE_INT}.
 */
public static final Op MINLOC;

/** Global maximum operator. See {@link #MINLOC}.*/
public static final Op MAXLOC;

public static final Datatype DATATYPE_NULL;

public static final Datatype BYTE, CHAR, SHORT, BOOLEAN,
                             INT, LONG, FLOAT, DOUBLE, PACKED,
                             FLOAT_COMPLEX, DOUBLE_COMPLEX;

/** Struct which must be used with {@link #int2}. */
public static final Datatype INT2;
/** Struct which must be used with {@link #shortInt}. */
public static final Datatype SHORT_INT;
/** Struct which must be used with {@link #longInt}. */
public static final Datatype LONG_INT;
/** Struct which must be used with {@link #floatInt}. */
public static final Datatype FLOAT_INT;
/** Struct which must be used with {@link #doubleInt}. */
public static final Datatype DOUBLE_INT;

/** Struct object for {@link #INT2} datatype. */
public static final Int2 int2;
/** Struct object for {@link #SHORT_INT} datatype. */
public static final ShortInt shortInt;
/** Struct object for {@link #LONG_INT} datatype. */
public static final LongInt longInt;
/** Struct object for {@link #FLOAT_INT} datatype. */
public static final FloatInt floatInt;
/** Struct object for {@link #DOUBLE_INT} datatype. */
public static final DoubleInt doubleInt;

public static final Request REQUEST_NULL;
public static final Group GROUP_EMPTY;
public static final Info INFO_ENV, INFO_NULL;

public static final int PROC_NULL;
public static final int UNDEFINED;
public static final int IDENT, CONGRUENT, SIMILAR, UNEQUAL;
public static final int TAG_UB, HOST, IO, WTIME_IS_GLOBAL;

public static final int APPNUM, LASTUSEDCODE, UNIVERSE_SIZE, WIN_BASE,
                        WIN_SIZE, WIN_DISP_UNIT;

public static final int VERSION, SUBVERSION;
public static final int ROOT, KEYVAL_INVALID, BSEND_OVERHEAD;
public static final int MAX_OBJECT_NAME, MAX_PORT_NAME, MAX_DATAREP_STRING;
public static final int MAX_INFO_KEY, MAX_INFO_VAL;
public static final int ORDER_C, ORDER_FORTRAN;
public static final int DISTRIBUTE_BLOCK, DISTRIBUTE_CYCLIC, DISTRIBUTE_NONE,
                        DISTRIBUTE_DFLT_DARG;

public static final int MODE_CREATE, MODE_RDONLY, MODE_WRONLY, MODE_RDWR,
                        MODE_DELETE_ON_CLOSE, MODE_UNIQUE_OPEN, MODE_EXCL,
                        MODE_APPEND, MODE_SEQUENTIAL;
public static final int DISPLACEMENT_CURRENT;
public static final int SEEK_SET, SEEK_CUR, SEEK_END;

public static final int MODE_NOCHECK, MODE_NOPRECEDE, MODE_NOPUT,
                        MODE_NOSTORE, MODE_NOSUCCEED;
public static final int LOCK_EXCLUSIVE, LOCK_SHARED;

public static final Errhandler ERRORS_ARE_FATAL, ERRORS_RETURN;

// Error classes and codes
public static final int SUCCESS;
public static final int ERR_BUFFER;
public static final int ERR_COUNT;
public static final int ERR_TYPE;
public static final int ERR_TAG;
public static final int ERR_COMM;
public static final int ERR_RANK;
public static final int ERR_REQUEST;
public static final int ERR_ROOT;
public static final int ERR_GROUP;
public static final int ERR_OP;
public static final int ERR_TOPOLOGY;
public static final int ERR_DIMS;
public static final int ERR_ARG;
public static final int ERR_UNKNOWN;
public static final int ERR_TRUNCATE;
public static final int ERR_OTHER;
public static final int ERR_INTERN;
public static final int ERR_IN_STATUS;
public static final int ERR_PENDING;
public static final int ERR_ACCESS;
public static final int ERR_AMODE;
public static final int ERR_ASSERT;
public static final int ERR_BAD_FILE;
public static final int ERR_BASE;
public static final int ERR_CONVERSION;
public static final int ERR_DISP;
public static final int ERR_DUP_DATAREP;
public static final int ERR_FILE_EXISTS;
public static final int ERR_FILE_IN_USE;
public static final int ERR_FILE;
public static final int ERR_INFO_KEY;
public static final int ERR_INFO_NOKEY;
public static final int ERR_INFO_VALUE;
public static final int ERR_INFO;
public static final int ERR_IO;
public static final int ERR_KEYVAL;
public static final int ERR_LOCKTYPE;
public static final int ERR_NAME;
public static final int ERR_NO_MEM;
public static final int ERR_NOT_SAME;
public static final int ERR_NO_SPACE;
public static final int ERR_NO_SUCH_FILE;
public static final int ERR_PORT;
public static final int ERR_QUOTA;
public static final int ERR_READ_ONLY;
public static final int ERR_RMA_CONFLICT;
public static final int ERR_RMA_SYNC;
public static final int ERR_SERVICE;
public static final int ERR_SIZE;
public static final int ERR_SPAWN;
public static final int ERR_UNSUPPORTED_DATAREP;
public static final int ERR_UNSUPPORTED_OPERATION;
public static final int ERR_WIN;
public static final int ERR_LASTCODE;
public static final int ERR_SYSRESOURCE;

static
{
    System.loadLibrary("mpi_java");

    DATATYPE_NULL = new Datatype();

    BYTE    = new Datatype();
    CHAR    = new Datatype();
    SHORT   = new Datatype();
    BOOLEAN = new Datatype();
    INT     = new Datatype();
    LONG    = new Datatype();
    FLOAT   = new Datatype();
    DOUBLE  = new Datatype();
    PACKED  = new Datatype();
    INT2    = new Datatype();

    SHORT_INT  = new Datatype();
    LONG_INT   = new Datatype();
    FLOAT_INT  = new Datatype();
    DOUBLE_INT = new Datatype();
    FLOAT_COMPLEX  = new Datatype();
    DOUBLE_COMPLEX = new Datatype();

    int2      = newInt2();
    shortInt  = newShortInt();
    longInt   = newLongInt();
    floatInt  = newFloatInt();
    doubleInt = newDoubleInt();

    MAX    = new Op(1);
    MIN    = new Op(2);
    SUM    = new Op(3);
    PROD   = new Op(4);
    LAND   = new Op(5);
    BAND   = new Op(6);
    LOR    = new Op(7);
    BOR    = new Op(8);
    LXOR   = new Op(9);
    BXOR   = new Op(10);
    MINLOC = new Op(11);
    MAXLOC = new Op(12);

    GROUP_EMPTY  = new Group(Group.getEmpty());
    REQUEST_NULL = new Request(Request.getNull());
    INFO_ENV     = Info.newEnv();
    INFO_NULL    = new Info(Info.NULL);

    Constant c = new Constant();

    THREAD_SINGLE     = c.THREAD_SINGLE;
    THREAD_FUNNELED   = c.THREAD_FUNNELED;
    THREAD_SERIALIZED = c.THREAD_SERIALIZED;
    THREAD_MULTIPLE   = c.THREAD_MULTIPLE;

    GRAPH      = c.GRAPH;
    DIST_GRAPH = c.DIST_GRAPH;
    CART       = c.CART;

    ANY_SOURCE = c.ANY_SOURCE;
    ANY_TAG    = c.ANY_TAG;
    PROC_NULL  = c.PROC_NULL;

    UNDEFINED = c.UNDEFINED;

    IDENT     = c.IDENT;
    CONGRUENT = c.CONGRUENT;
    SIMILAR   = c.SIMILAR;
    UNEQUAL   = c.UNEQUAL;

    TAG_UB          = c.TAG_UB;
    HOST            = c.HOST;
    IO              = c.IO;
    WTIME_IS_GLOBAL = c.WTIME_IS_GLOBAL;

    APPNUM        = c.APPNUM;
    LASTUSEDCODE  = c.LASTUSEDCODE;
    UNIVERSE_SIZE = c.UNIVERSE_SIZE;
    WIN_BASE      = c.WIN_BASE;
    WIN_SIZE      = c.WIN_SIZE;
    WIN_DISP_UNIT = c.WIN_DISP_UNIT;

    VERSION    = c.VERSION;
    SUBVERSION = c.SUBVERSION;

    ROOT           = c.ROOT;
    KEYVAL_INVALID = c.KEYVAL_INVALID;
    BSEND_OVERHEAD = c.BSEND_OVERHEAD;

    MAX_OBJECT_NAME    = c.MAX_OBJECT_NAME;
    MAX_PORT_NAME      = c.MAX_PORT_NAME;
    MAX_DATAREP_STRING = c.MAX_DATAREP_STRING;

    MAX_INFO_KEY = c.MAX_INFO_KEY;
    MAX_INFO_VAL = c.MAX_INFO_VAL;

    ORDER_C       = c.ORDER_C;
    ORDER_FORTRAN = c.ORDER_FORTRAN;

    DISTRIBUTE_BLOCK     = c.DISTRIBUTE_BLOCK;
    DISTRIBUTE_CYCLIC    = c.DISTRIBUTE_CYCLIC;
    DISTRIBUTE_NONE      = c.DISTRIBUTE_NONE;
    DISTRIBUTE_DFLT_DARG = c.DISTRIBUTE_DFLT_DARG;

    MODE_CREATE          = c.MODE_CREATE;
    MODE_RDONLY          = c.MODE_RDONLY;
    MODE_WRONLY          = c.MODE_WRONLY;
    MODE_RDWR            = c.MODE_RDWR;
    MODE_DELETE_ON_CLOSE = c.MODE_DELETE_ON_CLOSE;
    MODE_UNIQUE_OPEN     = c.MODE_UNIQUE_OPEN;
    MODE_EXCL            = c.MODE_EXCL;
    MODE_APPEND          = c.MODE_APPEND;
    MODE_SEQUENTIAL      = c.MODE_SEQUENTIAL;

    DISPLACEMENT_CURRENT = c.DISPLACEMENT_CURRENT;

    SEEK_SET = c.SEEK_SET;
    SEEK_CUR = c.SEEK_CUR;
    SEEK_END = c.SEEK_END;

    MODE_NOCHECK   = c.MODE_NOCHECK;
    MODE_NOPRECEDE = c.MODE_NOPRECEDE;
    MODE_NOPUT     = c.MODE_NOPUT;
    MODE_NOSTORE   = c.MODE_NOSTORE;
    MODE_NOSUCCEED = c.MODE_NOSUCCEED;
    LOCK_EXCLUSIVE = c.LOCK_EXCLUSIVE;
    LOCK_SHARED    = c.LOCK_SHARED;

    ERRORS_ARE_FATAL = new Errhandler(Errhandler.getFatal());
    ERRORS_RETURN    = new Errhandler(Errhandler.getReturn());

    COMM_WORLD = new Intracomm();
    COMM_SELF  = new Intracomm();

    // Error classes and codes
    SUCCESS          = c.SUCCESS;
    ERR_BUFFER       = c.ERR_BUFFER;
    ERR_COUNT        = c.ERR_COUNT;
    ERR_TYPE         = c.ERR_TYPE;
    ERR_TAG          = c.ERR_TAG;
    ERR_COMM         = c.ERR_COMM;
    ERR_RANK         = c.ERR_RANK;
    ERR_REQUEST      = c.ERR_REQUEST;
    ERR_ROOT         = c.ERR_ROOT;
    ERR_GROUP        = c.ERR_GROUP;
    ERR_OP           = c.ERR_OP;
    ERR_TOPOLOGY     = c.ERR_TOPOLOGY;
    ERR_DIMS         = c.ERR_DIMS;
    ERR_ARG          = c.ERR_ARG;
    ERR_UNKNOWN      = c.ERR_UNKNOWN;
    ERR_TRUNCATE     = c.ERR_TRUNCATE;
    ERR_OTHER        = c.ERR_OTHER;
    ERR_INTERN       = c.ERR_INTERN;
    ERR_IN_STATUS    = c.ERR_IN_STATUS;
    ERR_PENDING      = c.ERR_PENDING;
    ERR_ACCESS       = c.ERR_ACCESS;
    ERR_AMODE        = c.ERR_AMODE;
    ERR_ASSERT       = c.ERR_ASSERT;
    ERR_BAD_FILE     = c.ERR_BAD_FILE;
    ERR_BASE         = c.ERR_BASE;
    ERR_CONVERSION   = c.ERR_CONVERSION;
    ERR_DISP         = c.ERR_DISP;
    ERR_DUP_DATAREP  = c.ERR_DUP_DATAREP;
    ERR_FILE_EXISTS  = c.ERR_FILE_EXISTS;
    ERR_FILE_IN_USE  = c.ERR_FILE_IN_USE;
    ERR_FILE         = c.ERR_FILE;
    ERR_INFO_KEY     = c.ERR_INFO_KEY;
    ERR_INFO_NOKEY   = c.ERR_INFO_NOKEY;
    ERR_INFO_VALUE   = c.ERR_INFO_VALUE;
    ERR_INFO         = c.ERR_INFO;
    ERR_IO           = c.ERR_IO;
    ERR_KEYVAL       = c.ERR_KEYVAL;
    ERR_LOCKTYPE     = c.ERR_LOCKTYPE;
    ERR_NAME         = c.ERR_NAME;
    ERR_NO_MEM       = c.ERR_NO_MEM;
    ERR_NOT_SAME     = c.ERR_NOT_SAME;
    ERR_NO_SPACE     = c.ERR_NO_SPACE;
    ERR_NO_SUCH_FILE = c.ERR_NO_SUCH_FILE;
    ERR_PORT         = c.ERR_PORT;
    ERR_QUOTA        = c.ERR_QUOTA;
    ERR_READ_ONLY    = c.ERR_READ_ONLY;
    ERR_RMA_CONFLICT = c.ERR_RMA_CONFLICT;
    ERR_RMA_SYNC     = c.ERR_RMA_SYNC;
    ERR_SERVICE      = c.ERR_SERVICE;
    ERR_SIZE         = c.ERR_SIZE;
    ERR_SPAWN        = c.ERR_SPAWN;
    ERR_UNSUPPORTED_DATAREP   = c.ERR_UNSUPPORTED_DATAREP;
    ERR_UNSUPPORTED_OPERATION = c.ERR_UNSUPPORTED_OPERATION;
    ERR_WIN          = c.ERR_WIN;
    ERR_LASTCODE     = c.ERR_LASTCODE;
    ERR_SYSRESOURCE  = c.ERR_SYSRESOURCE;
}

private static native Int2      newInt2();
private static native ShortInt  newShortInt();
private static native LongInt   newLongInt();
private static native FloatInt  newFloatInt();
private static native DoubleInt newDoubleInt();

private static void initCommon() throws MPIException
{
    initialized = true;

    DATATYPE_NULL.setBasic(Datatype.NULL);

    BYTE.setBasic(Datatype.BYTE);
    CHAR.setBasic(Datatype.CHAR);
    SHORT.setBasic(Datatype.SHORT);
    BOOLEAN.setBasic(Datatype.BOOLEAN);
    INT.setBasic(Datatype.INT);
    LONG.setBasic(Datatype.LONG);
    FLOAT.setBasic(Datatype.FLOAT);
    DOUBLE.setBasic(Datatype.DOUBLE);
    PACKED.setBasic(Datatype.PACKED);

    INT2.setBasic(Datatype.INT2, MPI.BYTE);
    SHORT_INT.setBasic(Datatype.SHORT_INT, MPI.BYTE);
    LONG_INT.setBasic(Datatype.LONG_INT, MPI.BYTE);
    FLOAT_INT.setBasic(Datatype.FLOAT_INT, MPI.BYTE);
    DOUBLE_INT.setBasic(Datatype.DOUBLE_INT, MPI.BYTE);
    FLOAT_COMPLEX.setBasic(Datatype.FLOAT_COMPLEX, MPI.FLOAT);
    DOUBLE_COMPLEX.setBasic(Datatype.DOUBLE_COMPLEX, MPI.DOUBLE);

    COMM_WORLD.setType(Intracomm.WORLD);
    COMM_SELF.setType(Intracomm.SELF);
}

/**
 * Initialize MPI.
 * <p>Java binding of the MPI operation {@code MPI_INIT}.
 * @param args arguments to the {@code main} method.
 * @return arguments
 * @throws MPIException
 */
public static String[] Init(String[] args) throws MPIException
{
    if(initialized)
        throw new MPIException("MPI is already initialized.");

    String[] newArgs = Init_jni(args);
    initCommon();
    return newArgs;
}

private static native String [] Init_jni(String[] args);

/**
 * Initialize MPI with threads.
 * <p>Java binding of the MPI operation {@code MPI_INIT_THREAD}.
 * @param args     arguments to the {@code main} method.
 * @param required desired level of thread support
 * @return provided level of thread support
 * @throws MPIException 
 */
public static int InitThread(String[] args, int required) throws MPIException
{
    if(initialized)
        throw new MPIException("MPI is already initialized.");

    int provided = InitThread_jni(args, required);
    initCommon();
    return provided;
}

private static native int InitThread_jni(String[] args, int required)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_QUERY_THREAD}.
 * @return provided level of thread support
 * @throws MPIException 
 */
public static int queryThread() throws MPIException
{
    MPI.check();
    return queryThread_jni();
}

private static native int queryThread_jni() throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_IS_THREAD_MAIN}.
 * @return true if it is the main thread
 * @throws MPIException 
 */
public static boolean isThreadMain() throws MPIException
{
    MPI.check();
    return isThreadMain_jni();
}

private static native boolean isThreadMain_jni() throws MPIException;

/**
 * Finalize MPI.
 * <p>Java binding of the MPI operation {@code MPI_FINALIZE}.
 * @throws MPIException
 */
public static void Finalize() throws MPIException
{
    check();
    Finalize_jni();
    finalized = true;
}

private static native void Finalize_jni() throws MPIException;

/**
 * Returns an elapsed time on the calling processor.
 * <p>Java binding of the MPI operation {@code MPI_WTIME}.
 * @return time in seconds since an arbitrary time in the past.
 * @throws MPIException
 */
public static double wtime() throws MPIException
{
    check();
    return wtime_jni();
}

private static native double wtime_jni();

/**
 * Returns resolution of timer.
 * <p>Java binding of the MPI operation {MPI_WTICK}.
 * @return resolution of {@code wtime} in seconds.
 * @throws MPIException
 */
public static double wtick() throws MPIException
{
    check();
    return wtick_jni();
}

private static native double wtick_jni();

/**
 * Returns the name of the processor on which it is called.
 * <p>Java binding of the MPI operation {@code MPI_GET_PROCESSOR_NAME}.
 * @return A unique specifier for the actual node.
 * @throws MPIException
 */
static public String getProcessorName() throws MPIException
{
    check();
    byte[] buf = new byte[MAX_PROCESSOR_NAME];
    int lengh = getProcessorName(buf);
    return new String(buf,0,lengh);
}

static private native int getProcessorName(byte[] buf);

/**
 * Test if MPI has been initialized.
 * <p>Java binding of the MPI operation {@code MPI_INITIALIZED}.
 * @return {@code true} if {@code Init} has been called,
 *         {@code false} otherwise.
 * @throws MPIException
 */
static public native boolean isInitialized() throws MPIException;

/**
 * Test if MPI has been finalized.
 * <p>Java binding of the MPI operation {@code MPI_FINALIZED}.
 * @return {@code true} if {@code Finalize} has been called,
 *         {@code false} otherwise.
 * @throws MPIException
 */
static public native boolean isFinalized() throws MPIException;

/**
 * Attaches a user-provided buffer for sending.
 * <p>Java binding of the MPI operation {@code MPI_BUFFER_ATTACH}.
 * @param buffer initial buffer
 * @throws MPIException
 */
static public void attachBuffer(byte[] buffer) throws MPIException
{
    check();
    MPI.buffer = buffer;
    attachBuffer_jni(buffer);
}

static private native void attachBuffer_jni(byte[] buffer);

/**
 * Removes an existing buffer (for use in sending).
 * <p>Java binding of the MPI operation {@code MPI_BUFFER_DETACH}.
 * @return initial buffer
 * @throws MPIException
 */
static public byte[] detachBuffer() throws MPIException
{
    check();
    detachBuffer_jni(buffer);
    byte[] result = MPI.buffer;
    MPI.buffer = null;
    return result;
}

static private native void detachBuffer_jni(byte[] buffer);

/**
 * Controls profiling.
 * <p>This method is not implemented.
 * <p>Java binding of the MPI operation {@code MPI_PCONTROL}.
 * @param level Profiling level.
 * @param obj   Profiling information.
 */
public static void pControl(int level, Object obj)
{
    // Nothing to do here.
}

/**
 * Check if MPI has been initialized and hasn't been finalized.
 * @throws MPIException
 */
protected static void check() throws MPIException
{
    if(!initialized)
        throw new MPIException("MPI is not initialized.");

    if(finalized)
        throw new MPIException("MPI is finalized.");
}

protected static byte[] attrSet(Object value) throws MPIException
{
    try
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream os = new ObjectOutputStream(baos);
        os.writeObject(value);
        os.close();
        return baos.toByteArray();
    }
    catch(IOException ex)
    {
        MPIException mpiex = new MPIException(ex);
        mpiex.setStackTrace(ex.getStackTrace());
        throw mpiex;
    }
}

protected static Object attrGet(byte[] value) throws MPIException
{
    if(value == null)
        return null;

    try
    {
        ByteArrayInputStream bais = new ByteArrayInputStream(value);
        ObjectInputStream is = new ObjectInputStream(bais);
        Object obj = is.readObject();
        is.close();
        return obj;
    }
    catch(ClassNotFoundException ex)
    {
        throw new MPIException(ex);
    }
    catch(IOException ex)
    {
        throw new MPIException(ex);
    }
}

/**
 * Allocates a new direct byte buffer.
 * @param capacity The new buffer's capacity, in bytes
 * @return The new byte buffer
 */
public static ByteBuffer newByteBuffer(int capacity)
{
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity);
    buf.order(nativeOrder);
    return buf;
}

/**
 * Allocates a new direct char buffer.
 * @param capacity The new buffer's capacity, in chars
 * @return The new char buffer
 */
public static CharBuffer newCharBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 2;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 2);
    buf.order(nativeOrder);
    return buf.asCharBuffer();
}

/**
 * Allocates a new direct short buffer.
 * @param capacity The new buffer's capacity, in shorts
 * @return The new short buffer
 */
public static ShortBuffer newShortBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 2;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 2);
    buf.order(nativeOrder);
    return buf.asShortBuffer();
}

/**
 * Allocates a new direct int buffer.
 * @param capacity The new buffer's capacity, in ints
 * @return The new int buffer
 */
public static IntBuffer newIntBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 4;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 4);
    buf.order(nativeOrder);
    return buf.asIntBuffer();
}

/**
 * Allocates a new direct long buffer.
 * @param capacity The new buffer's capacity, in longs
 * @return The new long buffer
 */
public static LongBuffer newLongBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 8;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 8);
    buf.order(nativeOrder);
    return buf.asLongBuffer();
}

/**
 * Allocates a new direct float buffer.
 * @param capacity The new buffer's capacity, in floats
 * @return The new float buffer
 */
public static FloatBuffer newFloatBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 4;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 4);
    buf.order(nativeOrder);
    return buf.asFloatBuffer();
}

/**
 * Allocates a new direct double buffer.
 * @param capacity The new buffer's capacity, in doubles
 * @return The new double buffer
 */
public static DoubleBuffer newDoubleBuffer(int capacity)
{
    assert capacity <= Integer.MAX_VALUE / 8;
    ByteBuffer buf = ByteBuffer.allocateDirect(capacity * 8);
    buf.order(nativeOrder);
    return buf.asDoubleBuffer();
}

/**
 * Asserts that a buffer is direct.
 * @param buf buffer
 */
protected static void assertDirectBuffer(Buffer buf)
{
    if(!buf.isDirect())
        throw new IllegalArgumentException("The buffer must be direct.");
}

/**
 * Asserts that buffers are direct.
 * @param sendbuf
 * @param recvbuf 
 */
protected static void assertDirectBuffer(Buffer sendbuf, Buffer recvbuf)
{
    if(!sendbuf.isDirect())
        throw new IllegalArgumentException("The send buffer must be direct.");

    if(!recvbuf.isDirect())
        throw new IllegalArgumentException("The recv. buffer must be direct.");
}

/**
 * Checks if an object is a direct buffer.
 * @param obj object
 * @return true if the object is a direct buffer
 */
protected static boolean isDirectBuffer(Object obj)
{
    return obj instanceof Buffer && ((Buffer)obj).isDirect();
}

/**
 * Checks if an object is a heap buffer.
 * @param obj object
 * @return true if the object is a heap buffer
 */
protected static boolean isHeapBuffer(Object obj)
{
    return obj instanceof Buffer && !((Buffer)obj).isDirect();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static ByteBuffer slice(ByteBuffer buf, int offset)
{
    return ((ByteBuffer)buf.clear().position(offset))
            .slice().order(nativeOrder);
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static CharBuffer slice(CharBuffer buf, int offset)
{
    return ((CharBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static ShortBuffer slice(ShortBuffer buf, int offset)
{
    return ((ShortBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static IntBuffer slice(IntBuffer buf, int offset)
{
    return ((IntBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static LongBuffer slice(LongBuffer buf, int offset)
{
    return ((LongBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static FloatBuffer slice(FloatBuffer buf, int offset)
{
    return ((FloatBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static DoubleBuffer slice(DoubleBuffer buf, int offset)
{
    return ((DoubleBuffer)buf.clear().position(offset)).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static ByteBuffer slice(byte[] buf, int offset)
{
    return ByteBuffer.wrap(buf, offset, buf.length - offset)
                     .slice().order(nativeOrder);
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static CharBuffer slice(char[] buf, int offset)
{
    return CharBuffer.wrap(buf, offset, buf.length - offset).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static ShortBuffer slice(short[] buf, int offset)
{
    return ShortBuffer.wrap(buf, offset, buf.length - offset).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static IntBuffer slice(int[] buf, int offset)
{
    return IntBuffer.wrap(buf, offset, buf.length - offset).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static LongBuffer slice(long[] buf, int offset)
{
    return LongBuffer.wrap(buf, offset, buf.length - offset).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static FloatBuffer slice(float[] buf, int offset)
{
    return FloatBuffer.wrap(buf, offset, buf.length - offset).slice();
}

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @return the new buffer.
 */
public static DoubleBuffer slice(double[] buf, int offset)
{
    return DoubleBuffer.wrap(buf, offset, buf.length - offset).slice();
}

} // MPI
