package mpi;

import java.nio.*;

/**
 * This class represents {@code MPI_Win}.
 */
public final class Win implements Freeable
{
private long handle;

/**
 * Java binding of {@code MPI_WIN_CREATE}.
 * @param base     initial address of window
 * @param size     size of window (buffer elements)
 * @param dispUnit local unit size for displacements (buffer elements)
 * @param info     info object
 * @param comm     communicator
 * @throws MPIException 
 */
public Win(Buffer base, int size, int dispUnit, Info info, Comm comm)
    throws MPIException
{
    if(!base.isDirect())
        throw new IllegalArgumentException("The buffer must be direct.");

    int baseSize;

    if(base instanceof ByteBuffer)
        baseSize = 1;
    else if(base instanceof CharBuffer || base instanceof ShortBuffer)
        baseSize = 2;
    else if(base instanceof IntBuffer || base instanceof FloatBuffer)
        baseSize = 4;
    else if(base instanceof LongBuffer || base instanceof DoubleBuffer)
        baseSize = 8;
    else
        throw new AssertionError();

    int sizeBytes = size * baseSize,
        dispBytes = dispUnit * baseSize;

    handle = createWin(base, sizeBytes, dispBytes, info.handle, comm.handle);
}

private native long createWin(
        Buffer base, int size, int dispUnit, long info, long comm)
        throws MPIException;

private int getBaseType(Datatype orgType, Datatype targetType)
{
    int baseType = orgType.baseType;

    if(baseType != targetType.baseType)
    {
        throw new IllegalArgumentException(
            "Both datatype arguments must be constructed "+
            "from the same predefined datatype.");
    }

    return baseType;
}

/**
 * Java binding of the MPI operation {@code MPI_GET_GROUP}.
 * @return group of processes which share access to the window
 * @throws MPIException 
 */
public Group getGroup() throws MPIException
{
    MPI.check();
    return new Group(getGroup(handle));
}

private native long getGroup(long win) throws MPIException;

/**
 * Java binding of {@code MPI_PUT}.
 * @param origin      origin buffer
 * @param orgCount    number of entries in origin buffer
 * @param orgType     datatype of each entry in origin buffer
 * @param targetRank  rank of target
 * @param targetDisp  displacement from start of window to target buffer
 * @param targetCount number of entries in target buffer
 * @param targetType  datatype of each entry in target buffer
 * @throws MPIException
 */
public void put(Buffer origin, int orgCount, Datatype orgType,
                int targetRank, int targetDisp, int targetCount,
                Datatype targetType)
    throws MPIException
{
    MPI.check();

    if(!origin.isDirect())
        throw new IllegalArgumentException("The origin must be direct buffer.");

    put(handle, origin, orgCount, orgType.handle,
        targetRank, targetDisp, targetCount, targetType.handle,
        getBaseType(orgType, targetType));
}

private native void put(
        long win, Buffer origin, int orgCount, long orgType,
        int targetRank, int targetDisp, int targetCount, long targetType,
        int baseType) throws MPIException;

/**
 * Java binding of {@code MPI_GET}.
 * @param origin      origin buffer
 * @param orgCount    number of entries in origin buffer
 * @param orgType     datatype of each entry in origin buffer
 * @param targetRank  rank of target
 * @param targetDisp  displacement from start of window to target buffer
 * @param targetCount number of entries in target buffer
 * @param targetType  datatype of each entry in target buffer
 */
public void get(Buffer origin, int orgCount, Datatype orgType,
                int targetRank, int targetDisp, int targetCount,
                Datatype targetType)
    throws MPIException
{
    MPI.check();

    if(!origin.isDirect())
        throw new IllegalArgumentException("The origin must be direct buffer.");

    get(handle, origin, orgCount, orgType.handle,
        targetRank, targetDisp, targetCount, targetType.handle,
        getBaseType(orgType, targetType));
}

private native void get(
        long win, Buffer origin, int orgCount, long orgType,
        int targetRank, int targetDisp, int targetCount, long targetType,
        int baseType) throws MPIException;

/**
 * Java binding of {@code MPI_ACCUMULATE}.
 * @param origin      origin buffer
 * @param orgCount    number of entries in origin buffer
 * @param orgType     datatype of each entry in origin buffer
 * @param targetRank  rank of target
 * @param targetDisp  displacement from start of window to target buffer
 * @param targetCount number of entries in target buffer
 * @param targetType  datatype of each entry in target buffer
 * @param op          reduce operation
 */
public void accumulate(Buffer origin, int orgCount, Datatype orgType,
                       int targetRank, int targetDisp, int targetCount,
                       Datatype targetType, Op op)
    throws MPIException
{
    MPI.check();

    if(!origin.isDirect())
        throw new IllegalArgumentException("The origin must be direct buffer.");

    accumulate(handle, origin, orgCount, orgType.handle,
               targetRank, targetDisp, targetCount, targetType.handle,
               op, op.handle, getBaseType(orgType, targetType));
}

private native void accumulate(
        long win, Buffer origin, int orgCount, long orgType,
        int targetRank, int targetDisp, int targetCount, long targetType,
        Op jOp, long hOp, int baseType) throws MPIException;

/**
 * Java binding of {@code MPI_WIN_FENCE}.
 * @param assertion program assertion
 */
public void fence(int assertion) throws MPIException
{
    MPI.check();
    fence(handle, assertion);
}

private native void fence(long win, int assertion) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_START}.
 * @param group     group of target processes
 * @param assertion program assertion
 * @throws MPIException 
 */
public void start(Group group, int assertion) throws MPIException
{
    MPI.check();
    start(handle, group.handle, assertion);
}

private native void start(long win, long group, int assertion)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_COMPLETE}.
 * @throws MPIException 
 */
public void complete() throws MPIException
{
    MPI.check();
    complete(handle);
}

private native void complete(long win) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_POST}.
 * @param group     group of origin processes
 * @param assertion program assertion
 * @throws MPIException 
 */
public void post(Group group, int assertion) throws MPIException
{
    MPI.check();
    post(handle, group.handle, assertion);
}

private native void post(long win, long group, int assertion)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_WAIT}.
 * @throws MPIException 
 */
public void waitFor() throws MPIException
{
    MPI.check();
    waitFor(handle);
}

private native void waitFor(long win) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_TEST}.
 * @return true if success
 * @throws MPIException 
 */
public boolean test() throws MPIException
{
    MPI.check();
    return test(handle);
}

private native boolean test(long win) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_LOCK}.
 * @param lockType  either MPI.LOCK_EXCLUSIVE or MPI.LOCK_SHARED
 * @param rank      rank of locked window
 * @param assertion program assertion
 * @throws MPIException 
 */
public void lock(int lockType, int rank, int assertion) throws MPIException
{
    MPI.check();
    lock(handle, lockType, rank, assertion);
}

private native void lock(long win, int lockType, int rank, int assertion)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_UNLOCK}.
 * @param rank rank of window
 * @throws MPIException 
 */
public void unlock(int rank) throws MPIException
{
    MPI.check();
    unlock(handle, rank);
}

private native void unlock(long win, int rank) throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_SET_ERRHANDLER}.
 * @param errhandler new MPI error handler for window
 * @throws MPIException
 */
public void setErrhandler(Errhandler errhandler) throws MPIException
{
    MPI.check();
    setErrhandler(handle, errhandler.handle);
}

private native void setErrhandler(long win, long errhandler)
        throws MPIException;

/**
 * Java binding of the MPI operation {@code MPI_WIN_CALL_ERRHANDLER}.
 * @param errorCode error code
 * @throws MPIException 
 */
public void callErrhandler(int errorCode) throws MPIException
{
    callErrhandler(handle, errorCode);
}

private native void callErrhandler(long handle, int errorCode)
        throws MPIException;

/**
 * Create a new attribute key.
 * <p>Java binding of the MPI operation {@code MPI_WIN_CREATE_KEYVAL}.
 * @return attribute key for future access
 * @throws MPIException
 */
public static int createKeyval() throws MPIException
{
    MPI.check();
    return createKeyval_jni();
}

private static native int createKeyval_jni() throws MPIException;

/**
 * Frees an attribute key.
 * <p>Java binding of the MPI operation {@code MPI_WIN_FREE_KEYVAL}.
 * @param keyval attribute key
 * @throws MPIException
 */
public static void freeKeyval(int keyval) throws MPIException
{
    MPI.check();
    freeKeyval_jni(keyval);
}

private static native void freeKeyval_jni(int keyval) throws MPIException;

/**
 * Stores attribute value associated with a key.
 * <p>Java binding of the MPI operation {@code MPI_WIN_SET_ATTR}.
 * @param keyval attribute key
 * @param value  attribute value
 * @throws MPIException
 */
public void setAttr(int keyval, Object value) throws MPIException
{
    MPI.check();
    setAttr(handle, keyval, MPI.attrSet(value));
}

private native void setAttr(long win, int keyval, byte[] value)
        throws MPIException;

/**
 * Retrieves attribute value by key.
 * <p>Java binding of the MPI operation {@code MPI_WIN_GET_ATTR}.
 * @param keyval attribute key
 * @return attribute value or null if no attribute is associated with the key.
 * @throws MPIException
 */
public Object getAttr(int keyval) throws MPIException
{
    MPI.check();
    Object obj = getAttr(handle, keyval);
    return obj instanceof byte[] ? MPI.attrGet((byte[])obj) : obj;
}

private native Object getAttr(long win, int keyval) throws MPIException;

/**
 * Deletes an attribute value associated with a key.
 * <p>Java binding of the MPI operation {@code MPI_WIN_DELETE_ATTR}.
 * @param keyval attribute key
 * @throws MPIException
 */
public void deleteAttr(int keyval) throws MPIException
{
    MPI.check();
    deleteAttr(handle, keyval);
}

private native void deleteAttr(long win, int keyval) throws MPIException;

/**
 * Java binding of {@code MPI_WIN_FREE}.
 * @throws MPIException 
 */
@Override public void free() throws MPIException
{
    MPI.check();
    handle = free(handle);
}

private native long free(long win) throws MPIException;

} // Win
