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

import java.nio.*;

/**
 * The {@code Datatype} class represents {@code MPI_Datatype} handles.
 */
public final class Datatype implements Freeable
{
protected long handle;
protected int baseType;
protected int baseSize;

// Cache to avoid unnecessary jni calls.
private int lb, extent, trueLb, trueExtent;

protected static final int NULL       =  0;
protected static final int BYTE       =  1;
protected static final int CHAR       =  2;
protected static final int SHORT      =  3;
protected static final int BOOLEAN    =  4;
protected static final int INT        =  5;
protected static final int LONG       =  6;
protected static final int FLOAT      =  7;
protected static final int DOUBLE     =  8;
protected static final int PACKED     =  9;
protected static final int INT2       = 10;
protected static final int SHORT_INT  = 11;
protected static final int LONG_INT   = 12;
protected static final int FLOAT_INT  = 13;
protected static final int DOUBLE_INT = 14;
protected static final int FLOAT_COMPLEX  = 15;
protected static final int DOUBLE_COMPLEX = 16;

static
{
    init();
}

private static native void init();

/*
 * Constructor used in static initializer of 'MPI'.
 *
 * (Called before MPI.Init(), so cannot make any native MPI calls.)
 *
 * (Initialization done in separate 'setBasic', so can create
 * datatype objects for 'BYTE', etc in static initializers invoked before
 * MPI.Init(), then initialize objects after MPI initialized.)
 */
protected Datatype()
{
}

protected void setBasic(int type)
{
    baseType = type;
    handle   = getDatatype(type);
    baseSize = type == NULL ? 0 : getSize(handle);
}

protected void setBasic(int type, Datatype oldType)
{
    baseType = oldType.baseType;
    handle   = getDatatype(type);
    baseSize = oldType.baseSize;
}

private static native long getDatatype(int type);

/*
 * Constructor used in 'create*' methods.
 */
private Datatype(Datatype oldType, long handle)
{
    baseType = oldType.baseType;
    baseSize = oldType.baseSize;
    this.handle = handle;
}

/*
 * Constructor used in 'create*' methods.
 */
private Datatype(int baseType, int baseSize, long handle)
{
    this.baseType = baseType;
    this.baseSize = baseSize;
    this.handle   = handle;
}

/**
 * Returns the lower bound of a datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_GET_EXTENT}.
 * @return lower bound of datatype
 * @throws MPIException
 */
public int getLb() throws MPIException
{
    if(extent == 0)
        getLbExtent();

    return lb;
}

/**
 * Returns the extent of a datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_GET_EXTENT}.
 * @return datatype extent
 * @throws MPIException
 */
public int getExtent() throws MPIException
{
    if(extent == 0)
        getLbExtent();

    return extent;
}

private void getLbExtent() throws MPIException
{
    MPI.check();
    int lbExt[] = new int[2];
    getLbExtent(handle, lbExt);
    lb     = lbExt[0] / baseSize;
    extent = lbExt[1] / baseSize;
}

private native void getLbExtent(long handle, int[] lbExt);

/**
 * Returns the true lower bound of a datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_GET_TRUE_EXTENT}.
 * @return lower bound of datatype
 * @throws MPIException
 */
public int getTrueLb() throws MPIException
{
    if(trueExtent == 0)
        getTrueLbExtent();

    return trueLb;
}

/**
 * Returns the true extent of a datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_GET_TRUE_EXTENT}.
 * @return datatype true extent
 * @throws MPIException
 */
public int getTrueExtent() throws MPIException
{
    if(trueExtent == 0)
        getTrueLbExtent();

    return trueExtent;
}

private void getTrueLbExtent() throws MPIException
{
    MPI.check();
    int lbExt[] = new int[2];
    getTrueLbExtent(handle, lbExt);
    trueLb     = lbExt[0] / baseSize;
    trueExtent = lbExt[1] / baseSize;
}

private native void getTrueLbExtent(long handle, int[] lbExt);

/**
 * Returns the total size of a datatype - the number of buffer
 * elements it represents.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_SIZE}.
 * @return datatype size
 * @throws MPIException
 */
public int getSize() throws MPIException
{
    MPI.check();
    return getSize(handle) / baseSize;
}

private native int getSize(long type);

/**
 * Commits a derived datatype.
 * Java binding of the MPI operation {@code MPI_TYPE_COMMIT}.
 * @throws MPIException
 */
public void commit() throws MPIException
{
    MPI.check();
    commit(handle);
}

private native void commit(long type);

/**
 * Frees the datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_FREE}.
 * @throws MPIException
 */
@Override public void free() throws MPIException
{
    MPI.check();
    handle = free(handle);
}

private native long free(long type) throws MPIException;

/**
 * Returns {@code true} if this datatype is MPI_DATATYPE_NULL.
 * @return {@code true} if this datatype is MPI_DATATYPE_NULL
 */
public boolean isNull()
{
    return handle == MPI.DATATYPE_NULL.handle;
}

/**
 * Java binding of {@code MPI_TYPE_DUP}.
 * <p>It is recommended to use {@link #dup} instead of {@link #clone}
 * because the last can't throw an {@link mpi.MPIException}.
 * @return new datatype
 */
@Override public Datatype clone()
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
 * Java binding of {@code MPI_TYPE_DUP}.
 * @return new datatype
 * @throws MPIException 
 */
public Datatype dup() throws MPIException
{
    MPI.check();
    return new Datatype(this, dup(handle));
}

private native long dup(long type) throws MPIException;

/**
 * Construct new datatype representing replication of old datatype into
 * contiguous locations.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_CONTIGUOUS}.
 * <p>The base type of the new datatype is the same as the base type of
 * {@code oldType}.
 * @param count   replication count
 * @param oldType old datatype
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createContiguous(int count, Datatype oldType)
        throws MPIException
{
    MPI.check();
    return new Datatype(oldType, getContiguous(count, oldType.handle));
}

private static native long getContiguous(int count, long oldType);

/**
 * Construct new datatype representing replication of old datatype into
 * locations that consist of equally spaced blocks.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_VECTOR}.
 * <p>The base type of the new datatype is the same as the base type of
 * {@code oldType}.
 * @param count       number of blocks
 * @param blockLength number of elements in each block
 * @param stride      number of elements between start of each block
 * @param oldType     old datatype
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createVector(int count, int blockLength,
                                    int stride, Datatype oldType)
        throws MPIException
{
    MPI.check();
    long handle = getVector(count, blockLength, stride, oldType.handle);
    return new Datatype(oldType, handle);
}

private static native long getVector(
        int count, int blockLength, int stride, long oldType)
        throws MPIException;

/**
 * Identical to {@code createVector} except that the stride is expressed
 * directly in terms of the buffer index, rather than the units of
 * the old type.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_HVECTOR}.
 * @param count       number of blocks
 * @param blockLength number of elements in each
 * @param stride      number of bytes between start of each block
 * @param oldType     old datatype
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createHVector(int count, int blockLength,
                                     int stride, Datatype oldType)
        throws MPIException
{
    MPI.check();
    long handle = getHVector(count, blockLength, stride, oldType.handle);
    return new Datatype(oldType, handle);
}

private static native long getHVector(
        int count, int blockLength, int stride, long oldType)
        throws MPIException;

/**
 * Construct new datatype representing replication of old datatype into
 * a sequence of blocks where each block can contain a different number
 * of copies and have a different displacement.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_INDEXED}.
 * <p>The number of blocks is taken to be size of the {@code blockLengths}
 * argument. The second argument, {@code displacements}, should be the
 * same size. The base type of the new datatype is the same as the base
 * type of {@code oldType}.
 * @param blockLengths  number of elements per block
 * @param displacements displacement of each block in units of old type
 * @param oldType       old datatype
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createIndexed(int[] blockLengths,
                                     int[] displacements, Datatype oldType)
        throws MPIException
{
    MPI.check();
    long handle = getIndexed(blockLengths, displacements, oldType.handle);
    return new Datatype(oldType, handle);
}

private static native long getIndexed(
        int[] blockLengths, int[] displacements, long oldType)
        throws MPIException;

/**
 * Identical to {@code createIndexed} except that the displacements are
 * expressed directly in terms of the buffer index, rather than the
 * units of the old type.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_HINDEXED}.
 * @param blockLengths  number of elements per block
 * @param displacements byte displacement in buffer for each block
 * @param oldType       old datatype
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createHIndexed(int[] blockLengths,
                                      int[] displacements, Datatype oldType)
        throws MPIException
{
    MPI.check();
    long handle = getHIndexed(blockLengths, displacements, oldType.handle);
    return new Datatype(oldType, handle);
}

private static native long getHIndexed(
        int[] blockLengths, int[] displacements, long oldType)
        throws MPIException;

/**
 * The most general type constructor.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_STRUCT}.
 * <p>The number of blocks is taken to be size of the {@code blockLengths}
 * argument. The second and third arguments, {@code displacements},
 * and {@code types}, should be the same size.
 * @param blockLengths  number of elements in each block
 * @param displacements byte displacement of each block
 * @param types         type of elements in each block
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createStruct(int[] blockLengths,
                                    int[] displacements, Datatype[] types)
        throws MPIException
{
    MPI.check();
    long handle = getStruct(blockLengths, displacements, types);
    return new Datatype(MPI.BYTE, handle);
}

private static native long getStruct(
        int[] blockLengths, int[] displacements, Datatype[] types)
        throws MPIException;

/*
 * JMS add proper documentation here
 * JMS int != Aint!  This needs to be fixed throughout.
 */
/**
 * Create a datatype with a new lower bound and extent from an existing
 * datatype.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_CREATE_RESIZED}.
 * @param oldType input datatype
 * @param lb      new lower bound of datatype (address integer)
 * @param extent  new extent of datatype (address integer)
 * @return new datatype
 * @throws MPIException
 */
public static Datatype createResized(Datatype oldType, int lb, int extent)
        throws MPIException
{
    MPI.check();
    long handle = getResized(oldType.handle, lb, extent);
    return new Datatype(oldType, handle);
}

private static native long getResized(long oldType, int lb, int extent);

/**
 * Sets the print name for the datatype.
 * @param name name for the datatype
 * @throws MPIException
 */
public void setName(String name) throws MPIException
{
    MPI.check();
    setName(handle, name);
}

private native void setName(long handle, String name) throws MPIException;

/**
 * Return the print name from the datatype.
 * @return name of the datatype
 * @throws MPIException
 */
public String getName() throws MPIException
{
    MPI.check();
    return getName(handle);
}

private native String getName(long handle) throws MPIException;

/**
 * Create a new attribute key.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_CREATE_KEYVAL}.
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
 * <p>Java binding of the MPI operation {@code MPI_TYPE_FREE_KEYVAL}.
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
 * <p>Java binding of the MPI operation {@code MPI_TYPE_SET_ATTR}.
 * @param keyval attribute key
 * @param value  attribute value
 * @throws MPIException
 */
public void setAttr(int keyval, Object value) throws MPIException
{
    MPI.check();
    setAttr(handle, keyval, MPI.attrSet(value));
}

private native void setAttr(long type, int keyval, byte[] value)
        throws MPIException;

/**
 * Retrieves attribute value by key.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_GET_ATTR}.
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

private native Object getAttr(long type, int keyval) throws MPIException;

/**
 * Deletes an attribute value associated with a key.
 * <p>Java binding of the MPI operation {@code MPI_TYPE_DELETE_ATTR}.
 * @param keyval attribute key
 * @throws MPIException
 */
public void deleteAttr(int keyval) throws MPIException
{
    MPI.check();
    deleteAttr(handle, keyval);
}

private native void deleteAttr(long type, int keyval) throws MPIException;

/**
 * Gets the offset of a buffer in bytes.
 * @param buffer buffer
 * @return offset in bytes
 */
protected int getOffset(Object buffer)
{
    return baseSize * ((Buffer)buffer).arrayOffset();
}

} // Datatype
