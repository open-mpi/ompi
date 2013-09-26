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
 * File         : Op.java
 * Author       : Xinying Li, Sang LIm
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

package mpi;

import java.nio.*;

/**
 * This class represents {@code MPI_Op}.
 */
public final class Op implements Freeable
{
private final static int NULL   =  0;
private final static int MAX    =  1;
private final static int MIN    =  2;
private final static int SUM    =  3;
private final static int PROD   =  4;
private final static int LAND   =  5;
private final static int BAND   =  6;
private final static int LOR    =  7;
private final static int BOR    =  8;
private final static int LXOR   =  9;
private final static int BXOR   = 10;
private final static int MINLOC = 11;
private final static int MAXLOC = 12;

private UserFunction uf = null;
private boolean commute;
private Datatype datatype;
private long handle;

static
{
    init();
}

private static native void init();

protected Op(int type)
{
    getOp(type);
    commute = true;
}

private native void getOp(int type);

/**
 * Bind a user-defined global reduction operation to an {@code Op} object.
 * <p>Java binding of the MPI operation {@code MPI_OP_CREATE}.
 * @param function user defined function
 * @param commute  {@code true} if commutative, {@code false} otherwise
 */
public Op(UserFunction function, boolean commute)
{
    handle = 0; // When JNI code gets the handle it will be initialized.
    uf = function;
    this.commute = commute;
}

protected void setDatatype(Datatype t)
{
    datatype = t;
}

protected void call(Object invec, Object inoutvec, int count)
    throws MPIException
{
    if(datatype.baseType == Datatype.BOOLEAN)
    {
        uf.call(invec, inoutvec, count, datatype);
    }
    else
    {
        uf.call(((ByteBuffer)invec).order(ByteOrder.nativeOrder()),
                ((ByteBuffer)inoutvec).order(ByteOrder.nativeOrder()),
                count, datatype);
    }
}

/**
 * Test if the operation is conmutative.
 * <p>Java binding of the MPI operation {@code MPI_OP_COMMUTATIVE}.
 * @return {@code true} if commutative, {@code false} otherwise
 */
public boolean isCommutative()
{
    return commute;
}

/**
 * Java binding of the MPI operation {@code MPI_OP_FREE}.
 * @throws MPIException
 */
@Override public native void free() throws MPIException;

/**
 * Test if operation object is null.
 * @return true if the operation object is null, false otherwise
 */
public native boolean isNull();

} // Op
