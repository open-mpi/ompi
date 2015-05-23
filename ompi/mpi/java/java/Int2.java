/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

package mpi;

/**
 * Struct class for {@link MPI#INT2} datatype.
 */
public final class Int2 extends Struct
{
private final int iOff, iSize;

/**
 * The struct object will be created only in MPI class.
 * @see MPI#int2
 */
protected Int2(int intOff, int intSize)
{
    iSize = intSize;
    int off = addIntField();
    assert off == 0;
    setOffset(intOff);
    iOff = addIntField();
    assert intOff == iOff;
}

private int addIntField()
{
    switch(iSize)
    {
        case 4: return addInt();
        case 8: return addLong();
        default: throw new AssertionError("Unsupported int size: "+ iSize);
    }
}

/**
 * Creates a Data object.
 * @return new Data object.
 */
@Override protected Int2.Data newData()
{
    return new Int2.Data();
}

/**
 * Class for reading/writing data in a struct stored in a byte buffer.
 */
public final class Data extends Struct.Data
{
    /**
     * Gets the first int.
     * @return first int
     */
    public int getValue()
    {
        return get(0);
    }

    /**
     * Gets the second int.
     * @return second int
     */
    public int getIndex()
    {
        return get(iOff);
    }

    /**
     * Puts the first int.
     * @param v first value
     */
    public void putValue(int v)
    {
        put(0, v);
    }

    /**
     * Puts the second int.
     * @param v second int
     */
    public void putIndex(int v)
    {
        put(iOff, v);
    }

    private int get(int off)
    {
        switch(iSize)
        {
            case 4: return getInt(off);
            case 8: return (int)getLong(off);
            default: throw new AssertionError();
        }
    }

    private void put(int off, int v)
    {
        switch(iSize)
        {
            case 4: putInt(off, v);  break;
            case 8: putLong(off, v); break;
            default: throw new AssertionError();
        }
    }
} // Data

} // Int2
