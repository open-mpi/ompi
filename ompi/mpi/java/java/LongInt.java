package mpi;

/**
 * Struct class for {@link MPI#LONG_INT} datatype.
 */
public final class LongInt extends Struct
{
private final int lSize, iOff, iSize;

/**
 * The struct object will be created only in MPI class.
 * @see MPI#longInt
 */
protected LongInt(int longSize, int intOff, int intSize)
{
    lSize = longSize;
    iSize = intSize;
    int lOff;

    switch(lSize)
    {
        case 4: lOff = addInt();  break;
        case 8: lOff = addLong(); break;
        default: throw new AssertionError("Unsupported long size: "+ lSize);
    }

    assert lOff == 0;
    setOffset(intOff);

    switch(iSize)
    {
        case 4: iOff = addInt();  break;
        case 8: iOff = addLong(); break;
        default: throw new AssertionError("Unsupported int size: "+ iSize);
    }

    assert(intOff == iOff);
}

/**
 * Creates a Data object.
 * @return new Data object.
 */
@Override protected LongInt.Data newData()
{
    return new LongInt.Data();
}

/**
 * Class for reading/writing data in a struct stored in a byte buffer.
 */
public final class Data extends Struct.Data
{
    /**
     * Gets the long value.
     * @return long value
     */
    public long getValue()
    {
        switch(lSize)
        {
            case 8: return getLong(0);
            case 4: return getInt(0);
            default: throw new AssertionError();
        }
    }

    /**
     * Gets the int value.
     * @return int value
     */
    public int getIndex()
    {
        switch(iSize)
        {
            case 4: return getInt(iOff);
            case 8: return (int)getLong(iOff);
            default: throw new AssertionError();
        }
    }

    /**
     * Puts the long value.
     * @param v long value
     */
    public void putValue(long v)
    {
        switch(lSize)
        {
            case 8: putLong(0, v);     break;
            case 4: putInt(0, (int)v); break;
            default: throw new AssertionError();
        }
    }

    /**
     * Puts the int value.
     * @param v int value
     */
    public void putIndex(int v)
    {
        switch(iSize)
        {
            case 4: putInt(iOff, v);  break;
            case 8: putLong(iOff, v); break;
            default: throw new AssertionError();
        }
    }
} // Data

} // LongInt
