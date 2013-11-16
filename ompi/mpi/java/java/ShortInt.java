package mpi;

/**
 * Struct class for {@link MPI#SHORT_INT} datatype.
 */
public final class ShortInt extends Struct
{
private final int sSize, iOff, iSize;

/**
 * The struct object will be created only in MPI class.
 * @see MPI#shortInt
 */
protected ShortInt(int shortSize, int intOff, int intSize)
{
    sSize = shortSize;
    iSize = intSize;
    int sOff;

    switch(sSize)
    {
        case 2: sOff = addShort(); break;
        case 4: sOff = addInt();   break;
        case 8: sOff = addLong();  break;
        default: throw new AssertionError("Unsupported short size: "+ sSize);
    }

    assert sOff == 0;
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
@Override protected Data newData()
{
    return new Data();
}

/**
 * Class for reading/writing data in a struct stored in a byte buffer.
 */
public final class Data extends Struct.Data
{
    /**
     * Gets the short value.
     * @return short value
     */
    public short getValue()
    {
        switch(sSize)
        {
            case 2: return getShort(0);
            case 4: return (short)getInt(0);
            case 8: return (short)getLong(0);
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
     * Puts the short value.
     * @param v short value
     */
    public void putValue(short v)
    {
        switch(sSize)
        {
            case 2: putShort(0, v); break;
            case 4: putInt(0, v);   break;
            case 8: putLong(0, v);  break;
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

} // ShortInt
