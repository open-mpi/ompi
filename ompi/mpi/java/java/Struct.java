package mpi;

import java.nio.*;
import java.util.*;

/**
 * Base class for defining struct data types.
 */
public abstract class Struct
{
private int extent;
private ArrayList<Field> fields = new ArrayList<Field>();

private Datatype datatype, types[];
private int offsets[], lengths[];
private static final String typeMismatch = "Type mismatch";

private void commit() throws MPIException
{
    if(datatype == null)
        createStruct();
}

private void createStruct() throws MPIException
{
    int count = fields.size();
    types   = new Datatype[count];
    offsets = new int[count];
    lengths = new int[count];

    for(int i = 0; i < count; i++)
    {
        Field f = fields.get(i);

        types[i] = f.type instanceof Struct ? ((Struct)f.type).datatype
                                            : (Datatype)f.type;
        offsets[i] = f.offset;
        lengths[i] = f.length;
    }

    datatype = Datatype.createStruct(lengths, offsets, types);
    datatype.commit();
    extent = datatype.getExtent();
}

/**
 * Returns the extent of the struct data type.
 * @return Extent of the struct data type.
 * @throws MPIException 
 */
public final int getExtent() throws MPIException
{
    commit();
    return extent;
}

/**
 * Returns the data type of the struct.
 * @return The data type of the struct.
 * @throws MPIException 
 */
public final Datatype getType() throws MPIException
{
    commit();
    return datatype;
}

/**
 * Creates a Data object.
 * @return New Data object.
 */
protected abstract Data newData();

@SuppressWarnings("unchecked")
private <T extends Data> T newData(ByteBuffer buffer, int offset)
{
    Data d = newData();
    d.buffer = buffer;
    d.offset = offset;
    return (T)d;
}

/**
 * Gets a Data object in order to access to the buffer.
 * @param buffer the Data object will read/write on this buffer.
 * @return Data object
 * @throws MPIException 
 */
public final <T extends Data> T getData(ByteBuffer buffer) throws MPIException
{
    commit();
    return newData(buffer, 0);
}

/**
 * Gets a Data object in order to access to the struct at the
 * specified position of a struct array stored in a Buffer.
 * @param buffer The Data object will read/write on this buffer.
 * @param index  Index of the struct in the buffer.
 * @return Data object.
 * @throws MPIException 
 */
public final <T extends Data> T getData(ByteBuffer buffer, int index)
    throws MPIException
{
    commit();
    return newData(buffer, index * extent);
}

/**
 * Gets a Data object in order to access to the byte array.
 * @param array The Data object will read/write on this byte array.
 * @return Data object.
 * @throws MPIException 
 */
public final <T extends Data> T getData(byte[] array) throws MPIException
{
    ByteBuffer buffer = ByteBuffer.wrap(array);
    buffer.order(ByteOrder.nativeOrder());
    return getData(buffer);
}

/**
 * Gets a Data object in order to access to the struct at the
 * specified position of a struct array stored in a byte array.
 * @param array The Data object will read/write on this byte array.
 * @param index Index of the struct in the array.
 * @return Data object.
 * @throws MPIException 
 */
public final <T extends Data> T getData(byte[] array, int index)
    throws MPIException
{
    ByteBuffer buffer = ByteBuffer.wrap(array);
    buffer.order(ByteOrder.nativeOrder());
    return getData(buffer, index);
}

private int addField(Object type, int typeExtent, int length)
{
    if(datatype != null)
        throw new AssertionError("The struct data type was committed.");

    int offset = extent;
    extent += typeExtent * length;
    fields.add(new Field(type, offset, length));
    return offset;
}

/**
 * Sets the offset of the next field.
 * <p>The offset must be greater or equal to the accumulated extent.
 * @param offset offset of the next field
 * @return this object in order to allow adding fields in a chained expression
 */
public final Struct setOffset(int offset)
{
    if(datatype != null)
        throw new AssertionError("The struct data type was committed.");

    if(offset < extent)
    {
        throw new IllegalArgumentException(
            "The offset must be greater or equal to the accumulated extent.");
    }

    extent = offset;
    return this;
}

/**
 * Adds a byte field to this struct.
 * @return Offset of the new field.
 */
public final int addByte()
{
    return addByte(1);
}

/**
 * Adds a byte array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addByte(int length)
{
    return addField(MPI.BYTE, 1, length);
}

/**
 * Adds a char field to this struct.
 * @return Offset of the new field.
 */
public final int addChar()
{
    return addChar(1);
}

/**
 * Adds a char array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addChar(int length)
{
    return addField(MPI.CHAR, 2, length);
}

/**
 * Adds a short field to this struct.
 * @return Offset of the new field.
 */
public final int addShort()
{
    return addShort(1);
}

/**
 * Adds a short array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addShort(int length)
{
    return addField(MPI.SHORT, 2, length);
}

/**
 * Adds an int field to this struct.
 * @return Offset of the new field.
 */
public final int addInt()
{
    return addInt(1);
}

/**
 * Adds an int array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addInt(int length)
{
    return addField(MPI.INT, 4, length);
}

/**
 * Adds a long field to this struct.
 * @return Offset of the new field.
 */
public final int addLong()
{
    return addLong(1);
}

/**
 * Adds a long array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addLong(int length)
{
    return addField(MPI.LONG, 8, length);
}

/**
 * Adds a float field to this struct.
 * @return Offset of the new field.
 */
public final int addFloat()
{
    return addFloat(1);
}

/**
 * Adds a float array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addFloat(int length)
{
    return addField(MPI.FLOAT, 4, length);
}

/**
 * Adds a double field to this struct.
 * @return Offset of the new field.
 */
public final int addDouble()
{
    return addDouble(1);
}

/**
 * Adds a double array to this struct.
 * @param length Length of the array.
 * @return Offset of the new field.
 */
public final int addDouble(int length)
{
    return addField(MPI.DOUBLE, 8, length);
}

/**
 * Adds a struct field to this struct.
 * @param struct Type of the field.
 * @return Offset of the new field.
 * @throws MPIException
 */
public final int addStruct(Struct struct) throws MPIException
{
    return addStruct(struct, 1);
}

/**
 * Adds an array of structs to this struct.
 * @param struct Type of the array.
 * @param length Length of the array.
 * @return Offset of the new field.
 * @throws MPIException 
 */
public final int addStruct(Struct struct, int length) throws MPIException
{
    struct.commit();
    return addField(struct, struct.extent, length);
}

/**
 * Adds a field of the specified data type.
 * @param type Data type.
 * @return Offset of the new field.
 * @throws MPIException 
 */
public final int addData(Datatype type) throws MPIException
{
    return addData(type, 1);
}

/**
 * Adds an array of the specified data type.
 * @param type Data type.
 * @param length Length of the array.
 * @return Offset of the new field.
 * @throws MPIException 
 */
public final int addData(Datatype type, int length) throws MPIException
{
    return addField(type, type.getExtent() * type.baseSize, length);
}

private boolean validType(int fieldOffset, int index, Datatype type)
{
    int i = Arrays.binarySearch(offsets, fieldOffset);
    return index >= 0 && index < lengths[i] && type == types[i];
}

private static class Field
{
    private Object type;
    private int offset, length;

    private Field(Object type, int offset, int length)
    {
        this.type   = type;
        this.offset = offset;
        this.length = length;
    }

} // Field

/**
 * Base class for reading/writing data in a struct stored in a byte buffer.
 */
public abstract class Data
{
    private ByteBuffer buffer;
    private int offset;

    /**
     * Gets the buffer where this struct data is stored.
     * <p>The buffer can be used in {@code send}/{@code recv} operations.
     * @return Buffer where the struct data is stored.
     */
    public final ByteBuffer getBuffer()
    {
        return offset == 0 ? buffer : MPI.slice(buffer, offset);
    }

    /**
     * Gets the byte value of a field.
     * @param field Offset of the field.
     * @return Byte value.
     */
    protected final byte getByte(int field)
    {
        assert validType(field, 0, MPI.BYTE) : typeMismatch;
        return buffer.get(offset + field);
    }

    /**
     * Gets the byte value at the specified position of a byte array.
     * @param field Offset of the byte array.
     * @param index Index of the byte in the array.
     * @return Byte value.
     */
    protected final byte getByte(int field, int index)
    {
        assert validType(field, index, MPI.BYTE) : typeMismatch;
        return buffer.get(offset + field + index);
    }

    /**
     * Puts a byte value in a field.
     * @param field Offset of the field.
     * @param v     Byte value.
     */
    protected final void putByte(int field, byte v)
    {
        assert validType(field, 0, MPI.BYTE) : typeMismatch;
        buffer.put(offset + field, v);
    }

    /**
     * Puts a byte value at the specified position of a byte array.
     * @param field Offset of the byte array.
     * @param index Index of the byte in the array.
     * @param v     Byte value.
     */
    protected final void putByte(int field, int index, byte v)
    {
        assert validType(field, index, MPI.BYTE) : typeMismatch;
        buffer.put(offset + field + index, v);
    }

    /**
     * Gets the char value of a field.
     * @param field Offset of the field.
     * @return Char value.
     */
    protected final char getChar(int field)
    {
        assert validType(field, 0, MPI.CHAR) : typeMismatch;
        return buffer.getChar(offset + field);
    }

    /**
     * Gets the char value at the specified position of a char array.
     * @param field Offset of the char array.
     * @param index Index of the char in the array.
     * @return Char value.
     */
    protected final char getChar(int field, int index)
    {
        assert validType(field, index, MPI.CHAR) : typeMismatch;
        return buffer.getChar(offset + field + index * 2);
    }

    /**
     * Puts a char value in a field.
     * @param field Offset of the field.
     * @param v     Char value.
     */
    protected final void putChar(int field, char v)
    {
        assert validType(field, 0, MPI.CHAR) : typeMismatch;
        buffer.putChar(offset + field, v);
    }

    /**
     * Puts a char value at the specified position of a char array.
     * @param field Offset of the char array.
     * @param index Index of the char in the array.
     * @param v     Char value.
     */
    protected final void putChar(int field, int index, char v)
    {
        assert validType(field, index, MPI.CHAR) : typeMismatch;
        buffer.putChar(offset + field + index * 2, v);
    }

    /**
     * Gets the short value of a field.
     * @param field Offset of the field.
     * @return Short value.
     */
    protected final short getShort(int field)
    {
        assert validType(field, 0, MPI.SHORT) : typeMismatch;
        return buffer.getShort(offset + field);
    }

    /**
     * Gets the short value at the specified position of a short array.
     * @param field Offset of the short array.
     * @param index Index of the short in the array.
     * @return Short value.
     */
    protected final short getShort(int field, int index)
    {
        assert validType(field, index, MPI.SHORT) : typeMismatch;
        return buffer.getShort(offset + field + index * 2);
    }

    /**
     * Puts a short value in a field.
     * @param field Offset of the field.
     * @param v     Short value.
     */
    protected final void putShort(int field, short v)
    {
        assert validType(field, 0, MPI.SHORT) : typeMismatch;
        buffer.putShort(offset + field, v);
    }

    /**
     * Puts a short value at the specified position of a short array.
     * @param field Offset of the short array.
     * @param index Index of the short in the array.
     * @param v     Short value.
     */
    protected final void putShort(int field, int index, short v)
    {
        assert validType(field, index, MPI.SHORT) : typeMismatch;
        buffer.putShort(offset + field + index * 2, v);
    }

    /**
     * Gets the int value of a field.
     * @param field Offset of the field.
     * @return Int value.
     */
    protected final int getInt(int field)
    {
        assert validType(field, 0, MPI.INT) : typeMismatch;
        return buffer.getInt(offset + field);
    }

    /**
     * Gets the int value at the specified position of an int array.
     * @param field Offset of the int array.
     * @param index Index of the int in the array.
     * @return Int value.
     */
    protected final int getInt(int field, int index)
    {
        assert validType(field, index, MPI.INT) : typeMismatch;
        return buffer.getInt(offset + field + index * 4);
    }

    /**
     * Puts an int value in a field.
     * @param field Offset of the field.
     * @param v     Int value.
     */
    protected final void putInt(int field, int v)
    {
        assert validType(field, 0, MPI.INT) : typeMismatch;
        buffer.putInt(offset + field, v);
    }

    /**
     * Puts an int value at the specified position of an int array.
     * @param field Offset of the int array.
     * @param index Index of the int in the array.
     * @param v     Int value.
     */
    protected final void putInt(int field, int index, int v)
    {
        assert validType(field, index, MPI.INT) : typeMismatch;
        buffer.putInt(offset + field + index * 4, v);
    }

    /**
     * Gets the long value of a field.
     * @param field Offset of the field.
     * @return Long value.
     */
    protected final long getLong(int field)
    {
        assert validType(field, 0, MPI.LONG) : typeMismatch;
        return buffer.getLong(offset + field);
    }

    /**
     * Gets the long value at the specified position of a long array.
     * @param field Offset of the long array.
     * @param index Index of the long in the array.
     * @return Long value.
     */
    protected final long getLong(int field, int index)
    {
        assert validType(field, index, MPI.LONG) : typeMismatch;
        return buffer.getLong(offset + field + index * 8);
    }

    /**
     * Puts a long value in a field.
     * @param field Offset of the field.
     * @param v     Long value.
     */
    protected final void putLong(int field, long v)
    {
        assert validType(field, 0, MPI.LONG) : typeMismatch;
        buffer.putLong(offset + field, v);
    }

    /**
     * Puts a long value at the specified position of a long array.
     * @param field Offset of the long array.
     * @param index Index of the long in the array.
     * @param v     Long value.
     */
    protected final void putLong(int field, int index, long v)
    {
        assert validType(field, index, MPI.LONG) : typeMismatch;
        buffer.putLong(offset + field + index * 8, v);
    }

    /**
     * Gets the float value of a field.
     * @param field Offset of the field.
     * @return Float value.
     */
    protected final float getFloat(int field)
    {
        assert validType(field, 0, MPI.FLOAT) : typeMismatch;
        return buffer.getFloat(offset + field);
    }

    /**
     * Gets the float value at the specified position of a float array.
     * @param field Offset of the float array.
     * @param index Index of the float in the array.
     * @return Float value.
     */
    protected final float getFloat(int field, int index)
    {
        assert validType(field, index, MPI.FLOAT) : typeMismatch;
        return buffer.getFloat(offset + field + index * 4);
    }

    /**
     * Puts a float value in a field.
     * @param field Offset of the field.
     * @param v     Float value.
     */
    protected final void putFloat(int field, float v)
    {
        assert validType(field, 0, MPI.FLOAT) : typeMismatch;
        buffer.putFloat(offset + field, v);
    }

    /**
     * Puts a float value at the specified position of a float array.
     * @param field Offset of the float array.
     * @param index Index of the float in the array.
     * @param v     Float value.
     */
    protected final void putFloat(int field, int index, float v)
    {
        assert validType(field, index, MPI.FLOAT) : typeMismatch;
        buffer.putFloat(offset + field + index * 4, v);
    }

    /**
     * Gets the double value of a field.
     * @param field Offset of the field.
     * @return Double value.
     */
    protected final double getDouble(int field)
    {
        assert validType(field, 0, MPI.DOUBLE) : typeMismatch;
        return buffer.getDouble(offset + field);
    }

    /**
     * Gets the double value at the specified position of a double array.
     * @param field Offset of the double array.
     * @param index Index of the double in the array.
     * @return Double value.
     */
    protected final double getDouble(int field, int index)
    {
        assert validType(field, index, MPI.DOUBLE) : typeMismatch;
        return buffer.getDouble(offset + field + index * 8);
    }

    /**
     * Puts a double value in a field.
     * @param field Offset of the field.
     * @param v     Double value.
     */
    protected final void putDouble(int field, double v)
    {
        assert validType(field, 0, MPI.DOUBLE) : typeMismatch;
        buffer.putDouble(offset + field, v);
    }

    /**
     * Puts a double value at the specified position of a double array.
     * @param field Offset of the double array.
     * @param index Index of the double in the array.
     * @param v     Double value.
     */
    protected final void putDouble(int field, int index, double v)
    {
        assert validType(field, index, MPI.DOUBLE) : typeMismatch;
        buffer.putDouble(offset + field + index * 8, v);
    }

    /**
     * Gets the struct data of a field.
     * @param struct Struct type.
     * @param field  Offset of the field.
     * @return Struct data.
     */
    protected final <S extends Struct, D extends Struct.Data>
            D getData(S struct, int field)
    {
        Struct s = (Struct)struct;
        assert validType(field, 0, s.datatype) : typeMismatch;
        return s.newData(buffer, offset + field);
    }

    /**
     * Gets the struct data at the specified position of a struct array.
     * @param struct Struct type.
     * @param field  Offset of the struct array.
     * @param index  Index of the struct in the array.
     * @return Struct data. 
     */
    protected final <S extends Struct, D extends Struct.Data>
            D getData(S struct, int field, int index)
    {
        Struct s = (Struct)struct;
        assert validType(field, index, s.datatype) : typeMismatch;
        return s.newData(buffer, offset + field + index * s.extent);
    }

    /**
     * Gets the buffer of a field.
     * <p>The buffer can be used in {@code send}/{@code recv} operations.
     * @param type  Data type of the buffer.
     * @param field Offset of the field.
     * @return Buffer object.
     */
    protected final ByteBuffer getBuffer(Datatype type, int field)
    {
        assert validType(field, 0, type) : typeMismatch;
        int position = offset + field;
        return position == 0 ? buffer : MPI.slice(buffer, position);
    }

    /**
     * Gets the buffer data at the specified position of a buffer array.
     * <p>The buffer can be used in {@code send}/{@code recv} operations.
     * @param type  Data type of the buffer.
     * @param field Offset of the buffer array.
     * @param index Index of the buffer in the array.
     * @return Buffer object.
     * @throws MPIException 
     */
    protected final ByteBuffer getBuffer(Datatype type, int field, int index)
        throws MPIException
    {
        assert validType(field, index, type) : typeMismatch;

        int extent   = type.getExtent() * type.baseSize,
            position = offset + field + index * extent;

        return position == 0 ? buffer : MPI.slice(buffer, position);
    }

} // Data

} // Struct
