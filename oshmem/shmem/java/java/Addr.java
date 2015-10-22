package shmem;

import java.nio.*;

/**
 * Symmetric data object.
 */
public final class Addr
{
private long handle;     // Address of the block memory.
private int  offset;     // Current offset of the data object.
private ByteBuffer buffer; // Direct buffer of the block memory.

static
{
    init();
}

private static native void init();

private Addr()
{
}

/**
 * Allocates a block of memory in the symmetric heap of the calling PE.
 * <p>Java binding of {@code shmalloc}.
 * @param size Size of the requested memory block, in bytes.
 * @throws ShMemException Allocation error.
 */
public Addr(int size) throws ShMemException
{
    buffer = malloc(size);
    checkAllocation();
}

private native ByteBuffer malloc(int size);

/**
 * Allocates a block from the symmetric heap with a byte alignment
 * specified by the programmer.
 * <p>Java binding of {@code shmemalign}.
 * @param align Size of the alignment block, in bytes.
 * @param size  Size for the memory block, in bytes.
 * @throws ShMemException Allocation error.
 */
public Addr(int align, int size) throws ShMemException
{
    buffer = memalign(align, size);
    checkAllocation();
}

private native ByteBuffer memalign(int align, int size);

/**
 * Expands or reduces the size of the block.
 * <p>Java binding of {@code shrealloc}.
 * @param size New size for the memory block, in bytes.
 * @throws ShMemException Allocation error.
 */
public void realloc(int size) throws ShMemException
{
    buffer = realloc(handle, size);
    checkAllocation();
}

private native ByteBuffer realloc(long addr, int size);

/**
 * Creates a new object that shares this object's content.
 * @param offset Offset of the new data object, in bytes.
 * @return New data object.
 */
public Addr slice(int offset)
{
    Addr addr = new Addr();
    addr.handle = handle;
    addr.offset = offset;
    addr.buffer = buffer;
    return addr;
}

/**
 * Gets a view of this object as a byte buffer.
 * @return Byte buffer.
 */
public ByteBuffer asByteBuffer()
{
    return ShMem.slice(buffer, offset);
}

/**
 * Gets a view of this object as a short buffer.
 * @return Short buffer.
 */
public ShortBuffer asShortBuffer()
{
    buffer.position(offset);
    return buffer.asShortBuffer();
}

/**
 * Gets a view of this object as an int buffer.
 * @return Int buffer.
 */
public IntBuffer asIntBuffer()
{
    buffer.position(offset);
    return buffer.asIntBuffer();
}

/**
 * Gets a view of this object as a long buffer.
 * @return Long buffer.
 */
public LongBuffer asLongBuffer()
{
    buffer.position(offset);
    return buffer.asLongBuffer();
}

/**
 * Gets a view of this object as a float buffer.
 * @return Float buffer.
 */
public FloatBuffer asFloatBuffer()
{
    buffer.position(offset);
    return buffer.asFloatBuffer();
}

/**
 * Gets a view of this object as a double buffer.
 * @return Double buffer.
 */
public DoubleBuffer asDoubleBuffer()
{
    buffer.position(offset);
    return buffer.asDoubleBuffer();
}

/**
 * Writes a {@code byte} value to symmetric data object.
 * @param value Value to be written.
 */
public void putByte(byte value)
{
    buffer.put(offset, value);
}

/**
 * Writes a {@code short} value to symmetric data object.
 * @param value Value to be written.
 */
public void putShort(short value)
{
    buffer.putShort(offset, value);
}

/**
 * Writes a {@code int} value to symmetric data object.
 * @param value Value to be written.
 */
public void putInt(int value)
{
    buffer.putInt(offset, value);
}

/**
 * Writes a {@code long} value to symmetric data object.
 * @param value Value to be written.
 */
public void putLong(long value)
{
    buffer.putLong(offset, value);
}

/**
 * Writes a {@code float} value to symmetric data object.
 * @param value Value to be written.
 */
public void putFloat(float value)
{
    buffer.putFloat(offset, value);
}

/**
 * Writes a {@code double} value to symmetric data object.
 * @param value Value to be written.
 */
public void putDouble(double value)
{
    buffer.putDouble(offset, value);
}

/**
 * Reads a {@code short} value from the symmetric data object.
 * @return Value readed.
 */
public short getShort()
{
    return buffer.getShort(offset);
}

/**
 * Reads a {@code int} value from the symmetric data object.
 * @return Value readed.
 */
public int getInt()
{
    return buffer.getInt(offset);
}

/**
 * Reads a {@code long} value from the symmetric data object.
 * @return Value readed.
 */
public long getLong()
{
    return buffer.getLong(offset);
}

/**
 * Reads a {@code float} value from the symmetric data object.
 * @return Value readed.
 */
public float getFloat()
{
    return buffer.getFloat(offset);
}

/**
 * Reads a {@code double} value from the symmetric data object.
 * @return Value readed.
 */
public double getDouble()
{
    return buffer.getDouble(offset);
}

/**
 * Indicates if an address is accessible via OpenSHMEM operations
 * from the specified target PE.
 * <p>Java binding of {@code shmem_addr_accessible}.
 * @param pe PE number of the target PE.
 * @return true if the address is accessible, false otherwise.
 */
public boolean isAccessible(int pe)
{
    return isAccessible(handle, pe);
}

private native boolean isAccessible(long addr, int pe);

/**
 * Frees a memory block.
 * <p>Java binding of {@code shfree}.
 */
public void free()
{
    free(handle);
    handle = 0;
    buffer = null;
}

private native void free(long addr);

private void checkAllocation() throws ShMemException
{
    if(buffer == null)
        throw new ShMemException("Allocation error.");

    buffer.order(ByteOrder.nativeOrder());
}

/**
 * Returns an address that can be used to directly reference
 * target (this object) on the target PE.
 * <p>Java binding of {@code shmem_ptr}.
 * @param pe Target PE.
 * @return Address to the data object on the target PE.
 * @throws ShMemException if the target cannot be accessed directly.
 */
public Addr getAddr(int pe) throws ShMemException
{
    // If native getAddr returns null maybe we also must
    // return null instead of throwing an exception.
    Addr addr = new Addr();
    addr.buffer = getAddr(buffer, pe, addr);
    addr.offset = offset;
    addr.checkAllocation();
    return addr;
}

private native ByteBuffer getAddr(ByteBuffer buff, int pe, Addr addr);

/**
 * Writes a {@code byte} to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_char_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putByte(byte value, int pe)
{
    putByte(addr(), value, pe);
}

private native void putByte(long addr, byte value, int pe);

/**
 * Writes a {@code short} value to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_short_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putShort(short value, int pe)
{
    putShort(addr(), value, pe);
}

private native void putShort(long addr, short value, int pe);

/**
 * Writes an {@code int} value to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_int_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putInt(int value, int pe)
{
    putInt(addr(), value, pe);
}

private native void putInt(long addr, int value, int pe);

/**
 * Writes a {@code long} value to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_long_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putLong(long value, int pe)
{
    putLong(addr(), value, pe);
}

private native void putLong(long addr, long value, int pe);

/**
 * Writes a {@code float} value to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_float_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putFloat(float value, int pe)
{
    putFloat(addr(), value, pe);
}

private native void putFloat(long addr, float value, int pe);

/**
 * Writes a {@code double} value to symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_double_p}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 */
public void putDouble(double value, int pe)
{
    putDouble(addr(), value, pe);
}

private native void putDouble(long addr, double value, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_char_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putByte(ByteBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putByteBuffer(addr(), source, len, pe);
    else
        putByteArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putByteBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_char_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putByte(byte[] source, int pe)
{
    putByteArray(addr(), source, 0, source.length, pe);
}

private native void putByteArray(
        long addr, byte[] src, int off, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_short_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putShort(ShortBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putShortBuffer(addr(), source, len, pe);
    else
        putShortArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putShortBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_short_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putShort(short[] source, int pe)
{
    putShortArray(addr(), source, 0, source.length, pe);
}

private native void putShortArray(
        long addr, short[] src, int off, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_int_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putInt(IntBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putIntBuffer(addr(), source, len, pe);
    else
        putIntArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putIntBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_int_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putInt(int[] source, int pe)
{
    putIntArray(addr(), source, 0, source.length, pe);
}

private native void putIntArray(long addr, int[] src, int off, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_long_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putLong(LongBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putLongBuffer(addr(), source, len, pe);
    else
        putLongArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putLongBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_long_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putLong(long[] source, int pe)
{
    putLongArray(addr(), source, 0, source.length, pe);
}

private native void putLongArray(
        long addr, long[] src, int off, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_float_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putFloat(FloatBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putFloatBuffer(addr(), source, len, pe);
    else
        putFloatArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putFloatBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_float_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putFloat(float[] source, int pe)
{
    putFloatArray(addr(), source, 0, source.length, pe);
}

private native void putFloatArray(
        long addr, float[] src, int off, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source capacity.
 * <p>Java binding of {@code shmem_double_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putDouble(DoubleBuffer source, int pe)
{
    int len = source.capacity();

    if(source.isDirect())
        putDoubleBuffer(addr(), source, len, pe);
    else
        putDoubleArray(addr(), source.array(), source.arrayOffset(), len, pe);
}

private native void putDoubleBuffer(long addr, Buffer src, int len, int pe);

/**
 * Copies contiguous data from a local object to an object
 * on the destination PE.
 * <p>The number of elements to be transferred is the source length.
 * <p>Java binding of {@code shmem_double_put}.
 * @param source Buffer to be transferred to the target data object.
 * @param pe     Target PE.
 */
public void putDouble(double[] source, int pe)
{
    putDoubleArray(addr(), source, 0, source.length, pe);
}

private native void putDoubleArray(
        long addr, double[] src, int off, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_short_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutShort(ShortBuffer source, int tst, int sst, int len, int pe)
{
    if(source.isDirect())
    {
        iPutShortBuffer(addr(), source, tst, sst, len, pe);
    }
    else
    {
        iPutShortArray(addr(), source.array(),
                       source.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iPutShortBuffer(
        long addr, ShortBuffer src, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_short_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutShort(short[] source, int tst, int sst, int len, int pe)
{
    iPutShortArray(addr(), source, 0, tst, sst, len, pe);
}

private native void iPutShortArray(
        long addr, short[] src, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_int_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutInt(IntBuffer source, int tst, int sst, int len, int pe)
{
    if(source.isDirect())
    {
        iPutIntBuffer(addr(), source, tst, sst, len, pe);
    }
    else
    {
        iPutIntArray(addr(), source.array(),
                     source.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iPutIntBuffer(
        long addr, Buffer src, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_int_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutInt(int[] source, int tst, int sst, int len, int pe)
{
    iPutIntArray(addr(), source, 0, tst, sst, len, pe);
}

private native void iPutIntArray(
        long addr, int[] src, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_long_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutLong(LongBuffer source, int tst, int sst, int len, int pe)
{
    if(source.isDirect())
    {
        iPutLongBuffer(addr(), source, tst, sst, len, pe);
    }
    else
    {
        iPutLongArray(addr(), source.array(),
                      source.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iPutLongBuffer(
        long addr, Buffer src, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_long_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutLong(long[] source, int tst, int sst, int len, int pe)
{
    iPutLongArray(addr(), source, 0, tst, sst, len, pe);
}

private native void iPutLongArray(
        long addr, long[] src, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_float_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutFloat(FloatBuffer source, int tst, int sst, int len, int pe)
{
    if(source.isDirect())
    {
        iPutFloatBuffer(addr(), source, tst, sst, len, pe);
    }
    else
    {
        iPutFloatArray(addr(), source.array(),
                       source.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iPutFloatBuffer(
        long addr, Buffer src, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_float_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutFloat(float[] source, int tst, int sst, int len, int pe)
{
    iPutFloatArray(addr(), source, 0, tst, sst, len, pe);
}

private native void iPutFloatArray(
        long addr, float[] src, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_double_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutDouble(DoubleBuffer source, int tst, int sst, int len, int pe)
{
    if(source.isDirect())
    {
        iPutDoubleBuffer(addr(), source, tst, sst, len, pe);
    }
    else
    {
        iPutDoubleArray(addr(), source.array(),
                        source.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iPutDoubleBuffer(
        long addr, Buffer src, int tst, int sst, int len, int pe);

/**
 * Copies strided data from a local object to a strided data object
 * on the destination PE.
 * <p>Java binding of {@code shmem_double_iput}.
 * @param source Buffer to be transferred to the target data object.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iPutDouble(double[] source, int tst, int sst, int len, int pe)
{
    iPutDoubleArray(addr(), source, 0, tst, sst, len, pe);
}

private native void iPutDoubleArray(
        long addr, double[] src, int off, int tst, int sst, int len, int pe);

/**
 * Reads a {@code byte} from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_char_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public byte getByte(int pe)
{
    return getByte(addr(), pe);
}

private native byte getByte(long addr, int pe);

/**
 * Reads a {@code short} value from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_short_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public short getShort(int pe)
{
    return getShort(addr(), pe);
}

private native short getShort(long addr, int pe);

/**
 * Reads an {@code int} value from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_int_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public int getInt(int pe)
{
    return getInt(addr(), pe);
}

private native int getInt(long addr, int pe);

/**
 * Reads a {@code long} value from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_long_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public long getLong(int pe)
{
    return getLong(addr(), pe);
}

private native long getLong(long addr, int pe);

/**
 * Reads a {@code float} value from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_float_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public float getFloat(int pe)
{
    return getFloat(addr(), pe);
}

private native float getFloat(long addr, int pe);

/**
 * Reads a {@code double} value from the symmetric data object on the target PE.
 * <p>Java binding of {@code shmem_double_g}.
 * @param pe Target PE.
 * @return The value at the symmetric data object on the target PE.
 */
public double getDouble(int pe)
{
    return getDouble(addr(), pe);
}

private native double getDouble(long addr, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_char_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getByte(ByteBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getByteBuffer(addr(), target, len, pe);
    else
        getByteArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getByteBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_char_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getByte(byte[] target, int pe)
{
    getByteArray(addr(), target, 0, target.length, pe);
}

private native void getByteArray(
        long addr, byte[] target, int off, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_short_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getShort(ShortBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getShortBuffer(addr(), target, len, pe);
    else
        getShortArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getShortBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_short_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getShort(short[] target, int pe)
{
    getShortArray(addr(), target, 0, target.length, pe);
}

private native void getShortArray(
        long addr, short[] target, int off, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_int_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getInt(IntBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getIntBuffer(addr(), target, len, pe);
    else
        getIntArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getIntBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_int_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getInt(int[] target, int pe)
{
    getIntArray(addr(), target, 0, target.length, pe);
}

private native void getIntArray(
        long addr, int[] target, int off, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_long_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getLong(LongBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getLongBuffer(addr(), target, len, pe);
    else
        getLongArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getLongBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_long_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getLong(long[] target, int pe)
{
    getLongArray(addr(), target, 0, target.length, pe);
}

private native void getLongArray(
        long addr, long[] target, int off, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_float_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getFloat(FloatBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getFloatBuffer(addr(), target, len, pe);
    else
        getFloatArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getFloatBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_float_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getFloat(float[] target, int pe)
{
    getFloatArray(addr(), target, 0, target.length, pe);
}

private native void getFloatArray(
        long addr, float[] target, int off, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target capacity.
 * <p>Java binding of {@code shmem_double_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getDouble(DoubleBuffer target, int pe)
{
    int len = target.capacity();

    if(target.isDirect())
        getDoubleBuffer(addr(), target, len, pe);
    else
        getDoubleArray(addr(), target.array(), target.arrayOffset(), len, pe);
}

private native void getDoubleBuffer(long addr, Buffer target, int len, int pe);

/**
 * Copies contiguous data to a local object from an object on the target PE.
 * <p>The number of elements to be transferred is the target length.
 * <p>Java binding of {@code shmem_double_get}.
 * @param target Buffer in which to save de data on the local PE.
 * @param pe     Target PE.
 */
public void getDouble(double[] target, int pe)
{
    getDoubleArray(addr(), target, 0, target.length, pe);
}

private native void getDoubleArray(
        long addr, double[] target, int off, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_short_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetShort(ShortBuffer target, int tst, int sst, int len, int pe)
{
    if(target.isDirect())
    {
        iGetShortBuffer(addr(), target, tst, sst, len, pe);
    }
    else
    {
        iGetShortArray(addr(), target.array(),
                       target.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iGetShortBuffer(
        long addr, Buffer target, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_short_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetShort(short[] target, int tst, int sst, int len, int pe)
{
    iGetShortArray(addr(), target, 0, tst, sst, len, pe);
}

private native void iGetShortArray(
        long addr, short[] target, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_int_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetInt(IntBuffer target, int tst, int sst, int len, int pe)
{
    if(target.isDirect())
    {
        iGetIntBuffer(addr(), target, tst, sst, len, pe);
    }
    else
    {
        iGetIntArray(addr(), target.array(),
                     target.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iGetIntBuffer(
        long addr, Buffer target, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_int_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetInt(int[] target, int tst, int sst, int len, int pe)
{
    iGetIntArray(addr(), target, 0, tst, sst, len, pe);
}

private native void iGetIntArray(
        long addr, int[] target, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_long_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetLong(LongBuffer target, int tst, int sst, int len, int pe)
{
    if(target.isDirect())
    {
        iGetLongBuffer(addr(), target, tst, sst, len, pe);
    }
    else
    {
        iGetLongArray(addr(), target.array(),
                      target.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iGetLongBuffer(
        long addr, Buffer target, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_long_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetLong(long[] target, int tst, int sst, int len, int pe)
{
    iGetLongArray(addr(), target, 0, tst, sst, len, pe);
}

private native void iGetLongArray(
        long addr, long[] target, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_float_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetFloat(FloatBuffer target, int tst, int sst, int len, int pe)
{
    if(target.isDirect())
    {
        iGetFloatBuffer(addr(), target, tst, sst, len, pe);
    }
    else
    {
        iGetFloatArray(addr(), target.array(),
                       target.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iGetFloatBuffer(
        long addr, Buffer target, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_float_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetFloat(float[] target, int tst, int sst, int len, int pe)
{
    iGetFloatArray(addr(), target, 0, tst, sst, len, pe);
}

private native void iGetFloatArray(
        long addr, float[] target, int off, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_double_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetDouble(DoubleBuffer target, int tst, int sst, int len, int pe)
{
    if(target.isDirect())
    {
        iGetDoubleBuffer(addr(), target, tst, sst, len, pe);
    }
    else
    {
        iGetDoubleArray(addr(), target.array(),
                        target.arrayOffset(), tst, sst, len, pe);
    }
}

private native void iGetDoubleBuffer(
        long addr, Buffer target, int tst, int sst, int len, int pe);

/**
 * Copies strided data located on a target PE to a local strided data object.
 * <p>Java binding of {@code shmem_double_iget}.
 * @param target Buffer in which to save the data on the local PE.
 * @param tst    Stride between consecutive elements in the target object.
 * @param sst    Stride between consecutive elements in the source object.
 * @param len    Number of elements in the target and source objects.
 * @param pe     Target PE.
 */
public void iGetDouble(double[] target, int tst, int sst, int len, int pe)
{
    iGetDoubleArray(addr(), target, 0, tst, sst, len, pe);
}

private native void iGetDoubleArray(
        long addr, double[] target, int off, int tst, int sst, int len, int pe);

/**
 * Performs an atomic swap operation.
 * <p>Java binding of {@code shmem_int_swap}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public int swapInt(int value, int pe)
{
    return swapInt(addr(), value, pe);
}

private native int swapInt(long addr, int value, int pe);

/**
 * Performs an atomic swap operation.
 * <p>Java binding of {@code shmem_long_swap}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public long swapLong(long value, int pe)
{
    return swapLong(addr(), value, pe);
}

private native long swapLong(long addr, long value, int pe);

/**
 * Performs an atomic swap operation.
 * <p>Java binding of {@code shmem_float_swap}.
 * @param value Value to be transferred.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public float swapFloat(float value, int pe)
{
    return swapFloat(addr(), value, pe);
}

private native float swapFloat(long addr, float value, int pe);

/**
 * Performs an atomic swap operation.
 * <p>Java binding of {@code shmem_double_swap}.
 * @param value  Value to be transferred.
 * @param pe     Target PE.
 * @return Previous value of target.
 */
public double swapDouble(double value, int pe)
{
    return swapDouble(addr(), value, pe);
}

private native double swapDouble(long addr, double value, int pe);

/**
 * Performs an atomic conditional swap operation.
 * <p>Java binding of {@code shmem_int_cswap}.
 * @param cond  Value compared to the target value.
 * @param value Value to be written to the target PE.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public int cSwapInt(int cond, int value, int pe)
{
    return cSwapInt(addr(), cond, value, pe);
}

private native int cSwapInt(long addr, int cond, int value, int pe);

/**
 * Performs an atomic conditional swap operation.
 * <p>Java binding of {@code shmem_long_cswap}.
 * @param cond  Value compared to the target value.
 * @param value Value to be written to the target PE.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public long cSwapLong(long cond, long value, int pe)
{
    return cSwapLong(addr(), cond, value, pe);
}

private native long cSwapLong(long addr, long cond, long value, int pe);

/**
 * Performs an atomic fetch-and-add operation.
 * <p>Java binding of {@code shmem_int_fadd}.
 * @param value Value to be added.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public int fAddInt(int value, int pe)
{
    return fAddInt(addr(), value, pe);
}

private native int fAddInt(long addr, int value, int pe);

/**
 * Performs an atomic fetch-and-add operation.
 * <p>Java binding of {@code shmem_long_fadd}.
 * @param value Value to be added.
 * @param pe    Target PE.
 * @return Previous value of target.
 */
public long fAddLong(long value, int pe)
{
    return fAddLong(addr(), value, pe);
}

private native long fAddLong(long addr, long value, int pe);

/**
 * Performs an atomic fetch-and-increment operation.
 * <p>Java binding of {@code shmem_int_finc}.
 * @param pe Target PE.
 * @return Previous value of target.
 */
public int fIncInt(int pe)
{
    return fIncInt(addr(), pe);
}

private native int fIncInt(long addr, int pe);

/**
 * Performs an atomic fetch-and-increment operation.
 * <p>Java binding of {@code shmem_long_finc}.
 * @param pe Target PE.
 * @return Previous value of target.
 */
public long fIncLong(int pe)
{
    return fIncLong(addr(), pe);
}

private native long fIncLong(long addr, int pe);

/**
 * Performs an atomic add operation.
 * <p>Java binding of {@code shmem_int_add}.
 * @param value Value to be added.
 * @param pe    Target PE.
 */
public void addInt(int value, int pe)
{
    addInt(addr(), value, pe);
}

private native void addInt(long addr, int value, int pe);

/**
 * Performs an atomic add operation.
 * <p>Java binding of {@code shmem_long_add}.
 * @param value Value to be added.
 * @param pe    Target PE.
 */
public void addLong(long value, int pe)
{
    addLong(addr(), value, pe);
}

private native void addLong(long addr, long value, int pe);

/**
 * Performs an atomic increment operation.
 * <p>Java binding of {@code shmem_int_inc}.
 * @param pe Target PE.
 */
public void incInt(int pe)
{
    incInt(addr(), pe);
}

private native void incInt(long addr, int pe);

/**
 * Performs an atomic increment operation.
 * <p>Java binding of {@code shmem_long_inc}.
 * @param pe Target PE.
 */
public void incLong(int pe)
{
    incLong(addr(), pe);
}

private native void incLong(long addr, int pe);

/**
 * Sets a mutual exclusion memory lock.
 * <p>Java binding of {@code shmem_set_lock}.
 * The {@code long} value at address lock must be set to 0 on all PEs
 * prior to the first use.
 */
public void setLock()
{
    setLock(handle);
}

private native void setLock(long addr);

/**
 * Releases a lock previously set by the calling PE.
 * <p>Java binding of {@code shmem_clear_lock}.
 */
public void clearLock()
{
    clearLock(handle);
}

private native void clearLock(long addr);

/**
 * Sets a mutual exclusion lock only if it is currently cleared.
 * <p>Java binding of {@code shmem_test_lock}.
 * @return True if the lock had been set and the call returned without
 *         waiting to set the lock. False if the lock was originally
 *         cleared and this call was able to set the lock.
 */
public boolean testLock()
{
    return testLock(handle);
}

private native boolean testLock(long addr);

/**
 * Forces the calling PE to wait until the value at {@code offset}
 * is no longer equal to {@code value}.
 * <p>Java binding of {@code shmem_short_wait}.
 * @param value Value to be compared against the value at {@code offset}.
 */
public void waitShort(short value)
{
    waitShort(addr(), value);
}

private native void waitShort(long addr, short value);

/**
 * Forces the calling PE to wait until the value at {@code offset}
 * is no longer equal to {@code value}.
 * <p>Java binding of {@code shmem_int_wait}.
 * @param value  Value to be compared against the value at {@code offset}.
 */
public void waitInt(int value)
{
    waitInt(addr(), value);
}

private native void waitInt(long addr, int value);

/**
 * Forces the calling PE to wait until the value at {@code offset}
 * is no longer equal to {@code value}.
 * <p>Java binding of {@code shmem_long_wait}.
 * @param value  Value to be compared against the value at {@code offset}.
 */
public void waitLong(long value)
{
    waitLong(addr(), value);
}

private native void waitLong(long addr, long value);

/**
 * Forces the calling PE to wait until the condition {@code cond}
 * and {@code value} is satisfied.
 * <p>Java binding of {@code shmem_short_wait_until}.
 * @param cond  Indicates how to compare the value at
 *              {@code offset} and {@code value}.
 * @param value Value to compare.
 */
public void waitUntilShort(int cond, short value)
{
    waitUntilShort(addr(), cond, value);
}

private native void waitUntilShort(long addr, int cmp, short value);

/**
 * Forces the calling PE to wait until the condition {@code cond}
 * and {@code value} is satisfied.
 * <p>Java binding of {@code shmem_int_wait_until}.
 * @param cond  Indicates how to compare the value at
 *              {@code offset} and {@code value}.
 * @param value Value to compare.
 */
public void waitUntilInt(int cond, int value)
{
    waitUntilInt(addr(), cond, value);
}

private native void waitUntilInt(long addr, int cmp, int value);

/**
 * Forces the calling PE to wait until the condition {@code cond}
 * and {@code value} is satisfied.
 * <p>Java binding of {@code shmem_long_wait_until}.
 * @param cond  Indicates how to compare the value at
 *              {@code offset} and {@code value}.
 * @param value Value to compare.
 */
public void waitUntilLong(int cond, long value)
{
    waitUntilLong(addr(), cond, value);
}

private native void waitUntilLong(long addr, int cmp, long value);

/**
 * Copy a data from a root PE to a target location on all other
 * PEs of the active set.
 * <p>Java binding of {@code shmem_broadcast32}.
 * @param source   Symmetric data to be copied.
 * @param nlong    Number of elements in the target and source objects.
 * @param PE_root  PE from which data will be copied.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void broadcast32(Addr source, int nlong, int PE_root, int PE_start,
                        int logPE_stride, int PE_size, PSync pSync)
{
    broadcast32(addr(), source.addr(), nlong, PE_root,
                PE_start, logPE_stride, PE_size, pSync.addr());
}

private native void broadcast32(
        long target, long source, int nlong, int PE_root,
        int PE_start, int logPE_stride, int PE_size, long pSync);

/**
 * Copy a data from a root PE to a target location on all other
 * PEs of the active set.
 * <p>Java binding of {@code shmem_broadcast64}.
 * @param source   Symmetric data to be copied.
 * @param nlong    Number of elements in the target and source objects.
 * @param PE_root  PE from which data will be copied.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void broadcast64(Addr source, int nlong, int PE_root, int PE_start,
                        int logPE_stride, int PE_size, PSync pSync)
{
    broadcast64(addr(), source.addr(), nlong, PE_root,
                PE_start, logPE_stride, PE_size, pSync.addr());
}

private native void broadcast64(
        long target, long source, int nlong, int PE_root,
        int PE_start, int logPE_stride, int PE_size, long pSync);

/**
 * Concatenates blocks of data from multiple PEs to an object in every PE.
 * <p>Java binding of {@code shmem_collect32}.
 * @param source   Data to be concatenated.
 * @param nlong    Number of elements in the source objects.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void collect32(Addr source, int nlong, int PE_start,
                      int logPE_stride, int PE_size, PSync pSync)
{
    collect32(addr(), source.addr(), nlong, PE_start,
              logPE_stride, PE_size, pSync.addr());
}

private native void collect32(
        long target, long source, int nlong, int PE_start,
        int logPE_stride, int PE_size, long pSync);

/**
 * Concatenates blocks of data from multiple PEs to an object in every PE.
 * <p>Java binding of {@code shmem_collect64}.
 * @param source   Symmetric data to be copied.
 * @param nlong    Number of elements in the source objects.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void collect64(Addr source, int nlong, int PE_start,
                      int logPE_stride, int PE_size, PSync pSync)
{
    collect64(addr(), source.addr(), nlong, PE_start,
              logPE_stride, PE_size, pSync.addr());
}

private native void collect64(
        long target, long source, int nlong, int PE_start,
        int logPE_stride, int PE_size, long pSync);

/**
 * Concatenates blocks of data from multiple PEs to an object in every PE.
 * <p>Java binding of {@code shmem_fcollect32}.
 * @param source   Symmetric data to be copied.
 * @param nlong    Number of elements in the source objects.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void fcollect32(Addr source, int nlong, int PE_start,
                       int logPE_stride, int PE_size, PSync pSync)
{
    fcollect32(addr(), source.addr(), nlong, PE_start,
               logPE_stride, PE_size, pSync.addr());
}

private native void fcollect32(
        long target, long source, int nlong, int PE_start,
        int logPE_stride, int PE_size, long pSync);

/**
 * Concatenates blocks of data from multiple PEs to an object in every PE.
 * <p>Java binding of {@code shmem_fcollect64}.
 * @param source   Symmetric data to be copied.
 * @param nlong    Number of elements in the source objects.
 * @param PE_start The lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pSync    Symmetric work array.
 */
public void fcollect64(Addr source, int nlong, int PE_start,
                       int logPE_stride, int PE_size, PSync pSync)
{
    fcollect64(addr(), source.addr(), nlong, PE_start,
               logPE_stride, PE_size, pSync.addr());
}

private native void fcollect64(
        long target, long source, int nlong, int PE_start,
        int logPE_stride, int PE_size, long pSync);

/**
 * Performs a bitwise AND operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_and_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void andToAllShort(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    andToAllShort(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void andToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise AND operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_and_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void andToAllInt(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    andToAllInt(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void andToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise AND operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_and_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void andToAllLong(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    andToAllLong(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void andToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise OR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_or_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void orToAllShort(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    orToAllShort(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void orToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise OR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_or_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void orToAllInt(Addr source, int nreduce, int PE_start,
                       int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    orToAllInt(addr(), source.addr(), nreduce, PE_start,
               logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void orToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise OR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_or_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void orToAllLong(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    orToAllLong(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void orToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise XOR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_xor_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void xorToAllShort(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    xorToAllShort(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void xorToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise XOR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_xor_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void xorToAllInt(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    xorToAllInt(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void xorToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Performs a bitwise XOR operation on symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_xor_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void xorToAllLong(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    xorToAllLong(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void xorToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the maximum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_max_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void maxToAllShort(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    maxToAllShort(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void maxToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the maximum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_max_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void maxToAllInt(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    maxToAllInt(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void maxToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the maximum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_max_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void maxToAllLong(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    maxToAllLong(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void maxToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the maximum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_float_max_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code float} work array.
 * @param pSync    Symmetric work array.
 */
public void maxToAllFloat(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    maxToAllFloat(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void maxToAllFloat(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the maximum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_double_max_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code double} work array.
 * @param pSync    Symmetric work array.
 */
public void maxToAllDouble(Addr source, int nreduce, int PE_start,
                           int logPE_stride, int PE_size,
                           Addr pWrk, PSync pSync)
{
    maxToAllDouble(addr(), source.addr(), nreduce, PE_start,
                   logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void maxToAllDouble(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the minimum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_min_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void minToAllShort(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    minToAllShort(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void minToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the minimum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_min_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void minToAllInt(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    minToAllInt(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void minToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the minimum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_min_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void minToAllLong(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    minToAllLong(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void minToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the minimum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_float_min_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code float} work array.
 * @param pSync    Symmetric work array.
 */
public void minToAllFloat(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    minToAllFloat(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void minToAllFloat(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the minimum value of the source data over the active set of PEs.
 * <p>Java binding of {@code shmem_double_min_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code double} work array.
 * @param pSync    Symmetric work array.
 */
public void minToAllDouble(Addr source, int nreduce, int PE_start,
                           int logPE_stride, int PE_size,
                           Addr pWrk, PSync pSync)
{
    minToAllDouble(addr(), source.addr(), nreduce, PE_start,
                   logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void minToAllDouble(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the summation of symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_short_sum_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void sumToAllShort(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    sumToAllShort(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void sumToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the summation of symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_int_sum_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void sumToAllInt(Addr source, int nreduce, int PE_start,
                        int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    sumToAllInt(addr(), source.addr(), nreduce, PE_start,
                logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void sumToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the summation of symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_long_sum_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void sumToAllLong(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    sumToAllLong(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void sumToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the summation of symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_float_sum_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code float} work array.
 * @param pSync    Symmetric work array.
 */
public void sumToAllFloat(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    sumToAllFloat(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void sumToAllFloat(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the summation of symmetric data over the active set of PEs.
 * <p>Java binding of {@code shmem_double_sum_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code double} work array.
 * @param pSync    Symmetric work array.
 */
public void sumToAllDouble(Addr source, int nreduce, int PE_start,
                           int logPE_stride, int PE_size,
                           Addr pWrk, PSync pSync)
{
    sumToAllDouble(addr(), source.addr(), nreduce, PE_start,
                   logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void sumToAllDouble(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the product of symmetric arrays over the active set of PEs.
 * <p>Java binding of {@code shmem_short_prod_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code short} work array.
 * @param pSync    Symmetric work array.
 */
public void prodToAllShort(Addr source, int nreduce, int PE_start,
                           int logPE_stride, int PE_size,
                           Addr pWrk, PSync pSync)
{
    prodToAllShort(addr(), source.addr(), nreduce, PE_start,
                   logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void prodToAllShort(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the product of symmetric arrays over the active set of PEs.
 * <p>Java binding of {@code shmem_int_prod_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code int} work array.
 * @param pSync    Symmetric work array.
 */
public void prodToAllInt(Addr source, int nreduce, int PE_start,
                         int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    prodToAllInt(addr(), source.addr(), nreduce, PE_start,
                 logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void prodToAllInt(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the product of symmetric arrays over the active set of PEs.
 * <p>Java binding of {@code shmem_long_prod_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code long} work array.
 * @param pSync    Symmetric work array.
 */
public void prodToAllLong(Addr source, int nreduce, int PE_start,
                          int logPE_stride, int PE_size, Addr pWrk, PSync pSync)
{
    prodToAllLong(addr(), source.addr(), nreduce, PE_start,
                  logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void prodToAllLong(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the product of symmetric arrays over the active set of PEs.
 * <p>Java binding of {@code shmem_float_prod_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code float} work array.
 * @param pSync    Symmetric work array.
 */
public void prodToAllFloat(Addr source, int nreduce, int PE_start,
                           int logPE_stride, int PE_size,
                           Addr pWrk, PSync pSync)
{
    prodToAllFloat(addr(), source.addr(), nreduce, PE_start,
                   logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void prodToAllFloat(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

/**
 * Computes the product of symmetric arrays over the active set of PEs.
 * <p>Java binding of {@code shmem_double_prod_to_all}.
 * @param source   Symmetric data object that contains the elements for each
 *                 separate reduction operation.
 * @param nreduce  Number of elements in the target (this) and source data.
 * @param PE_start Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between PE numbers.
 * @param PE_size  Number of PEs in the active set.
 * @param pWrk     Symmetric {@code double} work array.
 * @param pSync    Symmetric work array.
 */
public void prodToAllDouble(Addr source, int nreduce, int PE_start,
                            int logPE_stride, int PE_size,
                            Addr pWrk, PSync pSync)
{
    prodToAllDouble(addr(), source.addr(), nreduce, PE_start,
                    logPE_stride, PE_size, pWrk.addr(), pSync.addr());
}

private native void prodToAllDouble(
        long target, long source, int nreduce, int PE_start,
        int logPE_stride, int PE_size, long pWrk, long pSync);

protected long addr()
{
    return handle + offset;
}

} // Addr
