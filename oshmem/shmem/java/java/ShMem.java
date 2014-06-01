package shmem;

import java.nio.*;

/**
 * Java bindings of OpenSHMEM.
 */
public final class ShMem
{
private static final ByteOrder nativeOrder = ByteOrder.nativeOrder();

public static final int CMP_EQ, CMP_GE, CMP_GT, CMP_LE, CMP_LT, CMP_NE;

public static final int BARRIER_SYNC_SIZE,
                        BCAST_SYNC_SIZE,
                        COLLECT_SYNC_SIZE,
                        REDUCE_SYNC_SIZE,
                        REDUCE_MIN_WRKDATA_SIZE,
                        SYNC_VALUE;
static
{
    System.loadLibrary("oshmem_java");
    init();
    Constant c = new Constant();

    CMP_EQ = c.CMP_EQ;
    CMP_GE = c.CMP_GE;
    CMP_GT = c.CMP_GT;
    CMP_LE = c.CMP_LE;
    CMP_LT = c.CMP_LT;
    CMP_NE = c.CMP_NE;

    BARRIER_SYNC_SIZE       = c.BARRIER_SYNC_SIZE;
    BCAST_SYNC_SIZE         = c.BCAST_SYNC_SIZE;
    COLLECT_SYNC_SIZE       = c.COLLECT_SYNC_SIZE;
    REDUCE_SYNC_SIZE        = c.REDUCE_SYNC_SIZE;
    REDUCE_MIN_WRKDATA_SIZE = c.REDUCE_MIN_WRKDATA_SIZE;
    SYNC_VALUE              = c.SYNC_VALUE;
}

private static native void init();

/**
 * Initializes OpenSHMEM.
 * <p>Java binding of {@code start_pes}.
 * @param npes Unused. Should be set to 0.
 */
public static native void startPEs(int npes);

/**
 * Returns the number of processing elements (PEs) used to run the application.
 * <p>Java binding of {@code num_pes}.
 * @return Number of processing elements (PEs) used to run the application.
 */
public static native int getNumPEs();

/**
 * Returns the PE number of the calling PE.
 * <p>Java binding of {@code shmem_my_pe}.
 * @return PE number of the calling PE.
 */
public static native int getMyPE();

/**
 * Determines if a target PE is reachable from the calling PE.
 * <p>Java binding of {@code shmem_pe_accessible}.
 * @param pe PE number of the target PE.
 * @return true if the target PE is reachable, false otherwise.
 */
public static boolean isPEAccessible(int pe)
{
    return isPEAccessible_jni(pe);
}

private static native boolean isPEAccessible_jni(int pe);

/**
 * Performs a barrier operation on a subset of PEs.
 * <p>Java binding of {@code shmem_barrier}.
 * @param PE_start     Lowest PE number of the active set of PEs.
 * @param logPE_stride Log (base 2) of the stride between consecutive
 *                     PE numbers in the active set.
 * @param PE_size      Number of PEs in the active set.
 * @param pSync        Symmetric {@code long} work array.
 */
public static void barrier(
        int PE_start, int logPE_stride, int PE_size, Addr pSync)
{
    barrier(PE_start, logPE_stride, PE_size, pSync.addr());
}

private static native void barrier(
        int PE_start, int logPE_stride, int PE_size, long pSync);

/**
 * Suspends the execution of the calling PE until all other
 * PEs issue a call to this statement.
 * <p>Java binding of {@code shmem_barrier_all}.
 */
public static native void barrierAll();

/**
 * Provides a separate ordering on the sequence of puts issued
 * by this PE to each destination PE.
 * <p>Java binding of {@code shmem_fence}.
 */
public static native void fence();

/**
 * Provides an ordering on the sequence of puts issued by this
 * PE across all destination PEs.
 * <p>Java binding of {@code shmem_quiet}.
 */
public static native void quiet();

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
 * @param length length
 * @return the new buffer.
 */
public static ByteBuffer slice(ByteBuffer buf, int offset, int length)
{
    return ((ByteBuffer)buf.limit(offset + length)
            .position(offset)).slice().order(nativeOrder);
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
 * @param length length
 * @return the new buffer.
 */
public static ShortBuffer slice(ShortBuffer buf, int offset, int length)
{
    return ((ShortBuffer)buf.limit(offset + length).position(offset)).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static IntBuffer slice(IntBuffer buf, int offset, int length)
{
    return ((IntBuffer)buf.limit(offset + length).position(offset)).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static LongBuffer slice(LongBuffer buf, int offset, int length)
{
    return ((LongBuffer)buf.limit(offset + length).position(offset)).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static FloatBuffer slice(FloatBuffer buf, int offset, int length)
{
    return ((FloatBuffer)buf.limit(offset + length).position(offset)).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static DoubleBuffer slice(DoubleBuffer buf, int offset, int length)
{
    return ((DoubleBuffer)buf.limit(offset + length).position(offset)).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static ByteBuffer slice(byte[] buf, int offset, int length)
{
    return ByteBuffer.wrap(buf, offset, length).slice().order(nativeOrder);
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
 * @param length length
 * @return the new buffer.
 */
public static ShortBuffer slice(short[] buf, int offset, int length)
{
    return ShortBuffer.wrap(buf, offset, length).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static IntBuffer slice(int[] buf, int offset, int length)
{
    return IntBuffer.wrap(buf, offset, length).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static LongBuffer slice(long[] buf, int offset, int length)
{
    return LongBuffer.wrap(buf, offset, length).slice();
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
 * @param length length
 * @return the new buffer.
 */
public static FloatBuffer slice(float[] buf, int offset, int length)
{
    return FloatBuffer.wrap(buf, offset, length).slice();
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

/**
 * Creates a new buffer whose content is a shared subsequence of a buffer.
 * <p>The content of the new buffer will start at the specified offset.
 * @param buf    buffer
 * @param offset offset
 * @param length length
 * @return the new buffer.
 */
public static DoubleBuffer slice(double[] buf, int offset, int length)
{
    return DoubleBuffer.wrap(buf, offset, length).slice();
}

} // ShMem
