package mpi;

import java.nio.*;

/**
 * This class wraps a complex number stored in a buffer.
 */
public final class DoubleComplex
{
private final int offset;
private final DoubleBuffer buffer;

private DoubleComplex(DoubleBuffer buffer, int index)
{
    this.buffer = buffer;
    this.offset = index * 2;
}

/**
 * Wraps a complex number stored in a buffer
 * @param buffer buffer
 * @return complex number
 */
public static DoubleComplex get(DoubleBuffer buffer)
{
    return new DoubleComplex(buffer, 0);
}

/**
 * Wraps the complex number at the specified position
 * of an array of complex numbers stored in a buffer.
 * @param buffer buffer
 * @param index  index
 * @return complex number
 */
public static DoubleComplex get(DoubleBuffer buffer, int index)
{
    return new DoubleComplex(buffer, index);
}

/**
 * Wraps a complex number stored in the first two values of an array.
 * @param  array array
 * @return complex number
 */
public static DoubleComplex get(double[] array)
{
    return new DoubleComplex(DoubleBuffer.wrap(array), 0);
}

/**
 * Wraps the complex number at the specified position of
 * an array of complex numbers stored in an array of doubles.
 * @param array array
 * @param index index
 * @return complex number
 */
public static DoubleComplex get(double[] array, int index)
{
    return new DoubleComplex(DoubleBuffer.wrap(array), index);
}

/**
 * Wraps a complex number stored in a buffer
 * @param buffer buffer
 * @return complex number
 */
public static DoubleComplex get(ByteBuffer buffer)
{
    return new DoubleComplex(buffer.asDoubleBuffer(), 0);
}

/**
 * Wraps the complex number at the specified position
 * of an array of complex numbers stored in a buffer.
 * @param buffer buffer
 * @param index  index
 * @return complex number
 */
public static DoubleComplex get(ByteBuffer buffer, int index)
{
    return new DoubleComplex(buffer.asDoubleBuffer(), index);
}

/**
 * Gets the real value.
 * @return real value
 */
public double getReal()
{
    return buffer.get(offset);
}

/**
 * Gets the imaginary value.
 * @return imaginary value.
 */
public double getImag()
{
    return buffer.get(offset + 1);
}

/**
 * Puts the real value.
 * @param real real value
 */
public void putReal(double real)
{
    buffer.put(offset, real);
}

/**
 * Puts the imaginary value.
 * @param imag imaginary value
 */
public void putImag(double imag)
{
    buffer.put(offset + 1, imag);
}

/**
 * Gets the buffer where the complex number is stored.
 * @return buffer where the complex number is stored
 */
public DoubleBuffer getBuffer()
{
    return offset == 0 ? buffer : MPI.slice(buffer, offset);
}

} // DoubleComplex
