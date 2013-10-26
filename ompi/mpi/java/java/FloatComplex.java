package mpi;

import java.nio.*;

/**
 * This class wraps a complex number stored in a buffer.
 */
public final class FloatComplex
{
private final int offset;
private final FloatBuffer buffer;

private FloatComplex(FloatBuffer buffer, int index)
{
    this.buffer = buffer;
    this.offset = index * 2;
}

/**
 * Wraps a complex number stored in a buffer
 * @param buffer buffer
 * @return complex number
 */
public static FloatComplex get(FloatBuffer buffer)
{
    return new FloatComplex(buffer, 0);
}

/**
 * Wraps the complex number at the specified position
 * of an array of complex numbers stored in a buffer.
 * @param buffer buffer
 * @param index  index
 * @return complex number
 */
public static FloatComplex get(FloatBuffer buffer, int index)
{
    return new FloatComplex(buffer, index);
}

/**
 * Wraps a complex number stored in the first two values of an array.
 * @param  array array
 * @return complex number
 */
public static FloatComplex get(float[] array)
{
    return new FloatComplex(FloatBuffer.wrap(array), 0);
}

/**
 * Wraps the complex number at the specified position of
 * an array of complex numbers stored in an array of floats.
 * @param array array
 * @param index index
 * @return complex number
 */
public static FloatComplex get(float[] array, int index)
{
    return new FloatComplex(FloatBuffer.wrap(array), index);
}

/**
 * Wraps a complex number stored in a buffer
 * @param buffer buffer
 * @return complex number
 */
public static FloatComplex get(ByteBuffer buffer)
{
    return new FloatComplex(buffer.asFloatBuffer(), 0);
}

/**
 * Wraps the complex number at the specified position
 * of an array of complex numbers stored in a buffer.
 * @param buffer buffer
 * @param index  index
 * @return complex number
 */
public static FloatComplex get(ByteBuffer buffer, int index)
{
    return new FloatComplex(buffer.asFloatBuffer(), index);
}

/**
 * Gets the real value.
 * @return real value
 */
public float getReal()
{
    return buffer.get(offset);
}

/**
 * Gets the imaginary value.
 * @return imaginary value.
 */
public float getImag()
{
    return buffer.get(offset + 1);
}

/**
 * Puts the real value.
 * @param real real value
 */
public void putReal(float real)
{
    buffer.put(offset, real);
}

/**
 * Puts the imaginary value.
 * @param imag imaginary value
 */
public void putImag(float imag)
{
    buffer.put(offset + 1, imag);
}

/**
 * Gets the buffer where the complex number is stored.
 * @return buffer where the complex number is stored
 */
public FloatBuffer getBuffer()
{
    return offset == 0 ? buffer : MPI.slice(buffer, offset);
}

} // FloatComplex
