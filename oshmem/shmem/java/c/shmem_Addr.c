#include "oshmem_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "shmem.h"
#include "shmem_Addr.h"
#include "shmemJava.h"

JNIEXPORT void JNICALL Java_shmem_Addr_init(JNIEnv *env, jclass clazz)
{
    shmem_java.AddrHandle = (*env)->GetFieldID(env, clazz, "handle", "J");
}

JNIEXPORT jobject JNICALL Java_shmem_Addr_malloc(
        JNIEnv *env, jobject jthis, jint size)
{
    void *addr = shmalloc(size);
    (*env)->SetLongField(env, jthis, shmem_java.AddrHandle, (jlong)addr);
    return addr ? (*env)->NewDirectByteBuffer(env, addr, size) : NULL;
}

JNIEXPORT jobject JNICALL Java_shmem_Addr_memalign(
        JNIEnv *env, jobject jthis, jint align, jint size)
{
    void *addr = shmemalign(align, size);
    (*env)->SetLongField(env, jthis, shmem_java.AddrHandle, (jlong)addr);
    return addr ? (*env)->NewDirectByteBuffer(env, addr, size) : NULL;
}

JNIEXPORT jobject JNICALL Java_shmem_Addr_realloc(
        JNIEnv *env, jobject jthis, jlong jaddr, jint size)
{
    void *addr = shrealloc((void*)jaddr, size);
    (*env)->SetLongField(env, jthis, shmem_java.AddrHandle, (jlong)addr);
    return addr ? (*env)->NewDirectByteBuffer(env, addr, size) : NULL;
}

JNIEXPORT jboolean JNICALL Java_shmem_Addr_isAccessible(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_addr_accessible((void*)addr, pe) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_shmem_Addr_free(
        JNIEnv *env, jobject jthis, jlong addr)
{
    shfree((void*)addr);
}

JNIEXPORT jobject JNICALL Java_shmem_Addr_getAddr(
        JNIEnv *env, jobject jthis, jobject buff, jint pe, jobject obj)
{
    void *addr = (*env)->GetDirectBufferAddress(env, buff);
    jlong size = (*env)->GetDirectBufferCapacity(env, buff);
    void *ptr  = shmem_ptr(addr, pe);
    (*env)->SetLongField(env, obj, shmem_java.AddrHandle, (jlong)ptr);
    return ptr ? (*env)->NewDirectByteBuffer(env, ptr, size) : NULL;
}

JNIEXPORT void JNICALL Java_shmem_Addr_putByte(
        JNIEnv *env, jobject jthis, jlong addr, jbyte value, jint pe)
{
    shmem_char_p((char*)addr, (char)value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putShort(
        JNIEnv *env, jobject jthis, jlong addr, jshort value, jint pe)
{
    shmem_int16_p((int16_t*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putInt(
        JNIEnv *env, jobject jthis, jlong addr, jint value, jint pe)
{
    shmem_int32_p((int32_t*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putLong(
        JNIEnv *env, jobject jthis, jlong addr, jlong value, jint pe)
{
    shmem_int64_p((int64_t*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putFloat(
        JNIEnv *env, jobject jthis, jlong addr, jfloat value, jint pe)
{
    shmem_float_p((float*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putDouble(
        JNIEnv *env, jobject jthis, jlong addr, jdouble value, jint pe)
{
    shmem_double_p((double*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putByteBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_char_put((char*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putByteArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jbyteArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadByteArray(&item, env, array, off, len);
    shmem_char_put((char*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putShortBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_put16((void*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putShortArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jshortArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadShortArray(&item, env, array, off, len);
    shmem_put16((void*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putIntBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_put32((void*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putIntArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jintArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadIntArray(&item, env, array, off, len);
    shmem_put32((void*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putLongBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_put64((void*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putLongArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jlongArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadLongArray(&item, env, array, off, len);
    shmem_put64((void*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putFloatBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_float_put((float*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putFloatArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jfloatArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadFloatArray(&item, env, array, off, len);
    shmem_float_put((float*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putDoubleBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_double_put((double*)addr, source, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_putDoubleArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jdoubleArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_getReadDoubleArray(&item, env, array, off, len);
    shmem_double_put((double*)addr, source, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutShortBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iput16((void*)addr, source, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutShortArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jshortArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_iGetReadShortArray(&item,env,array,off,sst,len);
    shmem_iput16((void*)addr, source, tst, sst, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutIntBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iput32((void*)addr, source, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutIntArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jintArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_iGetReadIntArray(&item,env,array,off,sst,len);
    shmem_iput32((void*)addr, source, tst, sst, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutLongBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iput64((void*)addr, source, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutLongArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jlongArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_iGetReadLongArray(&item,env,array,off,sst,len);
    shmem_iput64((void*)addr, source, tst, sst, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutFloatBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_float_iput((float*)addr, source, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutFloatArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jfloatArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_iGetReadFloatArray(&item,env,array,off,sst,len);
    shmem_float_iput((float*)addr, source, tst, sst, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutDoubleBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *source = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_double_iput((double*)addr, source, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iPutDoubleArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jdoubleArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *source = shmem_java_iGetReadDoubleArray(&item,env,array,off,sst,len);
    shmem_double_iput((double*)addr, source, tst, sst, len, pe);
    shmem_java_releaseBuffer(source, item);
}

JNIEXPORT jbyte JNICALL Java_shmem_Addr_getByte(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_char_g((char*)addr, pe);
}

JNIEXPORT jshort JNICALL Java_shmem_Addr_getShort(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_int16_g((int16_t*)addr, pe);
}

JNIEXPORT jint JNICALL Java_shmem_Addr_getInt(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_int32_g((int32_t*)addr, pe);
}

JNIEXPORT jlong JNICALL Java_shmem_Addr_getLong(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_int64_g((int64_t*)addr, pe);
}

JNIEXPORT jfloat JNICALL Java_shmem_Addr_getFloat(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_float_g((float*)addr, pe);
}

JNIEXPORT jdouble JNICALL Java_shmem_Addr_getDouble(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_double_g((double*)addr, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getByteBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_char_get(target, (char*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getByteArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jbyteArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len);
    shmem_char_get(target, (char*)addr, len, pe);
    shmem_java_releaseWriteByteArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getShortBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_get16(target, (void*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getShortArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jshortArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len * 2);
    shmem_get16(target, (void*)addr, len, pe);
    shmem_java_releaseWriteShortArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getIntBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_get32(target, (void*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getIntArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jintArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len * 4);
    shmem_get32(target, (void*)addr, len, pe);
    shmem_java_releaseWriteIntArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getLongBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_get64(target, (void*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getLongArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jlongArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len * 8);
    shmem_get64(target, (void*)addr, len, pe);
    shmem_java_releaseWriteLongArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getFloatBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_float_get(target, (float*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getFloatArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jfloatArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len * 4);
    shmem_float_get(target, (float*)addr, len, pe);
    shmem_java_releaseWriteFloatArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getDoubleBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_double_get(target, (double*)addr, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_getDoubleArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jdoubleArray array, jint off, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_getBuffer(&item, env, len * 8);
    shmem_double_get(target, (double*)addr, len, pe);
    shmem_java_releaseWriteDoubleArray(target, item, env, array, off, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetShortBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iget16(target, (void*)addr, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetShortArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jshortArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_iGetBuffer(&item, env, tst, len, 2);
    shmem_iget16(target, (void*)addr, tst, sst, len, pe);
    shmem_java_iReleaseWriteShortArray(target, item, env, array, off, tst, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetIntBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iget32(target, (void*)addr, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetIntArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jintArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_iGetBuffer(&item, env, tst, len, 4);
    shmem_iget32(target, (void*)addr, tst, sst, len, pe);
    shmem_java_iReleaseWriteIntArray(target, item, env, array, off, tst, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetLongBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_iget64(target, (void*)addr, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetLongArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jlongArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_iGetBuffer(&item, env, tst, len, 8);
    shmem_iget64(target, (void*)addr, tst, sst, len, pe);
    shmem_java_iReleaseWriteLongArray(target, item, env, array, off, tst, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetFloatBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_float_iget(target, (float*)addr, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetFloatArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jfloatArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_iGetBuffer(&item, env, tst, len, 4);
    shmem_float_iget(target, (void*)addr, tst, sst, len, pe);
    shmem_java_iReleaseWriteFloatArray(target, item, env, array, off, tst, len);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetDoubleBuffer(
        JNIEnv *env, jobject jthis, jlong addr,
        jobject buffer, jint tst, jint sst, jint len, jint pe)
{
    void *target = (*env)->GetDirectBufferAddress(env, buffer);
    shmem_double_iget(target, (double*)addr, tst, sst, len, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_iGetDoubleArray(
        JNIEnv *env, jobject jthis, jlong addr,
        jdoubleArray array, jint off, jint tst, jint sst, jint len, jint pe)
{
    shmem_java_buffer_t *item;
    void *target = shmem_java_iGetBuffer(&item, env, tst, len, 8);
    shmem_double_iget(target, (void*)addr, tst, sst, len, pe);
    shmem_java_iReleaseWriteDoubleArray(target,item, env, array, off, tst, len);
}

JNIEXPORT jint JNICALL Java_shmem_Addr_swapInt(
        JNIEnv *env, jobject jthis, jlong addr, jint value, jint pe)
{
    return shmem_int32_swap((int32_t*)addr, value, pe);
}

JNIEXPORT jlong JNICALL Java_shmem_Addr_swapLong(
        JNIEnv *env, jobject jthis, jlong addr, jlong value, jint pe)
{
    return shmem_int64_swap((int64_t*)addr, value, pe);
}

JNIEXPORT jfloat JNICALL Java_shmem_Addr_swapFloat(
        JNIEnv *env, jobject jthis, jlong addr, jfloat value, jint pe)
{
    return shmem_float_swap((float*)addr, value, pe);
}

JNIEXPORT jdouble JNICALL Java_shmem_Addr_swapDouble(
        JNIEnv *env, jobject jthis, jlong addr, jdouble value, jint pe)
{
    return shmem_double_swap((double*)addr, value, pe);
}

JNIEXPORT jint JNICALL Java_shmem_Addr_cSwapInt(
        JNIEnv *env, jobject jthis, jlong addr, jint cond, jint value, jint pe)
{
    return shmem_int32_cswap((int32_t*)addr, cond, value, pe);
}

JNIEXPORT jlong JNICALL Java_shmem_Addr_cSwapLong(
        JNIEnv *env, jobject jthis, jlong addr,
        jlong cond, jlong value, jint pe)
{
    return shmem_int64_cswap((int64_t*)addr, cond, value, pe);
}

JNIEXPORT jint JNICALL Java_shmem_Addr_fAddInt(
        JNIEnv *env, jobject jthis, jlong addr, jint value, jint pe)
{
    return shmem_int32_fadd((int32_t*)addr, value, pe);
}

JNIEXPORT jlong JNICALL Java_shmem_Addr_fAddLong(
        JNIEnv *env, jobject jthis, jlong addr, jlong value, jint pe)
{
    return shmem_int64_fadd((int64_t*)addr, value, pe);
}

JNIEXPORT jint JNICALL Java_shmem_Addr_fIncInt(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_int32_finc((int32_t*)addr, pe);
}

JNIEXPORT jlong JNICALL Java_shmem_Addr_fIncLong(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    return shmem_int64_finc((int64_t*)addr, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_addInt(
        JNIEnv *env, jobject jthis, jlong addr, jint value, jint pe)
{
    shmem_int32_add((int32_t*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_addLong(
        JNIEnv *env, jobject jthis, jlong addr, jlong value, jint pe)
{
    shmem_int64_add((int64_t*)addr, value, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_incInt(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    shmem_int32_inc((int32_t*)addr, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_incLong(
        JNIEnv *env, jobject jthis, jlong addr, jint pe)
{
    shmem_int64_inc((int64_t*)addr, pe);
}

JNIEXPORT void JNICALL Java_shmem_Addr_setLock(
        JNIEnv *env, jobject jthis, jlong addr)
{
    shmem_set_lock((long*)addr);
}

JNIEXPORT void JNICALL Java_shmem_Addr_clearLock(
        JNIEnv *env, jobject jthis, jlong addr)
{
    shmem_clear_lock((long*)addr);
}

JNIEXPORT jboolean JNICALL Java_shmem_Addr_testLock(
        JNIEnv *env, jobject jthis, jlong addr)
{
    int r = shmem_test_lock((long*)addr);

    if (0 == r) {
        return JNI_FALSE;
    } else if (1 == r) {
        return JNI_TRUE;
    }

    /* Should never get here */
    opal_output(0, "*** Java lock is neither true nor false; something is very wrong!");
    abort();
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitShort(
        JNIEnv *env, jobject jthis, jlong addr, jshort value)
{
    shmem_int16_wait((void*)addr, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitInt(
        JNIEnv *env, jobject jthis, jlong addr, jint value)
{
    shmem_int32_wait((void*)addr, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitLong(
        JNIEnv *env, jobject jthis, jlong addr, jlong value)
{
    shmem_int64_wait((void*)addr, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitUntilShort(
        JNIEnv *env, jobject jthis, jlong addr, jint cmp, jshort value)
{
    shmem_int16_wait_until((void*)addr, cmp, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitUntilInt(
        JNIEnv *env, jobject jthis, jlong addr, jint cmp, jint value)
{
    shmem_int32_wait_until((void*)addr, cmp, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_waitUntilLong(
        JNIEnv *env, jobject jthis, jlong addr, jint cmp, jlong value)
{
    shmem_int64_wait_until((void*)addr, cmp, value);
}

JNIEXPORT void JNICALL Java_shmem_Addr_broadcast32(
        JNIEnv *env, jobject jthis, jlong target, jlong source,
        jint nlong, jint PE_root, jint PE_start, jint logPE_stride,
        jint PE_size, jlong pSync)
{
    shmem_broadcast32((void*)target, (void*)source, nlong, PE_root,
                      PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_broadcast64(
        JNIEnv *env, jobject jthis, jlong target, jlong source,
        jint nlong, jint PE_root, jint PE_start, jint logPE_stride,
        jint PE_size, jlong pSync)
{
    shmem_broadcast64((void*)target, (void*)source, nlong, PE_root,
                      PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_collect32(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nlong,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pSync)
{
    shmem_collect32((void*)target, (void*)source, nlong,
                    PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_collect64(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nlong,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pSync)
{
    shmem_collect64((void*)target, (void*)source, nlong,
                    PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_fcollect32(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nlong,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pSync)
{
    shmem_fcollect32((void*)target, (void*)source, nlong,
                     PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_fcollect64(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nlong,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pSync)
{
    shmem_fcollect64((void*)target, (void*)source, nlong,
                     PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_andToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_and_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_andToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_and_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_andToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_and_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_orToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_or_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                          logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_orToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_or_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                          logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_orToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_or_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                          logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_xorToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_xor_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_xorToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_xor_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_xorToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_xor_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_maxToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_max_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_maxToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_max_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_maxToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_max_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_maxToAllFloat(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_float_max_to_all((float*)target, (float*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (float*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_maxToAllDouble(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_double_max_to_all((double*)target, (double*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (double*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_minToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_min_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_minToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_min_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_minToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_min_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_minToAllFloat(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_float_min_to_all((float*)target, (float*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (float*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_minToAllDouble(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_double_min_to_all((double*)target, (double*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (double*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_sumToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_sum_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_sumToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_sum_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_sumToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_sum_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_sumToAllFloat(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_float_sum_to_all((float*)target, (float*)source, nreduce, PE_start,
                           logPE_stride, PE_size, (float*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_sumToAllDouble(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_double_sum_to_all((double*)target, (double*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (double*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_prodToAllShort(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int16_prod_to_all((int16_t*)target, (int16_t*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (int16_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_prodToAllInt(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int32_prod_to_all((int32_t*)target, (int32_t*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (int32_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_prodToAllLong(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_int64_prod_to_all((int64_t*)target, (int64_t*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (int64_t*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_prodToAllFloat(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_float_prod_to_all((float*)target, (float*)source, nreduce, PE_start,
                            logPE_stride, PE_size, (float*)pWrk, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_Addr_prodToAllDouble(
        JNIEnv *env, jobject jthis, jlong target, jlong source, jint nreduce,
        jint PE_start, jint logPE_stride, jint PE_size, jlong pWrk, jlong pSync)
{
    shmem_double_prod_to_all((double*)target, (double*)source, nreduce, PE_start,
                             logPE_stride, PE_size, (double*)pWrk, (long*)pSync);
}
