#ifndef _SHMEMJAVA_H_
#define _SHMEMJAVA_H_

#include "shmem.h"
#include "opal/class/opal_free_list.h"

typedef struct {
    jfieldID AddrHandle;
    jclass   ExceptionClass;
} shmem_java_globals_t;

extern shmem_java_globals_t shmem_java;

typedef struct shmem_java_buffer_t
{
    opal_free_list_item_t super;
    void *buffer;
} shmem_java_buffer_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(shmem_java_buffer_t);

void* shmem_java_getBuffer(shmem_java_buffer_t **item, JNIEnv *env, int size);
void shmem_java_releaseBuffer(void *ptr, shmem_java_buffer_t *item);

jbyte* shmem_java_getReadByteArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jbyteArray array, int off, int len);
jshort* shmem_java_getReadShortArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jshortArray array, int off, int len);
jint* shmem_java_getReadIntArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jintArray array, int off, int len);
jlong* shmem_java_getReadLongArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jlongArray array, int off, int len);
jfloat* shmem_java_getReadFloatArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jfloatArray array, int off, int len);
jdouble* shmem_java_getReadDoubleArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jdoubleArray array, int off, int len);

void shmem_java_releaseWriteByteArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jbyteArray array, int off, int len);
void shmem_java_releaseWriteShortArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jshortArray array, int off, int len);
void shmem_java_releaseWriteIntArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jintArray array, int off, int len);
void shmem_java_releaseWriteLongArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jlongArray array, int off, int len);
void shmem_java_releaseWriteFloatArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jfloatArray array, int off, int len);
void shmem_java_releaseWriteDoubleArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jdoubleArray array, int off, int len);

void* shmem_java_iGetBuffer(
        shmem_java_buffer_t **item, JNIEnv *env,
        int stride, int len, int elementSize);

jshort* shmem_java_iGetReadShortArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jshortArray array, int off, int stride, int len);
jint* shmem_java_iGetReadIntArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jintArray array, int off, int stride, int len);
jlong* shmem_java_iGetReadLongArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jlongArray array, int off, int stride, int len);
jfloat* shmem_java_iGetReadFloatArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jfloatArray array, int off, int stride, int len);
jdouble* shmem_java_iGetReadDoubleArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jdoubleArray array, int off, int stride, int len);

void shmem_java_iReleaseWriteShortArray(
        jshort *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jshortArray array, int off, int stride, int len);
void shmem_java_iReleaseWriteIntArray(
        jint *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jintArray array, int off, int stride, int len);
void shmem_java_iReleaseWriteLongArray(
        jlong *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jlongArray array, int off, int stride, int len);
void shmem_java_iReleaseWriteFloatArray(
        jfloat *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jfloatArray array, int off, int stride, int len);
void shmem_java_iReleaseWriteDoubleArray(
        jdouble *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jdoubleArray array, int off, int stride, int len);

#endif /* _SHMEMJAVA_H_ */
