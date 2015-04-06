#include "oshmem_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "shmem.h"
#include "shmem_ShMem.h"
#include "shmemJava.h"  /* must come AFTER the related .h so JNI is included */

shmem_java_globals_t shmem_java;
int oshmem_shmem_java_eager = 65536;
opal_free_list_t shmem_java_buffers;
static void *liboshmem = NULL;

static void bufferConstructor(shmem_java_buffer_t *item)
{
    item->buffer = malloc(oshmem_shmem_java_eager);
}

static void bufferDestructor(shmem_java_buffer_t *item)
{
    free(item->buffer);
}

OBJ_CLASS_INSTANCE(shmem_java_buffer_t,
                   opal_free_list_item_t,
                   bufferConstructor,
                   bufferDestructor);

static void initFreeList(void)
{
    mca_base_var_register("oshmem", "shmem", "java", "eager",
                          "Java buffers eager size",
                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                          OPAL_INFO_LVL_5,
                          MCA_BASE_VAR_SCOPE_READONLY,
                          &oshmem_shmem_java_eager);

    OBJ_CONSTRUCT(&shmem_java_buffers, opal_free_list_t);

    int r = opal_free_list_init(&shmem_java_buffers,
                                sizeof(shmem_java_buffer_t),
                                opal_cache_line_size,
                                OBJ_CLASS(shmem_java_buffer_t),
                                0, /* payload size */
                                0, /* payload align */
                                2, /* initial elements to alloc */
                                -1, /* max elements */
                                2, /* num elements per alloc */
                                NULL, /* mpool */
                                0, /* mpool reg flags */
                                NULL, /* unused0 */
                                NULL, /* item_init */
                                NULL /* inem_init context */);
    if(r != OPAL_SUCCESS)
    {
        fprintf(stderr, "Unable to initialize shmem_java_buffers.\n");
        exit(1);
    }
}

static jclass findClass(JNIEnv *env, const char *className)
{
    jclass c = (*env)->FindClass(env, className),
           r = (*env)->NewGlobalRef(env, c);

    (*env)->DeleteLocalRef(env, c);
    return r;
}

jint JNI_OnLoad(JavaVM *vm, void *reserved)
{
    /* These are the equivalences between the Java and C types. */
    if( sizeof(jbyte)  != sizeof(char)    ||
        sizeof(jshort) != sizeof(int16_t) ||
        sizeof(jint)   != sizeof(int32_t) ||
        sizeof(jlong)  != sizeof(int64_t))
    {
        fprintf(stderr, "C types not match with Java.\n");
        exit(1);
    }

    liboshmem = dlopen("liboshmem." OPAL_DYN_LIB_SUFFIX,
                       RTLD_NOW | RTLD_GLOBAL);

    if(liboshmem == NULL)
    {
        fprintf(stderr, "Java bindings failed to load liboshmem: %s\n",dlerror());
        exit(1);
    }

    initFreeList();
    return JNI_VERSION_1_6;
}

void JNI_OnUnload(JavaVM *vm, void *reserved)
{
    OBJ_DESTRUCT(&shmem_java_buffers);
    JNIEnv *env;
    (*vm)->GetEnv(vm, (void**)&env, 6);
    (*env)->DeleteGlobalRef(env, shmem_java.ExceptionClass);
    dlclose(liboshmem);
}

JNIEXPORT void JNICALL Java_shmem_ShMem_init(JNIEnv *env, jclass clazz)
{
    shmem_java.ExceptionClass = findClass(env, "shmem/ShMemException");
}

JNIEXPORT void JNICALL Java_shmem_ShMem_startPEs(
        JNIEnv *env, jclass clazz, jint i)
{
    start_pes(i);
}

JNIEXPORT jint JNICALL Java_shmem_ShMem_getNumPEs(JNIEnv *env, jclass clazz)
{
    return _num_pes();
}

JNIEXPORT jint JNICALL Java_shmem_ShMem_getMyPE(JNIEnv *env, jclass clazz)
{
    return _my_pe();
}

JNIEXPORT void JNICALL Java_shmem_ShMem_barrier(
        JNIEnv *env, jclass clazz, jint PE_start,
        jint logPE_stride, jint PE_size, jlong pSync)
{
    shmem_barrier(PE_start, logPE_stride, PE_size, (long*)pSync);
}

JNIEXPORT void JNICALL Java_shmem_ShMem_barrierAll(JNIEnv *env, jclass clazz)
{
    shmem_barrier_all();
}

JNIEXPORT void JNICALL Java_shmem_ShMem_fence(JNIEnv *env, jclass clazz)
{
    shmem_fence();
}

JNIEXPORT void JNICALL Java_shmem_ShMem_quiet(JNIEnv *env, jclass clazz)
{
    shmem_quiet();
}

void* shmem_java_getBuffer(shmem_java_buffer_t **item, JNIEnv *env, int size)
{
    if(size > oshmem_shmem_java_eager)
    {
        *item = NULL;
        return malloc(size);
    }
    else
    {
        opal_free_list_item_t *freeListItem;
        OPAL_FREE_LIST_GET(&shmem_java_buffers, freeListItem);

        if (NULL == item) {
            (*env)->ThrowNew(env, shmem_java.ExceptionClass,
                             "Error in OPAL_FREE_LIST_GET");
        }

        *item = (shmem_java_buffer_t*)freeListItem;
        return (*item)->buffer;
    }
}

void shmem_java_releaseBuffer(void *ptr, shmem_java_buffer_t *item)
{
    if(item == NULL)
    {
        free(ptr);
    }
    else
    {
        assert(item->buffer == ptr);
        OPAL_FREE_LIST_RETURN(&shmem_java_buffers, (opal_free_list_item_t*)item);
    }
}

jbyte* shmem_java_getReadByteArray(shmem_java_buffer_t **item, JNIEnv *env,
                                   jbyteArray array, int off, int len)
{
    jbyte *ptr = shmem_java_getBuffer(item, env, len);
    (*env)->GetByteArrayRegion(env, array, off, len, ptr);
    return ptr;
}

jshort* shmem_java_getReadShortArray(shmem_java_buffer_t **item, JNIEnv *env,
                                     jshortArray array, int off, int len)
{
    jshort *ptr = shmem_java_getBuffer(item, env, len * 2);
    (*env)->GetShortArrayRegion(env, array, off, len, ptr);
    return ptr;
}

jint* shmem_java_getReadIntArray(shmem_java_buffer_t **item, JNIEnv *env,
                                 jintArray array, int off, int len)
{
    jint *ptr = shmem_java_getBuffer(item, env, len * 4);
    (*env)->GetIntArrayRegion(env, array, off, len, ptr);
    return ptr;
}

jlong* shmem_java_getReadLongArray(shmem_java_buffer_t **item, JNIEnv *env,
                                   jlongArray array, int off, int len)
{
    jlong *ptr = shmem_java_getBuffer(item, env, len * 8);
    (*env)->GetLongArrayRegion(env, array, off, len, ptr);
    return ptr;
}

jfloat* shmem_java_getReadFloatArray(shmem_java_buffer_t **item, JNIEnv *env,
                                     jfloatArray array, int off, int len)
{
    jfloat *ptr = shmem_java_getBuffer(item, env, len * 4);
    (*env)->GetFloatArrayRegion(env, array, off, len, ptr);
    return ptr;
}

jdouble* shmem_java_getReadDoubleArray(shmem_java_buffer_t **item, JNIEnv *env,
                                       jdoubleArray array, int off, int len)
{
    jdouble *ptr = shmem_java_getBuffer(item, env, len * 8);
    (*env)->GetDoubleArrayRegion(env, array, off, len, ptr);
    return ptr;
}

void shmem_java_releaseWriteByteArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jbyteArray array, int off, int len)
{
    (*env)->SetByteArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_releaseWriteShortArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jshortArray array, int off, int len)
{
    (*env)->SetShortArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_releaseWriteIntArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jintArray array, int off, int len)
{
    (*env)->SetIntArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_releaseWriteLongArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jlongArray array, int off, int len)
{
    (*env)->SetLongArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_releaseWriteFloatArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jfloatArray array, int off, int len)
{
    (*env)->SetFloatArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_releaseWriteDoubleArray(
        void *ptr, shmem_java_buffer_t *item,
        JNIEnv *env, jdoubleArray array, int off, int len)
{
    (*env)->SetDoubleArrayRegion(env, array, off, len, ptr);
    shmem_java_releaseBuffer(ptr, item);
}

void* shmem_java_iGetBuffer(
        shmem_java_buffer_t **item, JNIEnv *env,
        int stride, int len, int elementSize)
{
    int size = (stride * (len - 1) + 1) * elementSize;
    assert(size >= 0);
    return shmem_java_getBuffer(item, env, size);
}

jshort* shmem_java_iGetReadShortArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jshortArray array, int off, int stride, int len)
{
    jshort *ptr = shmem_java_iGetBuffer(item, env, stride, len, 2),
           *des = ptr,
           *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
           *src = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, JNI_ABORT);
    return ptr;
}

jint* shmem_java_iGetReadIntArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jintArray array, int off, int stride, int len)
{
    jint *ptr = shmem_java_iGetBuffer(item, env, stride, len, 4),
         *des = ptr,
         *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
         *src = arr + off;

    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, JNI_ABORT);
    return ptr;
}

jlong* shmem_java_iGetReadLongArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jlongArray array, int off, int stride, int len)
{
    jlong *ptr = shmem_java_iGetBuffer(item, env, stride, len, 8),
          *des = ptr,
          *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
          *src = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, JNI_ABORT);
    return ptr;
}

jfloat* shmem_java_iGetReadFloatArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jfloatArray array, int off, int stride, int len)
{
    jfloat *ptr = shmem_java_iGetBuffer(item, env, stride, len, 4),
           *des = ptr,
           *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
           *src = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, JNI_ABORT);
    return ptr;
}

jdouble* shmem_java_iGetReadDoubleArray(
        shmem_java_buffer_t **item, JNIEnv *env,
        jdoubleArray array, int off, int stride, int len)
{
    jdouble *ptr = shmem_java_iGetBuffer(item, env, stride, len, 8),
            *des = ptr,
            *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
            *src = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, JNI_ABORT);
    return ptr;
}

void shmem_java_iReleaseWriteShortArray(
        jshort *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jshortArray array, int off, int stride, int len)
{
    jshort *src = ptr,
           *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
           *des = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, 0);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_iReleaseWriteIntArray(
        jint *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jintArray array, int off, int stride, int len)
{
    jint *src = ptr,
         *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
         *des = arr + off;

    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, 0);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_iReleaseWriteLongArray(
        jlong *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jlongArray array, int off, int stride, int len)
{
    jlong *src = ptr,
          *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
          *des = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, 0);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_iReleaseWriteFloatArray(
        jfloat *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jfloatArray array, int off, int stride, int len)
{
    jfloat *src = ptr,
           *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
           *des = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, 0);
    shmem_java_releaseBuffer(ptr, item);
}

void shmem_java_iReleaseWriteDoubleArray(
        jdouble *ptr, shmem_java_buffer_t *item, JNIEnv *env,
        jdoubleArray array, int off, int stride, int len)
{
    jdouble *src = ptr,
            *arr = (*env)->GetPrimitiveArrayCritical(env, array, NULL),
            *des = arr + off;
    int i;
    for(i = 0; i < len; i++)
    {
        *des = *src;
        des += stride;
        src += stride;
    }

    (*env)->ReleasePrimitiveArrayCritical(env, array, arr, 0);
    shmem_java_releaseBuffer(ptr, item);
}
