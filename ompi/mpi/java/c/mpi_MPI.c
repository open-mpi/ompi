/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/*
 * This file is almost a complete re-write for Open MPI compared to the
 * original mpiJava package. Its license and copyright are listed below.
 * See <path to ompi/mpi/java/README> for more information.
 */
/*
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * File         : mpi_MPI.c
 * Headerfile   : mpi_MPI.h
 * Author       : SungHoon Ko, Xinying Li (contributions from MAEDA Atusi)
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.17 $
 * Updated      : $Date: 2003/01/17 01:50:37 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#include <poll.h>
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "opal/util/output.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/base/mca_base_var.h"

#include "mpi.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/errhandler/errcode-internal.h"
#include "ompi/datatype/ompi_datatype.h"
#include "mpi_MPI.h"
#include "mpiJava.h"

ompi_java_globals_t ompi_java = {0};
int ompi_mpi_java_eager = 65536;
opal_free_list_t ompi_java_buffers = {{{0}}};
static void *libmpi = NULL;

static void bufferConstructor(ompi_java_buffer_t *item)
{
    item->buffer = malloc(ompi_mpi_java_eager);
}

static void bufferDestructor(ompi_java_buffer_t *item)
{
    free(item->buffer);
}

OBJ_CLASS_INSTANCE(ompi_java_buffer_t,
                   opal_free_list_item_t,
                   bufferConstructor,
                   bufferDestructor);

/*
 * Class:    mpi_MPI
 * Method:   loadGlobalLibraries
 *
 * Java implementations typically default to loading dynamic
 * libraries strictly to a local namespace. This breaks the
 * Open MPI model where components reference back up to the
 * base libraries (e.g., libmpi) as it requires that the
 * symbols in those base libraries be globally available.
 *
 * One option, of course, is to build with --disable-dlopen.
 * However, this would preclude the ability to pickup 3rd-party
 * binary plug-ins at time of execution. This is a valuable
 * capability that would be a negative factor towards use of
 * the Java bindings.
 *
 * The other option is to explicitly dlopen libmpi ourselves
 * and instruct dlopen to add all those symbols to the global
 * namespace. This must be done prior to calling any MPI
 * function (e.g., MPI_Init) or else Java will have already
 * loaded the library to the local namespace. So create a
 * special JNI entry point that just loads the required libmpi
 * to the global namespace and call it first (see MPI.java),
 * thus making all symbols available to subsequent dlopen calls
 * when opening OMPI components.
 */
jint JNI_OnLoad(JavaVM *vm, void *reserved)
{
    libmpi = dlopen("libmpi." OPAL_DYN_LIB_SUFFIX, RTLD_NOW | RTLD_GLOBAL);

#if defined(HAVE_DL_INFO) && defined(HAVE_LIBGEN_H)
    /*
     * OS X El Capitan does not propagate DYLD_LIBRARY_PATH to children any more
     * so if previous dlopen failed, try to open libmpi in the same directory
     * than the current libmpi_java
     */
    if(NULL == libmpi) {
        Dl_info info;
        if(0 != dladdr((void *)JNI_OnLoad, &info)) {
            char libmpipath[OPAL_PATH_MAX];
            char *libmpijavapath = strdup(info.dli_fname);
            if (NULL != libmpijavapath) {
                snprintf(libmpipath, OPAL_PATH_MAX-1, "%s/libmpi." OPAL_DYN_LIB_SUFFIX, dirname(libmpijavapath));
                free(libmpijavapath);
                libmpi = dlopen(libmpipath, RTLD_NOW | RTLD_GLOBAL);
            }
        }
    }
#endif

    if(NULL == libmpi)
    {
        fprintf(stderr, "Java bindings failed to load libmpi: %s\n",dlerror());
        exit(1);
    }

    return JNI_VERSION_1_6;
}

void JNI_OnUnload(JavaVM *vm, void *reserved)
{
    if(libmpi != NULL)
        dlclose(libmpi);
}

static void initFreeList(void)
{
    OBJ_CONSTRUCT(&ompi_java_buffers, opal_free_list_t);

    int r = opal_free_list_init(&ompi_java_buffers,
                                sizeof(ompi_java_buffer_t),
                                opal_cache_line_size,
                                OBJ_CLASS(ompi_java_buffer_t),
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
        fprintf(stderr, "Unable to initialize ompi_java_buffers.\n");
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

static void findClasses(JNIEnv *env)
{
    ompi_java.CartParmsClass  = findClass(env, "mpi/CartParms");
    ompi_java.ShiftParmsClass = findClass(env, "mpi/ShiftParms");
    ompi_java.GraphParmsClass = findClass(env, "mpi/GraphParms");

    ompi_java.DistGraphNeighborsClass = findClass(
                                        env, "mpi/DistGraphNeighbors");

    ompi_java.StatusClass    = findClass(env, "mpi/Status");
    ompi_java.ExceptionClass = findClass(env, "mpi/MPIException");

    ompi_java.ExceptionInit = (*env)->GetMethodID(
                              env, ompi_java.ExceptionClass,
                              "<init>", "(IILjava/lang/String;)V");

    ompi_java.IntegerClass = findClass(env, "java/lang/Integer");
    ompi_java.LongClass    = findClass(env, "java/lang/Long");

    ompi_java.IntegerValueOf = (*env)->GetStaticMethodID(
            env, ompi_java.IntegerClass, "valueOf", "(I)Ljava/lang/Integer;");
    ompi_java.LongValueOf = (*env)->GetStaticMethodID(
            env, ompi_java.LongClass, "valueOf", "(J)Ljava/lang/Long;");
}

static void deleteClasses(JNIEnv *env)
{
    (*env)->DeleteGlobalRef(env, ompi_java.CartParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.ShiftParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.VersionClass);
    (*env)->DeleteGlobalRef(env, ompi_java.CountClass);
    (*env)->DeleteGlobalRef(env, ompi_java.GraphParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.DistGraphNeighborsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.StatusClass);
    (*env)->DeleteGlobalRef(env, ompi_java.ExceptionClass);
    (*env)->DeleteGlobalRef(env, ompi_java.IntegerClass);
    (*env)->DeleteGlobalRef(env, ompi_java.LongClass);
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_newInt2(JNIEnv *env, jclass clazz)
{
    struct { int a; int b; } s;
    int iOff = (int)((MPI_Aint)(&(s.b)) - (MPI_Aint)(&(s.a)));
    jclass c = (*env)->FindClass(env, "mpi/Int2");
    jmethodID m = (*env)->GetMethodID(env, c, "<init>", "(II)V");
    return (*env)->NewObject(env, c, m, iOff, sizeof(int));
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_newShortInt(JNIEnv *env, jclass clazz)
{
    struct { short a; int b; } s;
    int iOff = (int)((MPI_Aint)(&(s.b)) - (MPI_Aint)(&(s.a)));
    jclass c = (*env)->FindClass(env, "mpi/ShortInt");
    jmethodID m = (*env)->GetMethodID(env, c, "<init>", "(III)V");
    return (*env)->NewObject(env, c, m, sizeof(short), iOff, sizeof(int));
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_newLongInt(JNIEnv *env, jclass clazz)
{
    struct { long a; int b; } s;
    int iOff = (int)((MPI_Aint)(&(s.b)) - (MPI_Aint)(&(s.a)));
    jclass c = (*env)->FindClass(env, "mpi/LongInt");
    jmethodID m = (*env)->GetMethodID(env, c, "<init>", "(III)V");
    return (*env)->NewObject(env, c, m, sizeof(long), iOff, sizeof(int));
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_newFloatInt(JNIEnv *env, jclass clazz)
{
    struct { float a; int b; } s;
    int iOff = (int)((MPI_Aint)(&(s.b)) - (MPI_Aint)(&(s.a)));
    jclass c = (*env)->FindClass(env, "mpi/FloatInt");
    jmethodID m = (*env)->GetMethodID(env, c, "<init>", "(II)V");
    return (*env)->NewObject(env, c, m, iOff, sizeof(int));
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_newDoubleInt(JNIEnv *env, jclass clazz)
{
    struct { double a; int b; } s;
    int iOff = (int)((MPI_Aint)(&(s.b)) - (MPI_Aint)(&(s.a)));
    jclass c = (*env)->FindClass(env, "mpi/DoubleInt");
    jmethodID m = (*env)->GetMethodID(env, c, "<init>", "(II)V");
    return (*env)->NewObject(env, c, m, iOff, sizeof(int));
}

JNIEXPORT void JNICALL Java_mpi_MPI_initVersion(JNIEnv *env, jclass jthis)
{
    ompi_java.VersionClass = findClass(env, "mpi/Version");
    ompi_java.VersionInit = (*env)->GetMethodID(env, ompi_java.VersionClass, "<init>", "(II)V");
}

JNIEXPORT jobjectArray JNICALL Java_mpi_MPI_Init_1jni(
        JNIEnv *env, jclass clazz, jobjectArray argv)
{
    jsize i;
    jclass string;
    jobject value;

    int len = (*env)->GetArrayLength(env, argv);
    char **sargs = (char**)calloc(len+1, sizeof(char*));

    for(i = 0; i < len; i++)
    {
        jstring jc = (jstring)(*env)->GetObjectArrayElement(env, argv, i);
        const char *s = (*env)->GetStringUTFChars(env, jc, NULL);
        sargs[i] = strdup(s);
        (*env)->ReleaseStringUTFChars(env, jc, s);
        (*env)->DeleteLocalRef(env, jc);
    }

    int rc = MPI_Init(&len, &sargs);
    ompi_java_exceptionCheck(env, rc);
    mca_base_var_register("ompi", "mpi", "java", "eager",
                          "Java buffers eager size",
                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                          OPAL_INFO_LVL_5,
                          MCA_BASE_VAR_SCOPE_READONLY,
                          &ompi_mpi_java_eager);

    string = (*env)->FindClass(env, "java/lang/String");
    value = (*env)->NewObjectArray(env, len, string, NULL);

    for(i = 0; i < len; i++)
    {
        jstring jc = (*env)->NewStringUTF(env, sargs[i]);
        (*env)->SetObjectArrayElement(env, value, i, jc);
        (*env)->DeleteLocalRef(env, jc);
        free (sargs[i]);
    }

    free (sargs);

    findClasses(env);
    initFreeList();
    return value;
}

JNIEXPORT jint JNICALL Java_mpi_MPI_InitThread_1jni(
        JNIEnv *env, jclass clazz, jobjectArray argv, jint required)
{
    jsize i;
    int len = (*env)->GetArrayLength(env,argv);
    char **sargs = (char**)calloc(len+1, sizeof(char*));

    for(i = 0; i < len; i++)
    {
        jstring jc = (jstring)(*env)->GetObjectArrayElement(env, argv, i);
        const char *s = (*env)->GetStringUTFChars(env, jc, 0);
        sargs[i] = strdup(s);
        (*env)->ReleaseStringUTFChars(env, jc, s);
        (*env)->DeleteLocalRef(env, jc);
    }

    int provided;
    int rc = MPI_Init_thread(&len, &sargs, required, &provided);
    ompi_java_exceptionCheck(env, rc);

    findClasses(env);
    initFreeList();
    return provided;
}

JNIEXPORT jint JNICALL Java_mpi_MPI_queryThread_1jni(JNIEnv *env, jclass clazz)
{
    int provided;
    int rc = MPI_Query_thread(&provided);
    ompi_java_exceptionCheck(env, rc);
    return provided;
}

JNIEXPORT jboolean JNICALL Java_mpi_MPI_isThreadMain_1jni(
                           JNIEnv *env, jclass clazz)
{
    int flag;
    int rc = MPI_Is_thread_main(&flag);
    ompi_java_exceptionCheck(env, rc);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_MPI_Finalize_1jni(JNIEnv *env, jclass obj)
{
    OBJ_DESTRUCT(&ompi_java_buffers);
    int rc = MPI_Finalize();
    ompi_java_exceptionCheck(env, rc);
    deleteClasses(env);
}

JNIEXPORT jobject JNICALL Java_mpi_MPI_getVersionJNI(JNIEnv *env, jclass jthis)
{
	int version, subversion;
	int rc = MPI_Get_version(&version, &subversion);
	ompi_java_exceptionCheck(env, rc);

	return (*env)->NewObject(env, ompi_java.VersionClass,
	                             ompi_java.VersionInit, version, subversion);
}

JNIEXPORT jstring JNICALL Java_mpi_MPI_getLibVersionJNI(JNIEnv *env, jclass jthis)
{
	int length;
	char version[MPI_MAX_LIBRARY_VERSION_STRING];
	int rc = MPI_Get_library_version(version, &length);
	ompi_java_exceptionCheck(env, rc);

	return (*env)->NewStringUTF(env, version);
}

JNIEXPORT jint JNICALL Java_mpi_MPI_getProcessorName(
                       JNIEnv *env, jclass obj, jbyteArray buf)
{
    int len;
    jbyte* bufc = (jbyte*)((*env)->GetByteArrayElements(env, buf, NULL));
    int rc = MPI_Get_processor_name((char*)bufc, &len);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseByteArrayElements(env, buf, bufc, 0);
    return len;
}

JNIEXPORT jdouble JNICALL Java_mpi_MPI_wtime_1jni(JNIEnv *env, jclass jthis)
{
    return MPI_Wtime();
}

JNIEXPORT jdouble JNICALL Java_mpi_MPI_wtick_1jni(JNIEnv *env, jclass jthis)
{
    return MPI_Wtick();
}

JNIEXPORT jboolean JNICALL Java_mpi_MPI_isInitialized(JNIEnv *env, jclass jthis)
{
    int flag;
    int rc = MPI_Initialized(&flag);
    ompi_java_exceptionCheck(env, rc);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jboolean JNICALL Java_mpi_MPI_isFinalized(JNIEnv *env, jclass jthis)
{
    int flag;
    int rc = MPI_Finalized(&flag);
    ompi_java_exceptionCheck(env, rc);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_MPI_attachBuffer_1jni(
                       JNIEnv *env, jclass jthis, jbyteArray buf)
{
    int size=(*env)->GetArrayLength(env,buf);
    jbyte* bufptr = (*env)->GetByteArrayElements(env, buf, NULL);
    int rc = MPI_Buffer_attach(bufptr,size);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT void JNICALL Java_mpi_MPI_detachBuffer_1jni(
                       JNIEnv *env, jclass jthis, jbyteArray buf)
{
    int size;
    jbyte* bufptr;
    int rc = MPI_Buffer_detach(&bufptr, &size);
    ompi_java_exceptionCheck(env, rc);

    if(buf != NULL)
        (*env)->ReleaseByteArrayElements(env,buf,bufptr,0);
}

void* ompi_java_getArrayCritical(void** bufBase, JNIEnv *env,
                                 jobject buf, int offset)
{
    *bufBase = (jbyte*)(*env)->GetPrimitiveArrayCritical(env, buf, NULL);
    return ((jbyte*)*bufBase) + offset;
}

void* ompi_java_getDirectBufferAddress(JNIEnv *env, jobject buf)
{
    /* Allow NULL buffers to send/recv 0 items as control messages. */
    return buf == NULL ? NULL : (*env)->GetDirectBufferAddress(env, buf);
}

static int getTypeExtent(JNIEnv *env, MPI_Datatype type)
{
    MPI_Aint lb, extent;
    int rc = MPI_Type_get_extent(type, &lb, &extent);
    ompi_java_exceptionCheck(env, rc);
    int value = extent;
    assert(((MPI_Aint)value) == extent);
    return value;
}

static void getArrayRegion(JNIEnv *env, jobject buf, int baseType,
                           int offset, int length, void *ptr)
{
    switch(baseType)
    {
        case 0:
            break;
        case 1:
            (*env)->GetByteArrayRegion(env, buf, offset, length, ptr);
            break;
        case 2:
            (*env)->GetCharArrayRegion(env, buf, offset / 2, length / 2, ptr);
            break;
        case 3:
            (*env)->GetShortArrayRegion(env, buf, offset / 2, length / 2, ptr);
            break;
        case 4:
            (*env)->GetBooleanArrayRegion(env, buf, offset, length, ptr);
            break;
        case 5:
            (*env)->GetIntArrayRegion(env, buf, offset / 4, length / 4, ptr);
            break;
        case 6:
            (*env)->GetLongArrayRegion(env, buf, offset / 8, length / 8, ptr);
            break;
        case 7:
            (*env)->GetFloatArrayRegion(env, buf, offset / 4, length / 4, ptr);
            break;
        case 8:
            (*env)->GetDoubleArrayRegion(env, buf, offset / 8, length / 8, ptr);
            break;
        case 9:
            (*env)->GetByteArrayRegion(env, buf, offset, length, ptr);
            break;
        default:
            assert(0);
    }
}

static void setArrayRegion(JNIEnv *env, jobject buf, int baseType,
                           int offset, int length, void *ptr)
{
    switch(baseType)
    {
        case 0:
            break;
        case 1:
            (*env)->SetByteArrayRegion(env, buf, offset, length, ptr);
            break;
        case 2:
            (*env)->SetCharArrayRegion(env, buf, offset / 2, length / 2, ptr);
            break;
        case 3:
            (*env)->SetShortArrayRegion(env, buf, offset / 2, length / 2, ptr);
            break;
        case 4:
            (*env)->SetBooleanArrayRegion(env, buf, offset, length, ptr);
            break;
        case 5:
            (*env)->SetIntArrayRegion(env, buf, offset / 4, length / 4, ptr);
            break;
        case 6:
            (*env)->SetLongArrayRegion(env, buf, offset / 8, length / 8, ptr);
            break;
        case 7:
            (*env)->SetFloatArrayRegion(env, buf, offset / 4, length / 4, ptr);
            break;
        case 8:
            (*env)->SetDoubleArrayRegion(env, buf, offset / 8, length / 8, ptr);
            break;
        case 9:
            (*env)->SetByteArrayRegion(env, buf, offset, length, ptr);
            break;
        default:
            assert(0);
    }
}

static void* getBuffer(JNIEnv *env, ompi_java_buffer_t **item, int size)
{
    if(size > ompi_mpi_java_eager)
    {
        *item = NULL;
        return malloc(size);
    }
    else
    {
        opal_free_list_item_t *freeListItem;
        freeListItem = opal_free_list_get (&ompi_java_buffers);

        ompi_java_exceptionCheck(env, NULL == freeListItem ? MPI_ERR_NO_MEM :
                                 MPI_SUCCESS);
        if (NULL == freeListItem) {
            return NULL;
        }

        *item = (ompi_java_buffer_t*)freeListItem;
        return (*item)->buffer;
    }
}

static void releaseBuffer(void *ptr, ompi_java_buffer_t *item)
{
    if(item == NULL)
    {
        free(ptr);
    }
    else
    {
        assert(item->buffer == ptr);
        opal_free_list_return (&ompi_java_buffers, (opal_free_list_item_t*)item);
    }
}

static int getCountv(int *counts, int *displs, int size)
{
    /* Maybe displs is not ordered. */
    int i, max = 0;

    for(i = 1; i < size; i++)
    {
        if(displs[max] < displs[i])
            max = i;
    }

    return displs[max] * counts[max];
}

static void* getReadPtr(ompi_java_buffer_t **item, JNIEnv *env, jobject buf,
                        int offset, int count, MPI_Datatype type, int baseType)
{
    int  length = count * getTypeExtent(env, type);
    void *ptr   = getBuffer(env, item, length);

    if(opal_datatype_is_contiguous_memory_layout(&type->super, count))
    {
        getArrayRegion(env, buf, baseType, offset, length, ptr);
    }
    else
    {
        void *inBuf, *inBase;
        inBuf = ompi_java_getArrayCritical(&inBase, env, buf, offset);

        int rc = opal_datatype_copy_content_same_ddt(
                 &type->super, count, ptr, inBuf);

        ompi_java_exceptionCheck(env,
                rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);

        (*env)->ReleasePrimitiveArrayCritical(env, buf, inBase, JNI_ABORT);
    }

    return ptr;
}

static void* getReadPtrRank(
        ompi_java_buffer_t **item, JNIEnv *env, jobject buf, int offset,
        int count, int size, int rank, MPI_Datatype type, int baseType)
{
    int  extent = getTypeExtent(env, type),
         rLen   = extent * count,
         length = rLen * size,
         rDispl = rLen * rank,
         rOff   = offset + rDispl;
    void *ptr   = getBuffer(env, item, length);
    void *rPtr  = (char*)ptr + rDispl;

    if(opal_datatype_is_contiguous_memory_layout(&type->super, count))
    {
        getArrayRegion(env, buf, baseType, rOff, rLen, rPtr);
    }
    else
    {
        void *bufPtr, *bufBase;
        bufPtr = ompi_java_getArrayCritical(&bufBase, env, buf, rOff);

        int rc = opal_datatype_copy_content_same_ddt(
                 &type->super, count, rPtr, bufPtr);

        ompi_java_exceptionCheck(env,
                rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);

        (*env)->ReleasePrimitiveArrayCritical(env, buf, bufBase, JNI_ABORT);
    }

    return ptr;
}

static void* getReadPtrvRank(
        ompi_java_buffer_t **item, JNIEnv *env, jobject buf,
        int offset, int *counts, int *displs, int size,
        int rank, MPI_Datatype type, int baseType)
{
    int  extent  = getTypeExtent(env, type),
         length  = extent * getCountv(counts, displs, size);
    void *ptr    = getBuffer(env, item, length);
    int  rootOff = offset + extent * displs[rank];

    if(opal_datatype_is_contiguous_memory_layout(&type->super, counts[rank]))
    {
        int  rootLength = extent * counts[rank];
        void *rootPtr   = (char*)ptr + extent * displs[rank];
        getArrayRegion(env, buf, baseType, rootOff, rootLength, rootPtr);
    }
    else
    {
        void *inBuf, *inBase;
        inBuf = ompi_java_getArrayCritical(&inBase, env, buf, rootOff);

        int rc = opal_datatype_copy_content_same_ddt(
                 &type->super, counts[rank], ptr, inBuf);

        ompi_java_exceptionCheck(env,
                rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);

        (*env)->ReleasePrimitiveArrayCritical(env, buf, inBase, JNI_ABORT);
    }

    return ptr;
}

static void* getReadPtrvAll(
        ompi_java_buffer_t **item, JNIEnv *env, jobject buf,
        int offset, int *counts, int *displs, int size,
        MPI_Datatype type, int baseType)
{
    int  i,
         extent  = getTypeExtent(env, type),
         length  = extent * getCountv(counts, displs, size);
    void *ptr    = getBuffer(env, item, length);

    if(opal_datatype_is_contiguous_memory_layout(&type->super, 2))
    {
        for(i = 0; i < size; i++)
        {
            int   iOff = offset + extent * displs[i],
                  iLen = extent * counts[i];
            void *iPtr = (char*)ptr + extent * displs[i];
            getArrayRegion(env, buf, baseType, iOff, iLen, iPtr);
        }
    }
    else
    {
        void *bufPtr, *bufBase;
        bufPtr = ompi_java_getArrayCritical(&bufBase, env, buf, offset);

        for(i = 0; i < size; i++)
        {
            int   iOff = extent * displs[i];
            char *iBuf = iOff + (char*)bufPtr,
                 *iPtr = iOff + (char*)ptr;

            int rc = opal_datatype_copy_content_same_ddt(
                     &type->super, counts[i], iPtr, iBuf);

            ompi_java_exceptionCheck(env,
                    rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);
        }

        (*env)->ReleasePrimitiveArrayCritical(env, buf, bufBase, JNI_ABORT);
    }

    return ptr;
}

static void* getWritePtr(ompi_java_buffer_t **item, JNIEnv *env,
                         int count, MPI_Datatype type)
{
    int extent = getTypeExtent(env, type),
        length = count * extent;

    return getBuffer(env, item, length);
}

static void* getWritePtrv(ompi_java_buffer_t **item, JNIEnv *env,
                          int *counts, int *displs, int size, MPI_Datatype type)
{
    int extent = getTypeExtent(env, type),
        count  = getCountv(counts, displs, size),
        length = extent * count;

    return getBuffer(env, item, length);
}

void ompi_java_getReadPtr(
        void **ptr, ompi_java_buffer_t **item, JNIEnv *env, jobject buf,
        jboolean db, int offset, int count, MPI_Datatype type, int baseType)
{
    if(buf == NULL || baseType == 0)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *ptr  = NULL;
        *item = NULL;
    }
    else if(db)
    {
        assert(offset == 0);
        *ptr  = (*env)->GetDirectBufferAddress(env, buf);
        *item = NULL;
    }
    else
    {
        *ptr = getReadPtr(item, env, buf, offset, count, type, baseType);
    }
}

void ompi_java_getReadPtrRank(
        void **ptr, ompi_java_buffer_t **item, JNIEnv *env,
        jobject buf, jboolean db, int offset, int count, int size,
        int rank, MPI_Datatype type, int baseType)
{
    if(buf == NULL || baseType == 0)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *ptr  = NULL;
        *item = NULL;
    }
    else if(db)
    {
        assert(offset == 0);
        *ptr  = (*env)->GetDirectBufferAddress(env, buf);
        *item = NULL;
    }
    else
    {
        *ptr = getReadPtrRank(item, env, buf, offset, count,
                              size, rank, type, baseType);
    }
}

void ompi_java_getReadPtrv(
        void **ptr, ompi_java_buffer_t **item, JNIEnv *env,
        jobject buf, jboolean db, int offset, int *counts, int *displs,
        int size, int rank, MPI_Datatype type, int baseType)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *ptr  = NULL;
        *item = NULL;
    }
    else if(db)
    {
        assert(offset == 0);
        *ptr  = (*env)->GetDirectBufferAddress(env, buf);
        *item = NULL;
    }
    else if(rank == -1)
    {
        *ptr = getReadPtrvAll(item, env, buf, offset, counts,
                              displs, size, type, baseType);
    }
    else
    {
        *ptr = getReadPtrvRank(item, env, buf, offset, counts,
                               displs, size, rank, type, baseType);
    }
}

void ompi_java_releaseReadPtr(
        void *ptr, ompi_java_buffer_t *item, jobject buf, jboolean db)
{
    if(!db && buf && ptr)
        releaseBuffer(ptr, item);
}

void ompi_java_getWritePtr(
        void **ptr, ompi_java_buffer_t **item, JNIEnv *env,
        jobject buf, jboolean db, int count, MPI_Datatype type)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *ptr  = NULL;
        *item = NULL;
    }
    else if(db)
    {
        *ptr  = (*env)->GetDirectBufferAddress(env, buf);
        *item = NULL;
    }
    else
    {
        *ptr = getWritePtr(item, env, count, type);
    }
}

void ompi_java_getWritePtrv(
        void **ptr, ompi_java_buffer_t **item, JNIEnv *env, jobject buf,
        jboolean db, int *counts, int *displs, int size, MPI_Datatype type)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *ptr  = NULL;
        *item = NULL;
    }
    else if(db)
    {
        *ptr  = (*env)->GetDirectBufferAddress(env, buf);
        *item = NULL;
    }
    else
    {
        *ptr = getWritePtrv(item, env, counts, displs, size, type);
    }
}

void ompi_java_releaseWritePtr(
        void *ptr, ompi_java_buffer_t *item, JNIEnv *env, jobject buf,
        jboolean db, int offset, int count, MPI_Datatype type, int baseType)
{
    if(db || !buf || !ptr)
        return;

    if(opal_datatype_is_contiguous_memory_layout(&type->super, count))
    {
        int length = count * getTypeExtent(env, type);
        setArrayRegion(env, buf, baseType, offset, length, ptr);
    }
    else
    {
        void *inBuf, *inBase;
        inBuf = ompi_java_getArrayCritical(&inBase, env, buf, offset);

        int rc = opal_datatype_copy_content_same_ddt(
                 &type->super, count, inBuf, ptr);

        ompi_java_exceptionCheck(env,
                rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);

        (*env)->ReleasePrimitiveArrayCritical(env, buf, inBase, 0);
    }

    releaseBuffer(ptr, item);
}

void ompi_java_releaseWritePtrv(
        void *ptr, ompi_java_buffer_t *item, JNIEnv *env,
        jobject buf, jboolean db, int offset, int *counts, int *displs,
        int size, MPI_Datatype type, int baseType)
{
    if(db || !buf || !ptr)
        return;

    int i;
    int extent = getTypeExtent(env, type);

    if(opal_datatype_is_contiguous_memory_layout(&type->super, 2))
    {
        for(i = 0; i < size; i++)
        {
            int   iOff = offset + extent * displs[i],
                  iLen = extent * counts[i];
            void *iPtr = (char*)ptr + extent * displs[i];
            setArrayRegion(env, buf, baseType, iOff, iLen, iPtr);
        }
    }
    else
    {
        void *bufPtr, *bufBase;
        bufPtr = ompi_java_getArrayCritical(&bufBase, env, buf, offset);

        for(i = 0; i < size; i++)
        {
            int   iOff = extent * displs[i];
            char *iBuf = iOff + (char*)bufPtr,
                 *iPtr = iOff + (char*)ptr;

            int rc = opal_datatype_copy_content_same_ddt(
                     &type->super, counts[i], iBuf, iPtr);

            ompi_java_exceptionCheck(env,
                    rc==OPAL_SUCCESS ? OMPI_SUCCESS : OMPI_ERROR);
        }

        (*env)->ReleasePrimitiveArrayCritical(env, buf, bufBase, 0);
    }

    releaseBuffer(ptr, item);
}

jobject ompi_java_Integer_valueOf(JNIEnv *env, jint i)
{
    return (*env)->CallStaticObjectMethod(env,
           ompi_java.IntegerClass, ompi_java.IntegerValueOf, i);
}

jobject ompi_java_Long_valueOf(JNIEnv *env, jlong i)
{
    return (*env)->CallStaticObjectMethod(env,
           ompi_java.LongClass, ompi_java.LongValueOf, i);
}

void ompi_java_getIntArray(JNIEnv *env, jintArray array,
                           jint **jptr, int **cptr)
{
    jint *jInts = (*env)->GetIntArrayElements(env, array, NULL);
    *jptr = jInts;

    if(sizeof(int) == sizeof(jint))
    {
        *cptr = (int*)jInts;
    }
    else
    {
        int i, length = (*env)->GetArrayLength(env, array);
        int *cInts = calloc(length, sizeof(int));

        for(i = 0; i < length; i++)
            cInts[i] = jInts[i];

        *cptr = cInts;
    }
}

void ompi_java_releaseIntArray(JNIEnv *env, jintArray array,
                               jint *jptr, int *cptr)
{
    if(jptr != cptr)
    {
        int i, length = (*env)->GetArrayLength(env, array);

        for(i = 0; i < length; i++)
            jptr[i] = cptr[i];

        free(cptr);
    }

    (*env)->ReleaseIntArrayElements(env, array, jptr, 0);
}

void ompi_java_forgetIntArray(JNIEnv *env, jintArray array,
                              jint *jptr, int *cptr)
{
    if(jptr != cptr)
        free(cptr);

    (*env)->ReleaseIntArrayElements(env, array, jptr, JNI_ABORT);
}

void ompi_java_getDatatypeArray(JNIEnv *env, jlongArray array,
                           jlong **jptr, MPI_Datatype **cptr)
{
    jlong *jLongs = (*env)->GetLongArrayElements(env, array, NULL);
    *jptr = jLongs;

    int i, length = (*env)->GetArrayLength(env, array);
    MPI_Datatype *cDatatypes = calloc(length, sizeof(MPI_Datatype));

    for(i = 0; i < length; i++){
        cDatatypes[i] = (MPI_Datatype)jLongs[i];
    }
    *cptr = cDatatypes;
}

void ompi_java_forgetDatatypeArray(JNIEnv *env, jlongArray array,
                              jlong *jptr, MPI_Datatype *cptr)
{
    if((long)jptr != (long)cptr)
        free(cptr);

    (*env)->ReleaseLongArrayElements(env, array, jptr, JNI_ABORT);
}

void ompi_java_getBooleanArray(JNIEnv *env, jbooleanArray array,
                               jboolean **jptr, int **cptr)
{
    int i, length = (*env)->GetArrayLength(env, array);
    jboolean *jb = (*env)->GetBooleanArrayElements(env, array, NULL);
    int *cb = (int*)calloc(length, sizeof(int));

    for(i = 0; i < length; i++)
        cb[i] = jb[i];

    *jptr = jb;
    *cptr = cb;
}

void ompi_java_releaseBooleanArray(JNIEnv *env, jbooleanArray array,
                                   jboolean *jptr, int *cptr)
{
    int i, length = (*env)->GetArrayLength(env, array);

    for(i = 0; i < length; i++)
        jptr[i] = cptr[i] ? JNI_TRUE : JNI_FALSE;

    free(cptr);
    (*env)->ReleaseBooleanArrayElements(env, array, jptr, 0);
}

void ompi_java_forgetBooleanArray(JNIEnv *env, jbooleanArray array,
                                  jboolean *jptr, int *cptr)
{
    free(cptr);
    (*env)->ReleaseBooleanArrayElements(env, array, jptr, JNI_ABORT);
}

void ompi_java_getPtrArray(JNIEnv *env, jlongArray array,
                           jlong **jptr, void ***cptr)
{
    jlong *jp = *jptr = (*env)->GetLongArrayElements(env, array, NULL);

    if(sizeof(jlong) == sizeof(void*))
    {
        *cptr = (void**)jp;
    }
    else
    {
        int i, length = (*env)->GetArrayLength(env, array);
        void **cp = *cptr = calloc(length, sizeof(void*));

        for(i = 0; i < length; i++)
            cp[i] = (void*)jp[i];
    }
}

void ompi_java_releasePtrArray(JNIEnv *env, jlongArray array,
                               jlong *jptr, void **cptr)
{
    if(jptr != (jlong*)cptr)
    {
        int i, length = (*env)->GetArrayLength(env, array);

        for(i = 0; i < length; i++)
            jptr[i] = (jlong)cptr[i];

        free(cptr);
    }

    (*env)->ReleaseLongArrayElements(env, array, jptr, 0);
}

jboolean ompi_java_exceptionCheck(JNIEnv *env, int rc)
{
    if (rc < 0) {
        /* handle ompi error code */
        rc = ompi_errcode_get_mpi_code (rc);
        /* ompi_mpi_errcode_get_class CAN NOT handle negative error codes.
         * all Open MPI MPI error codes should be > 0. */
        assert (rc >= 0);
    }

    if(MPI_SUCCESS == rc)
    {
        return JNI_FALSE;
    }
    else if((*env)->ExceptionCheck(env))
    {
        return JNI_TRUE;
    }
    else
    {
        int     errClass = ompi_mpi_errcode_get_class(rc);
        char    *message = ompi_mpi_errnum_get_string(rc);
        jstring jmessage = (*env)->NewStringUTF(env, (const char*)message);

        jobject mpiex = (*env)->NewObject(env, ompi_java.ExceptionClass,
                                          ompi_java.ExceptionInit,
                                          rc, errClass, jmessage);
        (*env)->Throw(env, mpiex);
        (*env)->DeleteLocalRef(env, mpiex);
        (*env)->DeleteLocalRef(env, jmessage);
        return JNI_TRUE;
    }
}

void* ompi_java_attrSet(JNIEnv *env, jbyteArray jval)
{
    int length = (*env)->GetArrayLength(env, jval);
    void *cval = malloc(sizeof(int) + length);
    *((int*)cval) = length;

    (*env)->GetByteArrayRegion(env, jval,
            0, length, (jbyte*)cval + sizeof(int));

    return cval;
}

jbyteArray ompi_java_attrGet(JNIEnv *env, void *cval)
{
    int length = *((int*)cval);
    jbyteArray jval = (*env)->NewByteArray(env, length);

    (*env)->SetByteArrayRegion(env, jval,
            0, length, (jbyte*)cval + sizeof(int));

    return jval;
}

int ompi_java_attrCopy(void *attrValIn, void *attrValOut, int *flag)
{
    int length = *((int*)attrValIn) + sizeof(int);
    *((void**)attrValOut) = malloc(length);
    memcpy(*((void**)attrValOut), attrValIn, length);
    *flag = 1;
    return MPI_SUCCESS;
}

int ompi_java_attrDelete(void *attrVal)
{
    free(attrVal);
    return MPI_SUCCESS;
}
