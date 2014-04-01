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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <dlfcn.h>

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_var.h"

#include "mpi.h"
#include "ompi/errhandler/errcode.h"
#include "mpi_MPI.h"
#include "mpiJava.h"

int ompi_mpi_java_eager = 65536;
ompi_java_globals_t ompi_java;

static int len = 0;
static char **sargs = 0;
static void *libmpi = NULL;

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

    if(libmpi == NULL)
    {
        fprintf(stderr, "Java bindings failed to load liboshmem.\n");
        exit(1);
    }

    mca_base_var_register("ompi", "mpi", "java", "eager",
                          "Java buffers eager size",
                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                          OPAL_INFO_LVL_5,
                          MCA_BASE_VAR_SCOPE_READONLY,
                          &ompi_mpi_java_eager);

    return JNI_VERSION_1_6;
}

void JNI_OnUnload(JavaVM *vm, void *reserved)
{
    if(libmpi != NULL)
        dlclose(libmpi);
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

JNIEXPORT jobjectArray JNICALL Java_mpi_MPI_Init_1jni(
        JNIEnv *env, jclass clazz, jobjectArray argv)
{
    jsize i;
    jclass string;
    jobject value;

    len = (*env)->GetArrayLength(env,argv);
    sargs = (char**)calloc(len+1, sizeof(char*));

    for(i = 0; i < len; i++)
    {
        jstring jc = (jstring)(*env)->GetObjectArrayElement(env, argv, i);
        const char *s = (*env)->GetStringUTFChars(env, jc, 0);
        sargs[i] = (char*)calloc(strlen(s) + 1, sizeof(char));
        strcpy(sargs[i], s);
        (*env)->DeleteLocalRef(env, jc);
    }

    int rc = MPI_Init(&len, &sargs);
    ompi_java_exceptionCheck(env, rc);

    string = (*env)->FindClass(env, "java/lang/String");
    value = (*env)->NewObjectArray(env, len, string, NULL);

    for(i = 0; i < len; i++)
    {
        jstring jc = (*env)->NewStringUTF(env, sargs[i]);
        (*env)->SetObjectArrayElement(env, value, i, jc);
        (*env)->DeleteLocalRef(env, jc);
    }

    findClasses(env);
    return value;
}

JNIEXPORT jint JNICALL Java_mpi_MPI_InitThread_1jni(
        JNIEnv *env, jclass clazz, jobjectArray argv, jint required)
{
    jsize i;
    len = (*env)->GetArrayLength(env,argv);
    sargs = (char**)calloc(len+1, sizeof(char*));

    for(i = 0; i < len; i++)
    {
        jstring jc = (jstring)(*env)->GetObjectArrayElement(env, argv, i);
        const char *s = (*env)->GetStringUTFChars(env, jc, 0);
        sargs[i] = (char*)calloc(strlen(s) + 1, sizeof(char));
        strcpy(sargs[i], s);
        (*env)->DeleteLocalRef(env, jc);
    }

    int provided;
    int rc = MPI_Init_thread(&len, &sargs, required, &provided);
    ompi_java_exceptionCheck(env, rc);

    findClasses(env);
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
    int rc = MPI_Finalize();
    ompi_java_exceptionCheck(env, rc);
    deleteClasses(env);
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

static void* getArrayPtr(void** bufBase, JNIEnv *env,
                         jobject buf, int baseType, int offset)
{
    switch(baseType)
    {
        case 0:   /* NULL */
            *bufBase = NULL;
            return NULL;

        case 1: {
            jbyte* els = (*env)->GetByteArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 2: {
            jchar* els = (*env)->GetCharArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 3: {
            jshort* els = (*env)->GetShortArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 4: {
            jboolean* els = (*env)->GetBooleanArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 5: {
            jint* els = (*env)->GetIntArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 6: {
            jlong* els = (*env)->GetLongArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 7: {
            jfloat* els = (*env)->GetFloatArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 8: {
            jdouble* els = (*env)->GetDoubleArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        case 9: {
            jbyte* els = (*env)->GetByteArrayElements(env, buf, NULL);
            *bufBase = els;
            return els + offset;
        }
        default:
            *bufBase = NULL;
            return NULL;  /* 'UNDEFINED' */
    }
}

static void releaseArrayPtr(JNIEnv *e, jobject buf, void *bufBase,
                            int baseType, jint mode)
{
    switch(baseType)
    {
        case 0:
            break;
        case 1:
            (*e)->ReleaseByteArrayElements(e, buf, (jbyte*)bufBase, mode);
            break;
        case 2:
            (*e)->ReleaseCharArrayElements(e, buf, (jchar*)bufBase, mode);
            break;
        case 3:
            (*e)->ReleaseShortArrayElements(e, buf, (jshort*)bufBase, mode);
            break;
        case 4:
            (*e)->ReleaseBooleanArrayElements(e, buf, (jboolean*)bufBase, mode);
            break;
        case 5:
            (*e)->ReleaseIntArrayElements(e, buf, (jint*)bufBase, mode);
            break;
        case 6:
            (*e)->ReleaseLongArrayElements(e, buf, (jlong*)bufBase, mode);
            break;
        case 7:
            (*e)->ReleaseFloatArrayElements(e, buf, (jfloat*)bufBase, mode);
            break;
        case 8:
            (*e)->ReleaseDoubleArrayElements(e, buf, (jdouble*)bufBase, mode);
            break;
        case 9:
            (*e)->ReleaseByteArrayElements(e, buf, (jbyte*)bufBase, mode);
            break;
        default:
            break;
    }
}

void* ompi_java_getBufPtr(void** bufBase, JNIEnv *env, jobject buf,
                          jboolean db, int baseType, int offset)
{
    if(buf == NULL)
    {
        /* Allow NULL buffers to send/recv 0 items as control messages. */
        *bufBase = NULL;
        return NULL;
    }
    else if(db)
    {
        *bufBase = (*env)->GetDirectBufferAddress(env, buf);
        assert(offset == 0);
        return *bufBase;
    }
    else
    {
        return getArrayPtr(bufBase, env, buf, baseType, offset);
    }
}

void ompi_java_releaseBufPtr(JNIEnv *env, jobject buf, jboolean db,
                             void* bufBase, int baseType)
{
    if(!db && buf)
        releaseArrayPtr(env, buf, bufBase, baseType, 0);
}

void ompi_java_releaseReadBufPtr(JNIEnv *env, jobject buf, jboolean db,
                                 void *bufBase, int baseType)
{
    if(!db && buf)
        releaseArrayPtr(env, buf, bufBase, baseType, JNI_ABORT);
}

void* ompi_java_getDirectBufferAddress(JNIEnv *env, jobject buf)
{
    /* Allow NULL buffers to send/recv 0 items as control messages. */
    return buf == NULL ? NULL : (*env)->GetDirectBufferAddress(env, buf);
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
