/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow.
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

#include "mpi.h"
#include "ompi/errhandler/errcode.h"
#include "mpi_MPI.h"
#include "mpiJava.h"

ompi_java_globals_t ompi_java;

static int len = 0;
static char** sargs = 0;
static void *mpilibhandle=NULL;

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
JNIEXPORT jboolean JNICALL Java_mpi_MPI_loadGlobalLibraries(JNIEnv *env, jclass obj)
{
    if (NULL == (mpilibhandle = dlopen("libmpi." OPAL_DYN_LIB_SUFFIX,
                                       RTLD_NOW | RTLD_GLOBAL))) {
        return JNI_FALSE;
    }
    return JNI_TRUE;
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

/*
 * Class:     mpi_MPI
 * Method:    Init_jni
 * Signature: ([Ljava/lang/String;)[Ljava/lang/String;
 */
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

    ompi_java_init_native_Datatype(env);
    ompi_java_findClasses(env);
    return value;
}

/*
 * Class:     mpi_MPI
 * Method:    InitThread_jni
 * Signature: ([Ljava/lang/String;I)I
 */
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

    ompi_java_init_native_Datatype(env);
    ompi_java_findClasses(env);
    return provided;
}

/*
 * Class:     mpi_MPI
 * Method:    queryThread_jni
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_mpi_MPI_queryThread_1jni(JNIEnv *env, jclass clazz)
{
    int provided;
    int rc = MPI_Query_thread(&provided);
    ompi_java_exceptionCheck(env, rc);
    return provided;
}

/*
 * Class:     mpi_MPI
 * Method:    isThreadMain_jni
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_MPI_isThreadMain_1jni(
                           JNIEnv *env, jclass clazz)
{
    int flag;
    int rc = MPI_Is_thread_main(&flag);
    ompi_java_exceptionCheck(env, rc);
    return flag ? JNI_TRUE : JNI_FALSE;
}

/*
 * Class:     mpi_MPI
 * Method:    Finalize_jni
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_Finalize_1jni(JNIEnv *env, jclass obj)
{
    if (NULL != mpilibhandle) {
        dlclose(mpilibhandle);
    }

    int rc = MPI_Finalize();
    ompi_java_exceptionCheck(env, rc);
    ompi_java_deleteClasses(env);
}

/*
 * Class:     mpi_MPI
 * Method:    getProcessorName
 * Signature: ([B)I
 */
JNIEXPORT jint JNICALL Java_mpi_MPI_getProcessorName(
                       JNIEnv *env, jclass obj, jbyteArray buf)
{
    int len, rc;
    jboolean isCopy;
    jbyte* bufc = (jbyte*)((*env)->GetByteArrayElements(env,buf,&isCopy)) ;

    rc = MPI_Get_processor_name((char*)bufc, &len);
    ompi_java_exceptionCheck(env, rc);
    (*env)->ReleaseByteArrayElements(env,buf,bufc,0) ;
    return len;
}

/*
 * Class:     mpi_MPI
 * Method:    wtime_jni
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_mpi_MPI_wtime_1jni(JNIEnv *env, jclass jthis)
{
    return MPI_Wtime();
}

/*
 * Class:     mpi_MPI
 * Method:    wtick_jni
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_mpi_MPI_wtick_1jni(JNIEnv *env, jclass jthis)
{
    return MPI_Wtick();
}

/*
 * Class:     mpi_MPI
 * Method:    isInitialized
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_MPI_isInitialized(JNIEnv *env, jclass jthis)
{
    int flag, rc;

    rc = MPI_Initialized(&flag);
    ompi_java_exceptionCheck(env, rc);

    if (flag==0) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

/*
 * Class:     mpi_MPI
 * Method:    isFinalized
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_mpi_MPI_isFinalized(JNIEnv *env, jclass jthis)
{
    int flag, rc;

    rc = MPI_Finalized(&flag);
    ompi_java_exceptionCheck(env, rc);

    if (flag==0) {
        return JNI_FALSE;
    } else {
        return JNI_TRUE;
    }
}

/*
 * Class:     mpi_MPI
 * Method:    attachBuffer_jni
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_attachBuffer_1jni(
                       JNIEnv *env, jclass jthis, jbyteArray buf)
{
    jboolean isCopy;

    int size=(*env)->GetArrayLength(env,buf);
    jbyte* bufptr = (*env)->GetByteArrayElements(env,buf,&isCopy) ;

    int rc = MPI_Buffer_attach(bufptr,size);
    ompi_java_exceptionCheck(env, rc);
}

/*
 * Class:     mpi_MPI
 * Method:    detachBuffer_jni
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL Java_mpi_MPI_detachBuffer_1jni(
                       JNIEnv *env, jclass jthis, jbyteArray buf)
{
    /*jboolean isCopy;*/

    int size, rc;
    /*char* bufptr ;*/
    jbyte* bufptr ;

    rc = MPI_Buffer_detach(&bufptr, &size);
    ompi_java_exceptionCheck(env, rc);

    if (buf != NULL) {
        (*env)->ReleaseByteArrayElements(env,buf,bufptr,0);
    }
}

void ompi_java_findClasses(JNIEnv *env)
{
    ompi_java.CartParmsClass  = ompi_java_findClass(env, "mpi/CartParms");
    ompi_java.ShiftParmsClass = ompi_java_findClass(env, "mpi/ShiftParms");
    ompi_java.GraphParmsClass = ompi_java_findClass(env, "mpi/GraphParms");

    ompi_java.DistGraphNeighborsClass = ompi_java_findClass(
                                        env, "mpi/DistGraphNeighbors");

    ompi_java.ExceptionClass  = ompi_java_findClass(env, "mpi/MPIException");

    ompi_java.ExceptionInit = (*env)->GetMethodID(
                              env, ompi_java.ExceptionClass,
                              "<init>", "(IILjava/lang/String;)V");

    ompi_java.IntegerClass = ompi_java_findClass(env, "java/lang/Integer");
    ompi_java.LongClass    = ompi_java_findClass(env, "java/lang/Long");

    ompi_java.IntegerValueOf = (*env)->GetStaticMethodID(
            env, ompi_java.IntegerClass, "valueOf", "(I)Ljava/lang/Integer;");
    ompi_java.LongValueOf = (*env)->GetStaticMethodID(
            env, ompi_java.LongClass, "valueOf", "(J)Ljava/lang/Long;");
}

jclass ompi_java_findClass(JNIEnv *env, const char *className)
{
    jclass c = (*env)->FindClass(env, className),
           r = (*env)->NewGlobalRef(env, c);

    (*env)->DeleteLocalRef(env, c);
    return r;
}

void ompi_java_deleteClasses(JNIEnv *env)
{
    (*env)->DeleteGlobalRef(env, ompi_java.CartParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.ShiftParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.GraphParmsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.DistGraphNeighborsClass);
    (*env)->DeleteGlobalRef(env, ompi_java.ExceptionClass);
    (*env)->DeleteGlobalRef(env, ompi_java.IntegerClass);
    (*env)->DeleteGlobalRef(env, ompi_java.LongClass);
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
