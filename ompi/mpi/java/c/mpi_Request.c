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
 * File         : mpi_Request.c
 * Headerfile   : mpi_Request.h
 * Author       : Sung-Hoon Ko, Xinying Li, Bryan Carpenter
 * Created      : Thu Apr  9 12:22:15 1998
 * Revision     : $Revision: 1.11 $
 * Updated      : $Date: 2003/01/16 16:39:34 $
 * Copyright: Northeast Parallel Architectures Center
 *            at Syracuse University 1998
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <assert.h>
#ifdef HAVE_TARGETCONDITIONALS_H
#include <TargetConditionals.h>
#endif

#include "mpi.h"
#include "mpi_Request.h"
#include "mpiJava.h"

JNIEXPORT void JNICALL Java_mpi_Request_init(JNIEnv *env, jclass clazz)
{
    ompi_java.ReqHandle = (*env)->GetFieldID(env, clazz, "handle", "J");
}

JNIEXPORT jlong JNICALL Java_mpi_Request_getNull(JNIEnv *env, jclass clazz)
{
    return (jlong)MPI_REQUEST_NULL;
}

JNIEXPORT void JNICALL Java_mpi_Request_cancel(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Request req = (MPI_Request)handle;
    int rc = MPI_Cancel(&req);
    ompi_java_exceptionCheck(env, rc);
}

JNIEXPORT jlong JNICALL Java_mpi_Request_free(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Request req = (MPI_Request)handle;

    if(req != MPI_REQUEST_NULL)
    {
        if(req == NULL)
        {
            req = MPI_REQUEST_NULL;
        }
        else
        {
            /* Handle is NULL when we create a Prequest object. */
            int rc = MPI_Request_free(&req);
            ompi_java_exceptionCheck(env, rc);
        }
    }

    return (jlong)req;
}

JNIEXPORT jlong JNICALL Java_mpi_Request_waitStatus(
        JNIEnv *env, jobject jthis, jlong handle, jobject stat)
{
    MPI_Request req = (MPI_Request)handle;
    MPI_Status status;
    int rc = MPI_Wait(&req, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_status_set(&status, env, stat);
    return (jlong)req;
}

JNIEXPORT jlong JNICALL Java_mpi_Request_waitFor(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Request req = (MPI_Request)handle;
    int rc = MPI_Wait(&req, MPI_STATUS_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    return (jlong)req;
}

JNIEXPORT jobject JNICALL Java_mpi_Request_testStatus(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Request req = (MPI_Request)handle;
    int flag;
    MPI_Status status;
    int rc = MPI_Test(&req, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);
    return flag ? ompi_java_status_new(&status, env) : NULL;
}

JNIEXPORT jboolean JNICALL Java_mpi_Request_test(
        JNIEnv *env, jobject jthis, jlong handle)
{
    MPI_Request req = (MPI_Request)handle;
    int flag;
    int rc = MPI_Test(&req, &flag, MPI_STATUS_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAnyStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobject stat)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int index;
    MPI_Status status;
    int rc = MPI_Waitany(count, cReq, &index, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    ompi_java_status_set(&status, env, stat);
    (*env)->SetIntField(env, stat, ompi_java.StIndex, index);
}

JNIEXPORT jint JNICALL Java_mpi_Request_waitAny(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int index;
    MPI_Status status;  /* osvegis: MPI_STATUS_IGNORE doesn't work */
    int rc = MPI_Waitany(count, cReq, &index, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    return index;
}

JNIEXPORT jobject JNICALL Java_mpi_Request_testAnyStatus(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int index, flag;
    MPI_Status status;
    int rc = MPI_Testany(count, cReq, &index, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);

    if(!flag)
        return NULL;

    jobject st = ompi_java_status_new(&status, env);
    (*env)->SetIntField(env, st, ompi_java.StIndex, index);
    return st;
}

JNIEXPORT jint JNICALL Java_mpi_Request_testAny(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int index, flag;
    MPI_Status status;  /* osvegis: MPI_STATUS_IGNORE doesn't work */
    int rc = MPI_Testany(count, cReq, &index, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    return index;
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAllStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobjectArray statuses)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    MPI_Status *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));
    int rc = MPI_Waitall(count, cReq, stas);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);

    for(i = 0; i < count; i++)
    {
        /* Copy final native status to Java array_of status */
        jobject st = ompi_java_status_new(stas + i, env);
        (*env)->SetObjectArrayElement(env, statuses, i, st);
        (*env)->DeleteLocalRef(env, st);
    }

    free(stas);
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAll(
        JNIEnv *env, jclass jthis, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int rc = MPI_Waitall(count, cReq, MPI_STATUSES_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_testAllStatus(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    MPI_Status *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));
    int flag;
    int rc = MPI_Testall(count, cReq, &flag, stas);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    jobjectArray statuses = NULL;

    if(flag)
    {
        statuses = (*env)->NewObjectArray(
                   env, count, ompi_java.StatusClass, NULL);
        int i;
        for(i = 0; i < count; i++)
        {
            /* Copy final native status to Java statuses. */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetObjectArrayElement(env, statuses, i, st);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(stas);
    return statuses;
}

JNIEXPORT jboolean JNICALL Java_mpi_Request_testAll(
        JNIEnv *env, jclass jthis, jlongArray requests)
{
    int count = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int flag;
    int rc = MPI_Testall(count, cReq, &flag, MPI_STATUSES_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_waitSomeStatus(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int incount = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));

    int outcount;
    int rc = MPI_Waitsome(incount, cReq, &outcount, indices, stas);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    jobjectArray statuses = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        statuses = (*env)->NewObjectArray(
                   env, outcount, ompi_java.StatusClass, NULL);
        int i;
        for(i = 0; i < outcount; i++)
        {
            /* Copy final native status to Java 'statuses'... */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetIntField(env, st, ompi_java.StIndex, indices[i]);
            (*env)->SetObjectArrayElement(env, statuses, i, st);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(stas);
    free(indices);
    return statuses;
}

JNIEXPORT jintArray JNICALL Java_mpi_Request_waitSome(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int incount = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int *indices = (int*)calloc(incount, sizeof(int));
    int outcount;
    int rc = MPI_Waitsome(incount, cReq, &outcount, indices, MPI_STATUSES_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    jintArray jindices = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        jindices = (*env)->NewIntArray(env, outcount);
        (*env)->SetIntArrayRegion(env, jindices, 0, outcount, indices);
    }

    free(indices);
    return jindices;
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_testSomeStatus(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int incount = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));
    int outcount;
    int rc = MPI_Testsome(incount, cReq, &outcount, indices, stas);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    jobjectArray statuses = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        statuses = (*env)->NewObjectArray(
                   env, outcount, ompi_java.StatusClass, NULL);
        int i;
        for(i = 0; i < outcount; i++)
        {
            /* Copy final native status to Java 'statuses'... */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetIntField(env, st, ompi_java.StIndex, indices[i]);
            (*env)->SetObjectArrayElement(env, statuses, i, st);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(stas);
    free(indices);
    return statuses;
}

JNIEXPORT jintArray JNICALL Java_mpi_Request_testSome(
        JNIEnv *env, jclass clazz, jlongArray requests)
{
    int incount = (*env)->GetArrayLength(env, requests);
    jlong* jReq;
    MPI_Request *cReq;
    ompi_java_getPtrArray(env, requests, &jReq, (void***)&cReq);
    int *indices = (int*)calloc(incount, sizeof(int));
    int outcount;
    int rc = MPI_Testsome(incount, cReq, &outcount, indices, MPI_STATUSES_IGNORE);
    ompi_java_exceptionCheck(env, rc);
    ompi_java_releasePtrArray(env, requests, jReq, (void**)cReq);
    jintArray jindices = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        jindices = (*env)->NewIntArray(env, outcount);
        (*env)->SetIntArrayRegion(env, jindices, 0, outcount, indices);
    }

    free(indices);
    return jindices;
}
