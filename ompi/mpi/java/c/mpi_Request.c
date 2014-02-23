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

JNIEXPORT void JNICALL Java_mpi_Request_init(JNIEnv *env, jclass c)
{
    ompi_java.ReqHandle = (*env)->GetFieldID(env, c, "handle", "J");
    ompi_java.ReqStatus = (*env)->GetFieldID(env, c, "status", "Lmpi/Status;");
}

static void setIndices(JNIEnv *env, jintArray indices, int *cIdx, int count)
{
    jint *jIdx;

    if(sizeof(int) == sizeof(jint))
    {
        jIdx = cIdx;
    }
    else
    {
        jIdx = (jint*)calloc(count, sizeof(jint));
        int i;

        for(i = 0; i < count; i++)
            jIdx[i] = cIdx[i];
    }

    (*env)->SetIntArrayRegion(env, indices, 0, count, jIdx);
    
    if(jIdx != cIdx)
        free(jIdx);
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

JNIEXPORT jboolean JNICALL Java_mpi_Request_testStatus(
        JNIEnv *env, jobject jthis, jlong handle, jobject jStatus)
{
    MPI_Request req = (MPI_Request)handle;
    int flag;
    MPI_Status status;
    int rc = MPI_Test(&req, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);

    if(!flag)
        return JNI_FALSE;

    ompi_java_status_set(&status, env, jStatus);
    return JNI_TRUE;
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

JNIEXPORT jboolean JNICALL Java_mpi_Request_testAnyStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobject jStatus)
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
        return JNI_FALSE;

    ompi_java_status_set(&status, env, jStatus);
    (*env)->SetIntField(env, jStatus, ompi_java.StIndex, index);
    return JNI_TRUE;
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
        jobject st = (*env)->GetObjectArrayElement(env, statuses, i);
        ompi_java_status_set(stas + i, env, st);
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

JNIEXPORT int JNICALL Java_mpi_Request_testAllStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobjectArray objReqs)
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

    if(flag)
    {
        int i;
        for(i = 0; i < count; i++)
        {
            /* Copy final native status to Java statuses. */
            jobject rq = (*env)->GetObjectArrayElement(env, objReqs, i);
            jobject st = (*env)->GetObjectField(env, rq, ompi_java.ReqStatus);
            ompi_java_status_set(stas + i, env, st);
            (*env)->DeleteLocalRef(env, st);
            (*env)->DeleteLocalRef(env, rq);
        }
    }

    free(stas);
    return flag ? count : -1;
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

JNIEXPORT jint JNICALL Java_mpi_Request_waitSomeStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobjectArray objReqs)
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

    if(outcount != MPI_UNDEFINED)
    {
        int i;
        for(i = 0; i < outcount; i++)
        {
            /* Copy final native status to Java statuses. */
            jobject rq = (*env)->GetObjectArrayElement(env, objReqs, i);
            jobject st = (*env)->GetObjectField(env, rq, ompi_java.ReqStatus);
            ompi_java_status_set(stas + i, env, st);
            (*env)->SetIntField(env, st, ompi_java.StIndex, indices[i]);
            (*env)->DeleteLocalRef(env, st);
            (*env)->DeleteLocalRef(env, rq);
        }
    }

    free(stas);
    free(indices);
    return outcount;
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
        setIndices(env, jindices, indices, outcount);
    }

    free(indices);
    return jindices;
}

JNIEXPORT int JNICALL Java_mpi_Request_testSomeStatus(
        JNIEnv *env, jclass clazz, jlongArray requests, jobjectArray objReqs)
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

    if(outcount != MPI_UNDEFINED)
    {
        int i;
        for(i = 0; i < outcount; i++)
        {
            /* Copy final native status to Java statuses. */
            jobject rq = (*env)->GetObjectArrayElement(env, objReqs, i);
            jobject st = (*env)->GetObjectField(env, rq, ompi_java.ReqStatus);
            ompi_java_status_set(stas + i, env, st);
            (*env)->SetIntField(env, st, ompi_java.StIndex, indices[i]);
            (*env)->DeleteLocalRef(env, st);
            (*env)->DeleteLocalRef(env, rq);
        }
    }

    free(stas);
    free(indices);
    return outcount;
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
        setIndices(env, jindices, indices, outcount);
    }

    free(indices);
    return jindices;
}
