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

JNIEXPORT void JNICALL Java_mpi_Request_cancel_1jni(JNIEnv *env, jobject jthis)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);

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

JNIEXPORT jboolean JNICALL Java_mpi_Request_isNull(JNIEnv *env, jobject jthis)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);

    /* Handle is NULL when we create a Prequest object. */
    return req == NULL || req == MPI_REQUEST_NULL ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_Request_waitStatus(
                       JNIEnv *env, jobject jthis, jobject stat)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);

    MPI_Status status;
    int rc = MPI_Wait(&req, &status);
    ompi_java_exceptionCheck(env, rc);

    ompi_java_status_set(&status, env, stat);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);
}

JNIEXPORT void JNICALL Java_mpi_Request_waitNoStatus(JNIEnv *env, jobject jthis)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);

    MPI_Status status;
    int rc = MPI_Wait(&req, &status);
    ompi_java_exceptionCheck(env, rc);

    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);
}

JNIEXPORT jobject JNICALL Java_mpi_Request_testStatus_1jni(
                          JNIEnv *env, jobject jthis)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);
    int flag;
    MPI_Status status;
    int rc = MPI_Test(&req, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);

    if(!flag)
        return NULL;

    return ompi_java_status_new(&status, env);
}

JNIEXPORT jboolean JNICALL Java_mpi_Request_testNoStatus(
                           JNIEnv *env, jobject jthis)
{
    MPI_Request req = (MPI_Request)(*env)->GetLongField(
                      env, jthis, ompi_java.ReqHandle);
    int flag;
    MPI_Status status;
    int rc = MPI_Test(&req, &flag, &status);
    ompi_java_exceptionCheck(env, rc);
    (*env)->SetLongField(env, jthis, ompi_java.ReqHandle, (jlong)req);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAnyStatus(
        JNIEnv *env, jclass clazz, jobjectArray requests, jobject stat)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(count, sizeof(MPI_Request));

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int index;
    MPI_Status status;
    int rc = MPI_Waitany(count, reqs, &index, &status);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env,requests,i);
        (*env)->SetLongField(env, req, ompi_java.ReqHandle, (jlong)reqs[i]);
        (*env)->DeleteLocalRef(env, req);
    }

    ompi_java_status_set(&status, env, stat);
    (*env)->SetIntField(env, stat, ompi_java.StIndex, index);
    free(reqs);
}

JNIEXPORT jint JNICALL Java_mpi_Request_waitAnyNoStatus(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int count = (*env)->GetArrayLength(env,requests);
    MPI_Request *reqs = (MPI_Request*)calloc(count, sizeof(MPI_Request));

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int index;
    MPI_Status status;
    int rc = MPI_Waitany(count, reqs, &index, &status);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);
        (*env)->SetLongField(env, req, ompi_java.ReqHandle, (jlong)reqs[i]);
        (*env)->DeleteLocalRef(env, req);
    }

    free(reqs);
    return index;
}

JNIEXPORT jobject JNICALL Java_mpi_Request_testAnyStatus_1jni(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(count, sizeof(MPI_Request));

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int index, flag;
    MPI_Status status;
    int rc = MPI_Testany(count, reqs, &index, &flag, &status);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);
        (*env)->SetLongField(env, req, ompi_java.ReqHandle, (jlong)reqs[i]);
        (*env)->DeleteLocalRef(env, req);
    }

    free(reqs);

    if(!flag)
        return NULL;

    jobject st = ompi_java_status_new(&status, env);
    (*env)->SetIntField(env, st, ompi_java.StIndex, index);
    return st;
}

JNIEXPORT jint JNICALL Java_mpi_Request_testAnyNoStatus(
                       JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(count, sizeof(MPI_Request));

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int index, flag;
    MPI_Status status;
    int rc = MPI_Testany(count, reqs, &index, &flag, &status);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);
        (*env)->SetLongField(env, req, ompi_java.ReqHandle, (jlong)reqs[i]);
        (*env)->DeleteLocalRef(env, req);
    }

    free(reqs);
    return index;
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAllStatus(
        JNIEnv *env, jclass clazz, jobjectArray requests, jobjectArray statuses)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(2 * count, sizeof(MPI_Request));
    MPI_Request *reqsIni = reqs + count;
    MPI_Status  *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));

    for(i = 0; i < count; i++)
    {
        jobject r = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = reqsIni[i] = (MPI_Request)(*env)->GetLongField(
                               env, r, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, r);
    }

    int rc = MPI_Waitall(count, reqs, stas);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        if(reqsIni[i] != MPI_REQUEST_NULL)
        {
            /* Copy final native request to array of request */
            jobject r = (*env)->GetObjectArrayElement(env, requests, i);
            (*env)->SetLongField(env, r, ompi_java.ReqHandle, (jlong)reqs[i]);

            /* Copy final native status to Java array_of status */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetObjectArrayElement(env, statuses, i, st);

            /* Try not to create too many local references */
            (*env)->DeleteLocalRef(env, r);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(reqs);
    free(stas);
}

JNIEXPORT void JNICALL Java_mpi_Request_waitAllNoStatus(
        JNIEnv *env, jclass jthis, jobjectArray requests)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(2 * count, sizeof(MPI_Request));
    MPI_Request *reqsIni = reqs + count;
    MPI_Status  *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));

    for(i = 0; i < count; i++)
    {
        jobject r = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = reqsIni[i] = (MPI_Request)(*env)->GetLongField(
                               env, r, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, r);
    }

    int rc = MPI_Waitall(count, reqs, stas);
    ompi_java_exceptionCheck(env, rc);

    for(i = 0; i < count; i++)
    {
        if(reqsIni[i] != MPI_REQUEST_NULL)
        {
            /* Copy final native request to array of request */
            jobject r = (*env)->GetObjectArrayElement(env,requests,i);
            (*env)->SetLongField(env, r, ompi_java.ReqHandle, (jlong)reqs[i]);
            /* Try not to create too many local references */
            (*env)->DeleteLocalRef(env, r);
        }
    }

    free(reqs);
    free(stas);
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_testAllStatus_1jni(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(2 * count, sizeof(MPI_Request));
    MPI_Request *reqsIni = reqs + count;
    MPI_Status  *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));

    /* Copy initial native requests in Java array of request to reqs. */

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = reqsIni[i] = (MPI_Request)(*env)->GetLongField(
                               env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int flag;
    int rc = MPI_Testall(count, reqs, &flag, stas);
    ompi_java_exceptionCheck(env, rc);
    jobjectArray statuses = NULL;

    if(flag)
    {
        statuses = (*env)->NewObjectArray(
                   env, count, ompi_java.StatusClass, NULL);

        for(i = 0; i < count; i++)
        {
            if(reqsIni[i] != MPI_REQUEST_NULL)
            {
                jobject req = (*env)->GetObjectArrayElement(env, requests, i);

                /* Copy final native request to array of request. */
                (*env)->SetLongField(env, req,
                        ompi_java.ReqHandle, (jlong)reqs[i]);

                /* Copy final native status to Java statuses. */
                jobject st = ompi_java_status_new(stas + i, env);
                (*env)->SetObjectArrayElement(env, statuses, i, st);

                /* Try not to create too many local references. */
                (*env)->DeleteLocalRef(env, req);
                (*env)->DeleteLocalRef(env, st);
            }
        }
    }

    free(reqs);
    free(stas);
    return statuses;
}

JNIEXPORT jboolean JNICALL Java_mpi_Request_testAllNoStatus(
        JNIEnv *env, jclass jthis, jobjectArray requests)
{
    int i, flag;
    int count = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(2 * count, sizeof(MPI_Request));
    MPI_Request *reqsIni = reqs + count;
    MPI_Status  *stas = (MPI_Status*)calloc(count, sizeof(MPI_Status));

    for(i = 0; i < count; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = reqsIni[i] = (MPI_Request)(*env)->GetLongField(
                               env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int rc = MPI_Testall(count, reqs, &flag, stas);
    ompi_java_exceptionCheck(env, rc);

    if(flag)
    {
        for(i = 0; i < count; i++)
        {
            if(reqsIni[i] != MPI_REQUEST_NULL)
            {
                /* Copy final native request to array of request */
                jobject req  = (*env)->GetObjectArrayElement(env, requests, i);

                (*env)->SetLongField(env, req,
                        ompi_java.ReqHandle, (jlong)reqs[i]);

                /* Try not to create too many local references */
                (*env)->DeleteLocalRef(env, req);
            }
        }
    }

    free(reqs);
    free(stas);
    return flag ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_waitSomeStatus_1jni(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int incount = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(incount, sizeof(MPI_Request));
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));

    /* Copy initial native requests in Java array of request to reqs. */

    for(i = 0; i < incount; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int outcount;
    int rc = MPI_Waitsome(incount, reqs, &outcount, indices, stas);
    ompi_java_exceptionCheck(env, rc);
    jobjectArray statuses = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        statuses = (*env)->NewObjectArray(
                   env, outcount, ompi_java.StatusClass, NULL);

        for(i = 0; i < outcount; i++)
        {
            int index = indices[i];
            jobject req = (*env)->GetObjectArrayElement(env, requests, index);

            /* Copy final native request to 'requests'. */
            (*env)->SetLongField(env, req,
                    ompi_java.ReqHandle, (jlong)reqs[index]);

            /* Copy final native status to Java 'statuses'... */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetIntField(env, st, ompi_java.StIndex, index);
            (*env)->SetObjectArrayElement(env, statuses, i, st);

            /* Try not to create too many local references... */
            (*env)->DeleteLocalRef(env, req);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(reqs);
    free(stas);
    free(indices);
    return statuses;
}

JNIEXPORT jintArray JNICALL Java_mpi_Request_waitSomeNoStatus(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int incount = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(incount, sizeof(MPI_Request));
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));

    /* Copy initial native requests in Java array of request to reqs. */

    for(i = 0; i < incount; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int outcount;
    int rc = MPI_Waitsome(incount, reqs, &outcount, indices, stas);
    ompi_java_exceptionCheck(env, rc);
    jintArray jindices = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        for(i = 0; i < outcount; i++)
        {
            int index = indices[i];
            jobject req  = (*env)->GetObjectArrayElement(env, requests, index);

            /* Copy final native request to 'requests'. */
            (*env)->SetLongField(env, req,
                    ompi_java.ReqHandle, (jlong)reqs[index]);

            /* Try not to create too many local references... */
            (*env)->DeleteLocalRef(env, req);
        }

        jindices = (*env)->NewIntArray(env, outcount);
        (*env)->SetIntArrayRegion(env, jindices, 0, outcount, indices);
    }

    free(reqs);
    free(stas);
    free(indices);
    return jindices;
}

JNIEXPORT jobjectArray JNICALL Java_mpi_Request_testSomeStatus_1jni(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int incount = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs = (MPI_Request*)calloc(incount, sizeof(MPI_Request));
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));

    for(i = 0; i < incount; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int outcount;
    int rc = MPI_Testsome(incount,reqs,&outcount,indices, stas);
    ompi_java_exceptionCheck(env, rc);
    jobjectArray statuses = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        statuses = (*env)->NewObjectArray(
                   env, outcount, ompi_java.StatusClass, NULL);

        for(i = 0; i < outcount; i++)
        {
            int index = indices[i];
            jobject req = (*env)->GetObjectArrayElement(env, requests, index);

            /* Copy final native request to 'requests'. */
            (*env)->SetLongField(env, req,
                    ompi_java.ReqHandle, (jlong)reqs[index]);

            /* Copy final native status to Java 'statuses'... */
            jobject st = ompi_java_status_new(stas + i, env);
            (*env)->SetIntField(env, st, ompi_java.StIndex, index);
            (*env)->SetObjectArrayElement(env, statuses, i, st);

            /* Try not to create too many local references... */
            (*env)->DeleteLocalRef(env, req);
            (*env)->DeleteLocalRef(env, st);
        }
    }

    free(reqs);
    free(stas);
    free(indices);
    return statuses;
}

JNIEXPORT jintArray JNICALL Java_mpi_Request_testSomeNoStatus(
        JNIEnv *env, jclass clazz, jobjectArray requests)
{
    int i;
    int incount = (*env)->GetArrayLength(env, requests);
    MPI_Request *reqs=(MPI_Request*)calloc(incount, sizeof(MPI_Request));
    MPI_Status *stas = (MPI_Status*)calloc(incount, sizeof(MPI_Status));
    int *indices = (int*)calloc(incount, sizeof(int));

    for(i = 0; i < incount; i++)
    {
        jobject req = (*env)->GetObjectArrayElement(env, requests, i);

        reqs[i] = (MPI_Request)(*env)->GetLongField(
                  env, req, ompi_java.ReqHandle);

        (*env)->DeleteLocalRef(env, req);
    }

    int outcount;
    int rc = MPI_Testsome(incount, reqs, &outcount, indices, stas);
    ompi_java_exceptionCheck(env, rc);
    jintArray jindices = NULL;

    if(outcount != MPI_UNDEFINED)
    {
        for(i = 0; i < outcount; i++)
        {
            int index = indices[i];
            jobject req = (*env)->GetObjectArrayElement(env, requests, index);

            /* Copy final native request to 'requests'. */
            (*env)->SetLongField(env, req,
                    ompi_java.ReqHandle, (jlong)reqs[index]);

            /* Try not to create too many local references... */
            (*env)->DeleteLocalRef(env, req);
        }

        jindices = (*env)->NewIntArray(env, outcount);
        (*env)->SetIntArrayRegion(env, jindices, 0, outcount, indices);
    }

    free(reqs);
    free(stas);
    free(indices);
    return jindices;
}
